package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import checking._
import objects._
import objects.Conversions._
/**
 * A:U for some universe U ---> c: {x:A} B allowed
 * 
 * This allows, e.g., declaring list: type -> type or id: {a:type} a -> a
 */
/* TODO possible generalizations
 * Higher Pi types in higher-order positions, e.g., h: ({a:type}a->a) -> int.
 * allowed in System F and Haskell
 * 
 * a:K:kind |- A:type ---> {a:K}A : type
 * System F: only K = type
 * Haskell: allows more kinds for K, e.g., type -> type
 * But Haskrell restricts arguments to type variables to not contain Pi. It's unclear if/how this can be generalized to the MMT setting.
 */
object ShallowPolymorphism extends InhabitableRule(Pi.path) with PiOrArrowRule {
   def apply(solver: Solver)(tp: Term)(implicit stack: Stack, history: History) : Boolean = {
      tp match {
         case Pi(x,a,b) =>
            val historyArg = history + (x.toString + " must be typed by universe")
            solver.inferTypeAndThen(a)(stack, historyArg + "infer type") {u =>
               solver.check(Universe(stack, u))(historyArg + "check universe")
            } &&
            solver.check(Inhabitable(stack++x%a, b))
      }
   }
}

object PolymorphicApplyTerm extends EliminationRule(Apply.path, OfType.path) {
   override def priority: Int = super.priority + 1

   val kind = OMS(Typed.kind)
   val tp = OMS(Typed.ktype)

   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
      case ApplySpine(Lambda(x, tpA, bd), arg :: rest) =>
         solver.inferType(tpA)(stack, history) match {
            case Some(_) =>
               solver.check(Typing(stack, arg, tpA, None))
               val bS = solver.substituteSolved(bd ^? (x / arg), covered)
               val ret = if (rest.nonEmpty) ApplySpine(bS, rest: _*) else bS
               return solver.inferType(ret, covered)
            case _ =>
         }
         ApplyTerm.apply(solver)(tm, covered)

      /*
      case ApplySpine(Lambda(x,tpA,bd),arg1 :: rest) =>
         solver.check(Inhabitable(stack,tpA))
         solver.check(Typing(stack,arg1,tpA,None))
         solver.inferType(ApplySpine(bd ^? x/arg1,rest:_*),covered)
         */
      case Apply(_, _) =>
         ApplyTerm.apply(solver)(tm, covered)
      case _ => None
   }
}