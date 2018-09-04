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

/** infers the type of a beta-redex whose lambda is not well-typed by itself because it quantifies over too large a universe */ 
object PolymorphicApplyTerm extends EliminationRule(Apply.path, OfType.path) {
   override def priority: Int = super.priority + 1

   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
      case ApplySpine(Lambda(x, tpA, bd), arg :: rest) =>
         if (!covered) {
           solver.inferType(tpA)(stack, history + "checking type of bound variable").getOrElse(return None)
           solver.check(Typing(stack, arg, tpA, None))(history + "checking type of argument")
         }
         val bS = solver.substituteSolution(bd ^? (x / arg))
         val ret = ApplyGeneral(bS, rest)
         solver.inferType(ret, covered)          
      case _ =>
        None
   }
}