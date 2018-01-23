package info.kwarc.mmt.reflection

import info.kwarc.mmt.api._
import checking._
import objects._
import objects.Conversions._

import info.kwarc.mmt.lf._

/**
 * p |- t: U  --->  |- formation(p,t) : U
 */
object FormationInfer extends InferenceRule(Terms.formation.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] =
     tm match {
       case Terms.formation(p, t) =>
          //TODO check that inferred type does not depend on p
          solver.inferType(t)(stack++Context(p), history)
     }
}

/**
 * p |- t: a  --->  |- refl(p,t) : formation(p,a)
 */
object ReflInfer extends InferenceRule(Terms.refl.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] =
    tm match {
       case Terms.refl(p,t) =>
          solver.inferType(t)(stack ++ Context(p), history) map {tI => Terms.formation(p,tI)}
     }
}

/**
 * |- t: formation(p,a)  --->  |- elim(t,m) : m(a)
 */
object ElimInfer extends InferenceRule(Terms.elim.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack : Stack, history: History) : Option[Term] = {
     tm match {
       case Terms.elim(t,mor) =>
          solver.inferType(t) flatMap {
             case Terms.formation(p,a) =>
                solver.check(IsRealization(stack, mor, OMMOD(p))) // TODO must also check identity condition
                Some(OMM(a, mor))
             case _ => None
          }
     }
   }
}


/**
 * p |- eval(t) : a  --->  |- q: formation(p,a)
 */
object TypingRule extends TypingRule(Terms.formation.path) {
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack : Stack, history: History) : Boolean = {
    tp match {
      case Terms.formation(p,a) =>
         solver.check(Typing(stack++Context(p), Terms.eval(tm), a))
    }
  }
}

/**
 * p |- eval(t1) = eval(t2): a  --->  |- t1 = t2 : formation(p,a)
 */
object Extensionality extends TypeBasedEqualityRule(Nil, Terms.formation.path) {
  def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History) : Option[Boolean] = tp match {
     case Terms.formation(p, a) =>
        Some(solver.check(Equality(stack++Context(p), Terms.eval(tm1), Terms.eval(tm2), Some(a))))
  }
}

/** |- m: p -> .  ---> |- elim(refl(p, t), m) = m(t) */
object Computation extends ComputationRule(Terms.elim.path) {
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack : Stack, history: History) : Option[Term] = {
      tm match {
         case Terms.elim(Terms.refl(p,t),mor) =>
            if (!covered)
               solver.check(IsRealization(stack,mor,OMMOD(p)))
            Some(OMM(t, mor))
         case _ => None
      }
   }
}
