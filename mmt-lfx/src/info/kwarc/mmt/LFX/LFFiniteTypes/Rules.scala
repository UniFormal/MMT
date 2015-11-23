package info.kwarc.mmt.LFX.LFFiniteTypes

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.{Pi, Arrow, OfType, Typed}

/**
 * Created by raupi on 19.10.15.
 */
object Common {
  /** convenience function for recursively checking the judgement |- a: type */
  def isType(solver: Solver, a: Term)(implicit stack: Stack, history: History) =
    solver.check(Typing(stack, a, OMS(Typed.ktype), Some(OfType.path)))(history + "type of bound variable must be a type")

  def pickFresh(solver: Solver, x: LocalName)(implicit stack: Stack) =
    Context.pickFresh(solver.constantContext ++ solver.getPartialSolution ++ stack.context, x)
}

/** Formation: the type inference rule |-A:type  --->  |- {}-> A : {} -> A  * */
object EmptyFunTerm extends FormationRule(EmptyFun.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case EmptyFun(tp) =>
        if ( Common.isType(solver,tp)) Some(Arrow(EmptyType.term,tp)) else None
      case _ => None // should be impossible
    }
  }
}


/** Type 0->A has exactly one Element for each A **/
object ZeroTypeRule extends TypeBasedEqualityRule(Nil, Pi.path) {
  def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = tp match {
    case Pi(_,EmptyType.term,_) => Some(true)
    case _ => None
  }

  override def applicable(tp:Term) = tp match {
    case Pi(_,EmptyType.term,_) => true
    case _ => false
  }

}