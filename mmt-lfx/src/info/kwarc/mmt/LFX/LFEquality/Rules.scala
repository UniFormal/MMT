package info.kwarc.mmt.LFX.LFEquality

import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.{ApplySpine, Pi, OfType}
import objects.Conversions._

/** Formation: the type inference rule |-A:U, |-a:A, |-b:A  --->  |- A=B : U  * */
object EqTerm extends FormationRule(eqtype.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case eqtype(tm1,tm2) =>
        val tp1 = solver.inferType(tm1).getOrElse(return None)
        val tp2 = solver.inferType(tm2).getOrElse(return None)
        if (solver.safecheck(Equality(stack,tp1,tp2,None)) contains true) {
          solver.check(Equality(stack,tp1,tp2,None))
          val tp = solver.inferType(tp1).getOrElse(return None)
          Some(tp)
        } else if (solver.safecheck(Subtyping(stack,tp1,tp2)) contains true) {
          solver.check(Subtyping(stack,tp1,tp2))
          val tp = solver.inferType(tp2).getOrElse(return None)
          Some(tp)
        }
        else {
          solver.check(Subtyping(stack,tp2,tp1))
          val tp = solver.inferType(tp1).getOrElse(return None)
          Some(tp)
        }
      case _ => None // should be impossible
    }
  }
}

/** Introduction: the type inference rule |-a:A  --->  |- refl(a) : a=_Aa
  **/
object ReflTerm extends IntroductionRule(refl.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case refl(a) => Some(eqtype(a,a))
      case _ => None
  }
}


/** Elimination: the type inference rule |-a:A ; |- C : {x:A,p:x=a} U ; |- p : C(a,refl_a)
  *  ---> cong(a,C,p) : {x:A,p:x=a}C(x,p)
  **/
object CongTerm extends EliminationRule(cong.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case cong(a,c,p) =>
      val tpp = solver.inferType(c).getOrElse(return None)
      val tpA = solver.inferType(a).getOrElse(return None)
      tpp match {
        case Pi(x,tpA2,Pi(q,tpeq,u)) =>
          solver.check(Equality(stack,tpA,tpA2,None))
          solver.check(Inhabitable(stack,u))
          solver.check(Equality(stack ++ x%tpA,tpeq,eqtype(OMV(x),a),None))
          solver.check(Typing(stack,p,ApplySpine(c,a,refl(a))))
          val (xn,_) = Context.pickFresh(solver.constantContext ++ solver.getPartialSolution ++ stack.context, x)
          val (pn,_) = Context.pickFresh(solver.constantContext ++ solver.getPartialSolution ++ stack.context, q)
          Some(Pi(xn,tpA,Pi(pn,eqtype(OMV(xn),a),ApplySpine(c,OMV(xn),OMV(pn)))))
        case _ => throw TypingRule.NotApplicable
      }
    case _ => None
  }

}