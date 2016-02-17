package info.kwarc.mmt.LFX.LFCoprod

import info.kwarc.mmt.api.checking.TypingRule.NotApplicable
import info.kwarc.mmt.api.{objects, LocalName}
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects._
import objects.Conversions._
import info.kwarc.mmt.lf._

object Common {
  /** convenience function for recursively checking the judgement |- a: type */
  def isType(solver: Solver, a: Term)(implicit stack: Stack, history: History) =
    solver.check(Typing(stack, a, OMS(Typed.ktype), Some(OfType.path)))(history + "type of bound variable must be a type")

  def pickFresh(solver: Solver, x: LocalName)(implicit stack: Stack) =
    Context.pickFresh(solver.constantContext ++ solver.getPartialSolution ++ stack.context, x)
}

/** Formation: the type inference rule |-A:type, |-B:type  --->  |- A+B : type  * */
object CoprodTerm extends FormationRule(Coprod.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case Coprod(tp1,tp2) =>
        if ( Common.isType(solver,tp1) && Common.isType(solver,tp2)) Some(OMS(Typed.ktype)) else None
      case _ => None // should be impossible
    }
  }
}

/** Introduction: the type inference rule |-t:A  --->  |- inl(t,B) : A+B
  * */
object inlTerm extends IntroductionRule(inl.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case inl(t,tp2) =>
        if (Common.isType(solver,tp2)) {
          val tp1 = solver.inferType(t).getOrElse(return None)
          Some(Coprod(tp1,tp2))
        } else None
      case _ => None
    }
  }
}

object CoprodType extends TypingRule(Coprod.path) {
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) : Boolean = tp match {
    case Coprod(tpA,tpB) =>
      solver.safeSimplifyUntil(tm)(inl.unapply)._1 match {
        case inl(a,tpnB) =>
          val tpnA = solver.inferType(a,false).getOrElse(return false)
          solver.check(Equality(stack,tpA,tpnA,None))
          solver.check(Equality(stack,tpB,tpnB,None))
        case _ => solver.safeSimplifyUntil(tm)(inr.unapply)._1 match {
          case inr(b,tpnA) =>
            val tpnB = solver.inferType(b,false).getOrElse(return false)
            solver.check(Equality(stack,tpA,tpnA,None))
            solver.check(Equality(stack,tpB,tpnB,None))
          case _ => throw TypingRule.NotApplicable
        }
      }
    case _ => throw TypingRule.NotApplicable
  }
}

/** Introduction: the type inference rule |-t:B  --->  |- inr(t,A) : A+B
  * */
object inrTerm extends IntroductionRule(inr.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case inr(t,tp1) =>
        if (Common.isType(solver,tp1)) {
          val tp2 = solver.inferType(t).getOrElse(return None)
          Some(Coprod(tp1,tp2))
        } else None
      case _ => None
    }
  }
}

/** Elimination: the type inference rule x:A|-f:C(inl(x)), x:B|-g:C(inr(x)), |-u:A+B
  * ---> u match case x . f to C(x); case y . g to C(y) : C(u) **/
object MatchTerm extends EliminationRule(cmatch.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case cmatch(u,x,f,g,c) =>
      history += "Inferring type of atob"
      val (xn,sub) = Common.pickFresh(solver,x)
      val atob = solver.safeSimplifyUntil(solver.inferType(u).getOrElse(return None))(Coprod.unapply)._1
      atob match {
        case Coprod(a,b) =>
          history += "Checking types"
          solver.check(Typing(stack ++ xn%a,f ^? sub,c ^? x/inl(OMV(xn),b))) //(stack ++ xn%a,f ^?sub,c^?(x/inl(x,b)),None))
          solver.check(Typing(stack ++ xn%b,g ^? sub,c ^? x/inr(OMV(xn),a)))
          Some(c ^? x/u)
        case _ => None
      }

  }

}


/** Computation: the rules : - inl(a,B) match case x.f; case x.g = f[x/a]
  *                          - inr(b,A) match case x.f; case x.g = g[x/b]
  */

object MatchComp extends ComputationRule(cmatch.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term]
  = tm match {
    case cmatch(u,x,f,g,c) => u match {
      case inl(a,tpB) => if (covered) Some(f ^? x/a) else {
        val atob = solver.inferType(u).getOrElse(return None)
        atob match {
          case Coprod(tpA,tpB2) =>
            val (xn,sub) = Context.pickFresh(stack.context,x)
            solver.check(Equality(stack,tpB,tpB2,None))
            solver.check(Typing(stack ++ xn%tpA,f ^? sub,c ^? x/inl(xn,tpB))) //(stack ++ xn%a,f ^?sub,c^?(x/inl(x,b)),None))
            solver.check(Typing(stack ++ xn%tpB,g ^? sub,c ^? x/inr(xn,tpA)))
            Some(f ^? x/a)
          case _ => None
        }
      }
      case inr(b,tpA) => if (covered) Some(g ^? x/b) else {
        val atob = solver.inferType(u).getOrElse(return None)
        atob match {
          case Coprod(tpA2,tpB) =>
            val (xn,sub) = Context.pickFresh(stack.context,x)
            solver.check(Equality(stack,tpA,tpA2,None))
            solver.check(Typing(stack ++ xn%tpA,f ^? sub,c ^? x/inl(xn,tpB))) //(stack ++ xn%a,f ^?sub,c^?(x/inl(x,b)),None))
            solver.check(Typing(stack ++ xn%tpB,g ^? sub,c ^? x/inr(xn,tpA)))
            Some(g ^? x/b)
          case _ => None
        }
      }
      case _ => throw TypingRule.NotApplicable
    }
    case _ => None
  }
}

/** Computation rules for function addition **/

object AddFuncComp extends ComputationRule(Addfunc.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term]
  = {
    val (x,_) = Context.pickFresh(stack.context,LocalName("x"))
    try { getFunTerm(solver)(tm,covered).map(p => Lambda(x,p._2,p._1(OMV(x)))) } catch {
      case t : Throwable => throw t
    }
  }

  def getFunTerm(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History)
  : Option[(Term => Term,Term)] = tm match {
    case Addfunc(t1,t2) =>
      (solver.simplify(solver.inferType(t1)(stack,history).getOrElse(return None)),
        solver.simplify(solver.inferType(t2)(stack,history).getOrElse(return None))) match {
        case (Pi(y1,tpA,tpC),Pi(y2,tpB,tpC2)) =>
          val (x,_) = Context.pickFresh(stack.context,LocalName("x"))
          val (x1,sub1) = Context.pickFresh((stack++x%Coprod(tpA,tpB)).context,y1)
          solver.check(Equality(stack++x%Coprod(tpA,tpB),tpC ^? y1/inl(x,tpB),tpC2 ^? y2/inr(x,tpA),Some(OMS(Typed.ktype))))
          history+="Expanding function addition"
          val ret : Term => Term = tx => cmatch(tx,x1,Apply(t1,OMV(x1)),Apply(t2,OMV(x1)),tpC^?y1/OMV(x1))
          Some((ret,Coprod(tpA,tpB)))
        case _ => None
      }
    case _ => None
  }
}

/** Type inference rule for function addition */

object CompFuncTerm extends EliminationRule(Addfunc.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case Addfunc(t1,t2) => solver.inferType(AddFuncComp(solver)(tm,false).getOrElse(return None))(stack,history)
    case _ => None
  }
}

object MatchEquality extends TermHeadBasedEqualityRule(Nil,cmatch.path,cmatch.path) {
  def apply(check: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History): Option[Continue[Boolean]] = (tm1,tm2) match {
    case (cmatch(t1,x1,left1,right1,to1),cmatch(t2,x2,left2,right2,to2)) =>
      val tpAB = check.inferType(t1).getOrElse(check.inferType(t2).getOrElse(return None))
      check.simplify(tpAB) match {
        case Coprod(tpA,tpB) =>
          check.check(Equality(stack,t1,t2,None))
          check.check(Equality(stack ++ x1%tpA,left1,left2^?x2/OMV(x1),None))
          check.check(Equality(stack ++ x1%tpB,right1,right2^?x2/OMV(x1),None))
          check.check(Equality(stack ++ x1%tpAB,to1,to2^?x2/OMV(x1),None))
          Some(Continue(true))
        case _ => None
      }
    case _ => None
  }
}