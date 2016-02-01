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
    case cmatch(u,t1,t2,v,cotp,c) =>
      val (x1,tpA,f) = solver.safeSimplifyUntil(t1)(ccase.unapply)._1 match {
        case ccase(y,a,b) => (y,a,b)
        case _ => return None
      }
      val (x2,tpB,g) = solver.safeSimplifyUntil(t2)(ccase.unapply)._1 match {
        case ccase(y,a,b) => (y,a,b)
        case _ => return None
      }
      val (x,_) = Common.pickFresh(solver, v)
      val apb = solver.safeSimplifyUntil(solver.inferType(u)(stack,history).getOrElse(return None))(Coprod.unapply)._1
      if(!( apb match {
        case Coprod(a,b) => solver.check(Equality(stack,a,tpA,Some(OMS(Typed.ktype)))) &&
          solver.check(Equality(stack,b,tpB,Some(OMS(Typed.ktype))))
        case _ => false
      })) return None
      solver.check(Equality(stack,cotp,apb,Some(OMS(Typed.ktype))))

      if(!(solver.check(Typing(stack++x%tpA,f ^? x1/OMV(x),c ^? v/inl(x,tpB))) && solver.check(Typing(stack++x%tpB,g ^? x2/OMV(x),c ^? v/inr(x,tpA)))))
        return None

      Some(c ^? v/u)
    case _ => None
  }

}


/** Computation: the rules : - inl(a,B) match case x.f; case x.g = f[x/a]
  *                          - inr(b,A) match case x.f; case x.g = g[x/b]
  */

object MatchComp extends ComputationRule(cmatch.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term]
  = tm match {
    case cmatch(u,t1,t2,v,_,_) =>
      val (a,b,tpA,tpB) = solver.simplify(u) match {
        case inl(t,tp) => (Some(t),None,solver.inferType(t).getOrElse(return None),tp)
        case inr(t,tp) => (None,Some(t),tp,solver.inferType(t).getOrElse(return None))
        case _ => throw NotApplicable
      }
      val (f,x1,g,x2) = (solver.simplify(t1),solver.simplify(t2)) match {
        case (ccase(y1,tpA2,fx),ccase(y2,tpB2,gx)) => if (!(
          solver.check(Equality(stack,tpA,tpA2,Some(OMS(Typed.ktype)))) &&
          solver.check(Equality(stack,tpB,tpB2,Some(OMS(Typed.ktype)))))) return None
          else (fx,y1,gx,y2)
        case _ => throw NotApplicable
      }
      history += "Expanding match on coproduct embedding"
      if (a.isDefined) Some(solver.simplify(f ^? (x1/a.get)))
      else if (b.isDefined) Some(solver.simplify(g ^? (x2/b.get))) else None
    case _ => None
  }
}

/** Computation rules for function addition **/

object AddFuncComp extends ComputationRule(Addfunc.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term]
  = tm match {
    case Addfunc(t1,t2) =>
      (solver.simplify(solver.inferType(t1)(stack,history).getOrElse(return None)),
        solver.simplify(solver.inferType(t2)(stack,history).getOrElse(return None))) match {
        case (Pi(y1,tpA,tpC),Pi(y2,tpB,tpC2)) =>
          val (x,_) = Context.pickFresh(stack.context,LocalName("x"))
          val (x1,sub1) = Context.pickFresh((stack++x%Coprod(tpA,tpB)).context,y1)
          solver.check(Equality(stack++x%Coprod(tpA,tpB),tpC ^? y1/inl(x,tpB),tpC2 ^? y2/inr(x,tpA),Some(OMS(Typed.ktype))))
          history+="Expanding function addition"
          Some(Lambda(x,Coprod(tpA,tpB),
            solver.simplify(cmatch(OMV(x),x1,tpA,Apply(t1,OMV(x1)),x1,tpB,Apply(t2,OMV(x1)),x1,Coprod(tpA,tpB),tpC ^? y1/OMV(x1)))(stack++x%Coprod(tpA,tpB),history)
           // cmatch(OMV(x),x1,tpA,Apply(t1 ^? sub1,OMV(x1)),tpC,x1,tpB,Apply(t2 ^? y2/OMV(x1),OMV(x1)),tpC2)
          ))
        case _ => None
      }
    case _ => None
  }
}

object AddFuncApply extends ComputationRule(Apply.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term]
  = tm match {
    case Apply(p,u) => solver.simplify(p) match {
      case Addfunc(t1,t2) => Some(solver.simplify(ApplySpine(AddFuncComp(solver)(Addfunc(t1,t2),false).getOrElse(return None),u)))
      case _ => throw NotApplicable
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

/** Equality rules for case terms */

object CaseEquality extends TermHeadBasedEqualityRule(Nil, ccase.path, ccase.path) {
  def apply(checker: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
    (tm1,tm2) match {
      case (ccase(x1,tp1,t1), ccase(x2,tp2,t2)) =>
        val cont = Continue {
          history += "congruence for cases"
          val res1 = checker.check(Equality(stack,tp1,tp2,None))
          val (x,_) = Context.pickFresh(stack.context,x1)
          val res2 = checker.check(Equality(stack++x%tp1,t1 ^? x1/OMV(x),t2 ^? x2/OMV(x),None))
          res1 && res2
        }
        Some(cont)
      case _ => None
    }
  }
}

object CaseToEquality extends TermHeadBasedEqualityRule(Nil, cto.path, cto.path) {
  def apply(checker: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
    (tm1,tm2) match {
      case (cto(x1,tp1,t1), cto(x2,tp2,t2)) =>
        val cont = Continue {
          history += "congruence for casetypes"
          val res1 = checker.check(Equality(stack,tp1,tp2,None))
          val (x,_) = Context.pickFresh(stack.context,x1)
          val res2 = checker.check(Equality(stack++x%tp1,t1 ^? x1/OMV(x),t2 ^? x2/OMV(x),None))
          res1 && res2
        }
        Some(cont)
      case _ => None
    }
  }
}
