package info.kwarc.mmt.stex.rules

import info.kwarc.mmt.api.{GlobalName, ParametricRule, Rule, RuleSet}
import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History, InferenceAndTypingRule, InhabitableRule, Solver}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.{Equality, Inhabitable, OMA, OMBIND, OMS, OMV, Stack, Subtyping, Term, Typing}
import info.kwarc.mmt.stex.{SCtx, SHTML, SHTMLHoas}
import info.kwarc.mmt.api.objects.Conversions._
import info.kwarc.mmt.api.uom.{RecurseOnly, Simplifiability, Simplify}

object PiLikeRuleInh extends ParametricRule {

  case class PiLikeRuleInhI(hhead: GlobalName) extends InhabitableRule(hhead) {
    override def applicable(t: Term): Boolean = t match {
      case SHTMLHoas.bound(_,OMS(`hhead`), _, _, _) => true
      case SHTMLHoas.OmaSpine(_,OMS(`hhead`),List(_,_)) => true
      case _ => false
    }

    def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = term match {
      case SHTMLHoas.bound(_,OMS(`hhead`), x, Some(tp), tm) =>
        solver.check(Inhabitable(stack, tp))
        Some(solver.check(Inhabitable(stack ++ x % tp, tm)))
      case SHTMLHoas.OmaSpine(_,OMS(`hhead`),List(a,b)) =>
        solver.check(Inhabitable(stack, a))
        Some(solver.check(Inhabitable(stack, b)))
      case _ => None
    }
  }

  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(h)) => PiLikeRuleInhI(h)
    case _ =>
      ???
  }
}

object LambdaLikeRule extends ParametricRule {
  case class LambdaTypingRule(lambda:GlobalName,pi:GlobalName) extends InferenceAndTypingRule(lambda,pi) {
    def apply(solver: Solver, tm: Term, tp: Option[Term], covered: Boolean)(implicit stack: Stack, history: History): (Option[Term], Option[Boolean]) = {
      (tm,tp) match {
        case (SHTMLHoas.bound(h,OMS(`lambda`),x,Some(xtp),bd),None) =>
          if (!covered) {
            solver.check(Inhabitable(stack,xtp))
          }
          solver.inferType(bd,covered)(stack ++ x%xtp,history) match {
            case Some(tp) =>
              (Some(SHTMLHoas.bound(h,OMS(`pi`),x,Some(xtp),tp)),Some(true))
            case _ => (None,None)
          }
        case (SHTMLHoas.bound(h, OMS(`lambda`), x, Some(xtp), bd), Some(SHTMLHoas.bound(h2,OMS(`pi`),x2,Some(xtp2),bd2))) if h == h2 =>
          if (!covered) {
            solver.check(Inhabitable(stack, xtp))
            solver.check(Equality(stack, xtp, xtp2, None))
            (solver.safeSimplifyUntil(xtp)(SHTML.flatseq.tp.unapply)._1, solver.safeSimplifyUntil(xtp2)(SHTML.flatseq.tp.unapply)._1) match {
              case (SHTML.flatseq.tp(_), SHTML.flatseq.tp(_)) =>
                solver.check(Typing(stack ++ x % xtp, bd, bd2 ^? (x2 / OMV(x))))
              case (_, SHTML.flatseq.tp(_)) =>
                solver.check(Typing(stack ++ x % xtp, bd, bd2 ^? (x2 / SHTML.flatseq(List(OMV(x))))))
              case _ =>
                solver.check(Typing(stack ++ x % xtp, bd, bd2 ^? (x2 / OMV(x))))
            }
          }
          (tp,Some(true))
        case _ => (None,None)
      }
    }
  }

  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(l),OMS(p)) => LambdaTypingRule(l,p)
    case _ =>
      ???
  }

}

object LambdaLikeArrowRule extends ParametricRule {
  case class LambdaTypingRule(lambda:GlobalName,arr:GlobalName) extends InferenceAndTypingRule(lambda,arr) {

    override def applicable(t: Term): Boolean = t match {
      case SHTMLHoas.bound(h,OMS(`lambda`),x,Some(xtp),bd) => true
      case _ => false
    }
    def apply(solver: Solver, tm: Term, tp: Option[Term], covered: Boolean)(implicit stack: Stack, history: History): (Option[Term], Option[Boolean]) = {
      (tm,tp) match {
        case (SHTMLHoas.bound(h,OMS(`lambda`),x,Some(xtp),bd),None) =>
          if (!covered) {
            solver.check(Inhabitable(stack, xtp))
          }
          solver.inferType(bd,covered)(stack ++ x%xtp,history) match {
            case Some(tp) if !tp.freeVars.contains(x) =>
              (Some(SHTMLHoas.OmaSpine(h,OMS(`arr`),List(xtp,tp))),Some(true))
            case _ => (None,None)
          }
        case (SHTMLHoas.bound(h, OMS(`lambda`), x, Some(xtp), bd), Some(SHTMLHoas.OmaSpine(h2,OMS(`arr`),List(xtp2,bd2)))) if h.contains(h2) =>
          if (!covered) {
            solver.check(Inhabitable(stack, xtp))
            solver.check(Equality(stack, xtp, xtp2, None))
            (solver.safeSimplifyUntil(xtp)(SHTML.flatseq.tp.unapply)._1, solver.safeSimplifyUntil(xtp2)(SHTML.flatseq.tp.unapply)._1) match {
              case (SHTML.flatseq.tp(_), SHTML.flatseq.tp(_)) =>
                solver.check(Typing(stack ++ x % xtp, bd, bd2))
              case (_, SHTML.flatseq.tp(_)) =>
                solver.check(Typing(stack ++ x % xtp, bd, bd2))
              case _ =>
                solver.check(Typing(stack ++ x % xtp, bd, bd2))
            }
          }
          (tp,Some(true))
        case _ => (None,None)
      }
    }
  }


  case class ApplyTypingRule(app: GlobalName, arr: GlobalName) extends InferenceAndTypingRule(app, arr) {
    def apply(solver: Solver, tm: Term, tp: Option[Term], covered: Boolean)(implicit stack: Stack, history: History): (Option[Term], Option[Boolean]) = {
      (tm, tp) match {
        case (OMA(OMS(`app`), List(f, a)), None) =>
          solver.inferType(f, covered) match {
            case Some(SHTMLHoas.OmaSpine(h,OMS(`arr`),tA :: rest)) if rest.nonEmpty =>
              if (!covered) {
                solver.check(Typing(stack,a,tA))
              }
              if (rest.length == 1) {
                (Some(rest.head),Some(true))
              } else {
                (Some(SHTMLHoas.OmaSpine(h,OMS(`arr`),rest)),Some(true))
              }
            case _ => (None, None)
          }
        case (OMA(OMS(`app`), List(f, a)), Some(t)) =>
          solver.inferType(f, covered) match {
            case Some(SHTMLHoas.OmaSpine(h, OMS(`arr`), tA :: rest)) if rest.nonEmpty =>
              if (!covered) {
                solver.check(Typing(stack, a, tA))
                val tpB = if (rest.length == 1) rest.head else SHTMLHoas.OmaSpine(h,OMS(`arr`),rest)
                solver.check(Subtyping(stack,tpB,t))
              }
              (tp, Some(true))
            case _ => (None, None)
          }
        case _ => (None, None)
      }
    }
  }

  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(a),OMS(l),OMS(p)) => RuleSet(LambdaTypingRule(l,p),ApplyTypingRule(a,p),ApplyRule.ApplyCompRule(a,l))
    case _ =>
      ???
  }

}


object ApplyRule extends ParametricRule {
  case class ApplyTypingRule(app:GlobalName,pi:GlobalName) extends InferenceAndTypingRule(app,pi) {
    def apply(solver: Solver, tm: Term, tp: Option[Term], covered: Boolean)(implicit stack: Stack, history: History): (Option[Term], Option[Boolean]) = {
      (tm,tp) match {
        case (OMA(OMS(`app`),List(f,a)),None) =>
          solver.inferType(f,covered) match {
            case Some(SHTMLHoas.bound(_,OMS(`pi`),x,Some(t),r)) =>
              val tpA = solver.inferType(a,covered).getOrElse{
                return (None,None)
              }
              (solver.safeSimplifyUntil(tpA)(SHTML.flatseq.tp.unapply)._1, solver.safeSimplifyUntil(t)(SHTML.flatseq.tp.unapply)._1) match {
                case (SHTML.flatseq.tp(_), SHTML.flatseq.tp(_)) =>
                  if (!covered) {
                    solver.check(Typing(stack, a, t, None))
                  }
                  (Some(r ^? (x / a)), Some(true))
                case (_, SHTML.flatseq.tp(tA)) =>
                  if (!covered) {
                    solver.check(Typing(stack, a, tA, None))
                  }
                  (Some(r ^? (x / SHTML.flatseq(List(a)))), Some(true))
                case _ =>
                  if (!covered) {
                    solver.check(Typing(stack, a, t, None))
                  }
                  (Some(r ^? (x / a)), Some(true))
              }
            case _ => (None,None)
          }
        case (OMA(OMS(`app`),List(f,a)), Some(t)) =>
          solver.inferType(f, covered) match {
            case Some(SHTMLHoas.bound(_, OMS(`pi`), x, Some(t2), r)) =>
              val tpA = solver.inferType(a, covered).getOrElse {
                return (None, None)
              }
              if (!covered) {
                (solver.safeSimplifyUntil(tpA)(SHTML.flatseq.tp.unapply)._1, solver.safeSimplifyUntil(t2)(SHTML.flatseq.tp.unapply)._1) match {
                  case (SHTML.flatseq.tp(_), SHTML.flatseq.tp(_)) =>
                    solver.check(Typing(stack, a, t2, None))
                    solver.check(Equality(stack, r ^? (x / a), t, None))
                  case (_, SHTML.flatseq.tp(tA)) =>
                    solver.check(Typing(stack, a, tA, None))
                    solver.check(Equality(stack, r ^? (x / SHTML.flatseq(List(a))), t, None))
                  case _ =>
                    solver.check(Typing(stack, a, t2, None))
                    solver.check(Equality(stack, r ^? (x / a), t, None))
                }
              }
              (tp, Some(true))
            case _ => (None, None)
          }
        case _ => (None,None)
      }
    }
  }

  case class ApplyCompRule(app : GlobalName,lambda:GlobalName) extends ComputationRule(app) {
    override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
      tm match {
        case OMA(OMS(`app`),List(f @ SHTMLHoas.bound(_,OMS(`lambda`),x,Some(tp),bd),a)) =>
          if (!covered) {
            check.inferType(f)
            check.check(Typing(stack,a,tp,None))
          }
          val tpA = check.inferType(a).getOrElse {
            return Simplifiability.NoRecurse
          }
          (check.safeSimplifyUntil(tpA)(SHTML.flatseq.tp.unapply)._1, check.safeSimplifyUntil(tp)(SHTML.flatseq.tp.unapply)._1) match {
            case (SHTML.flatseq.tp(_),SHTML.flatseq.tp(_)) =>
              Simplify(bd ^? (x / a))
            case (_,SHTML.flatseq.tp(_)) =>
              Simplify(bd ^? (x / SHTML.flatseq(List(a))))
            case _ =>
              Simplify(bd ^? (x / a))
          }
        case OMA(OMS(`app`),List(f,_)) =>
          RecurseOnly(tm.subobjects.indexWhere(_._2 == f) :: Nil)
        case _ => Simplifiability.NoRecurse
      }
    }
  }

  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(a),OMS(l),OMS(p)) =>
      RuleSet(ApplyTypingRule(a,p),ApplyCompRule(a,l))
    case _ =>
      ???
  }

}