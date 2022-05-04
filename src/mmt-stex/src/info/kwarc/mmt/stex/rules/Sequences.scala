package info.kwarc.mmt.stex.rules

import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History, InferenceAndTypingRule, InferenceRule, Solver}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom.{RecurseOnly, Simplifiability, Simplify}
import info.kwarc.mmt.api.{GlobalName, LocalName, ParametricRule, Rule, RuleSet}
import info.kwarc.mmt.stex.{SCtx, SOMB, STeX, STerm}

trait SeqRule {
  val seqexprpath : GlobalName
  val seqtppath : GlobalName
  object SeqLike {
    def apply(tm : Term) = tm match {
      case STeX.flatseq(_) => true
      case OMV(_) if tm.metadata.get(STeX.flatseq.sym).nonEmpty => true
      case _ => false
    }
  }
  object Seqtype {
    def unapply(tp : Term) = tp match {
      case OMA(OMS(`seqtppath`),List(t)) =>
        Some(t)
      case _ => None
    }
    def apply(tp : Term) = OMA(OMS(seqtppath),List(tp))
  }
  object Seqexpr {
    def unapply(tp : Term) = tp match {
      case OMA(OMS(`seqexprpath`),ls) =>
        Some(ls)
      case _ => None
    }
    def apply(ls : List[Term]) = OMA(OMS(seqexprpath),ls)
  }
}

case class TypeRule(seqtppath : GlobalName,seqexprpath : GlobalName) extends InferenceRule(seqtppath,STeX.judgmentholds.sym) with SeqRule {
  override def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
    tm match {
      case Seqtype(tp) =>
        solver.inferType(tp)
      case _ => None
    }
  }
}

case class SeqInfTypeRule(seqtppath : GlobalName,seqexprpath : GlobalName) extends InferenceAndTypingRule(seqexprpath,STeX.judgmentholds.sym) with SeqRule {
  override def apply(solver: Solver, tm: Term, tp: Option[Term], covered: Boolean)(implicit stack: Stack, history: History): (Option[Term], Option[Boolean]) = tm match {
    case Seqexpr(h :: ls) =>
      tp match {
        case Some(Seqtype(tpA)) =>
          if (!covered) (h :: ls).foreach { tm =>
            solver.check(Typing(stack, tm, tpA))
          }
          (tp, Some(true))
        case Some(Seqexpr(tps)) =>
          // TODO ?
          (tp,Some(true))
        case _ =>
          solver.inferType(h) match {
            case None => (None, None)
            case Some(tpA) =>
              ls match {
                case Nil => (Some(Seqtype(tpA)), Some(true))
                case _ =>
                  solver.inferType(Seqexpr(ls)).flatMap(solver.safeSimplifyUntil(_)(Seqtype.unapply)._2) match {
                    case Some(tpAa) =>
                      solver.tryToCheckWithoutDelay(Subtyping(stack, tpA, tpAa)) match {
                        case Some(true) =>
                          tp.foreach { t => solver.check(Subtyping(stack, Seqtype(tpAa), t)) }
                          (Some(Seqtype(tpAa)), Some(true))
                        case _ =>
                          tp.foreach { t => solver.check(Subtyping(stack, Seqtype(tpAa), t)) }
                          (Some(Seqtype(tpA)), Some(solver.check(Subtyping(stack, tpAa, tpA))))
                      }
                    case _ =>
                      (None, None)
                  }
              }
          }
      }
    case _ => (None,None)
  }
}

object SequenceLike extends ParametricRule {

  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(seqexpr),OMS(seqtp)) =>
      RuleSet(TypeRule(seqtp,seqexpr),SeqInfTypeRule(seqtp, seqexpr))
    case _ =>
      ???
  }
}

object FoldRRule extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(head),OMS(seqexpr),OMS(seqtp)) =>
      RuleSet(Fold(head,seqexpr,seqtp),Compute(head,seqexpr,seqtp),Infer(head,seqexpr,seqtp))
    case _ =>
      ???
  }

  case class Fold(hhead : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends Rule {
    def apply(init:Term,seq : Term,v1 : LocalName,v2:LocalName,f : Term) = SOMB(OMS(hhead),STerm(init),STerm(seq),SCtx(Context(VarDecl(v1))),SCtx(Context(VarDecl(v2))),STerm(f))
    def unapply(tm : Term) = tm match {
      case SOMB(OMS(`hhead`),List(STerm(init),STerm(seq),SCtx(Context(v1)),SCtx(Context(v2)),STerm(f))) =>
        Some((seq,init,v1.name,v1.tp,v2.name,v2.tp,f))
      case _ => None
    }
  }
  import info.kwarc.mmt.api.objects.Conversions._

  case class Infer(hhead : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends InferenceRule(hhead,STeX.judgmentholds.sym) {
    override def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      val fold = Fold(head,seqexpr,seqtp)
      val tr = solver.rules.getByHead(classOf[TypeRule],seqtp).headOption.getOrElse { return None }
      tm match {
        case fold(seq,init,v1n,v1tp,v2n,v2tp,f) =>
          val tp = solver.inferType(init).getOrElse { return None }
          val tp2 = solver.inferType(seq) match {
            case Some(tr.Seqtype(a)) => a
            case _ => return None
          }
          if (!covered) {
            v1tp.foreach(tp => solver.check(Typing(stack,init,tp)))
            v2tp.foreach(tpi => solver.check(Typing(stack ++ (v1n % tp) ++ (v2n % tp2),f,tpi)))
          }
          solver.inferType(f)(stack ++ (v1n % tp) ++ (v2n % tp2),history)
        case _ => None
      }
    }
  }

  case class Compute(hhead : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends ComputationRule(hhead) {
    override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
      check match {
        case solver:Solver =>
          val fold = Fold(head,seqexpr,seqtp)
          val tr = solver.rules.getByHead(classOf[TypeRule],seqtp).headOption.getOrElse { return Simplifiability.NoRecurse }
          tm match {
            case fold(tr.Seqexpr(ls),init,v1n,v1tp,v2n,v2tp,f) =>
              val ret = ls.foldRight(init)((t,i) => {
                if (!covered) {
                  v1tp.foreach(tp => solver.check(Typing(stack, t, tp)))
                  v2tp.foreach(tp => solver.check(Typing(stack, i, tp)))
                }
                (f ^? (v1n / t)) ^? (v2n / i)
              })
              Simplify(ret)
            case fold(_,_,_,_,_,_,_) =>
              RecurseOnly(List(1))
            case _ =>
              Simplifiability.NoRecurse
          }
        case _ => Simplifiability.NoRecurse
      }
    }
  }


}


object SeqInitRule extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(head),OMS(seqexpr),OMS(seqtp)) =>
      RuleSet(Init(head,seqexpr,seqtp),Compute(head,seqexpr,seqtp),Infer(head,seqexpr,seqtp))
    case _ =>
      ???
  }

  case class Infer(hhead : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends InferenceRule(hhead,STeX.judgmentholds.sym) {
    override def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      val init = Init(head,seqexpr,seqtp)
      //val tr = solver.rules.getByHead(classOf[TypeRule],seqtp).headOption.getOrElse { return None }
      tm match {
        case init(seq) =>
          solver.inferType(seq)
        case _ => None
      }
    }
  }

  case class Init(head : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends Rule {
    def apply(seq : Term) = OMA(OMS(`head`),List(seq))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`head`),List(seq)) =>
        Some(seq)
      case _ => None
    }
  }
  import info.kwarc.mmt.api.objects.Conversions._

  case class Compute(hhead : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends ComputationRule(hhead) {
    override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
      check match {
        case solver:Solver =>
          val init = Init(head,seqexpr,seqtp)
          val tr = solver.rules.getByHead(classOf[TypeRule],seqtp).headOption.getOrElse { return Simplifiability.NoRecurse }
          tm match {
            case init(tr.Seqexpr(ls)) if ls.nonEmpty =>
              Simplify(tr.Seqexpr(ls.init))
            case init(_) =>
              RecurseOnly(List(1))
            case _ =>
              Simplifiability.NoRecurse
          }
        case _ => Simplifiability.NoRecurse
      }
    }
  }

}


object SeqLastRule extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(head),OMS(seqexpr),OMS(seqtp)) =>
      RuleSet(Last(head,seqexpr,seqtp),Compute(head,seqexpr,seqtp),Infer(head,seqexpr,seqtp))
    case _ =>
      ???
  }

  case class Infer(hhead : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends InferenceRule(hhead,STeX.judgmentholds.sym) {
    override def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      val last = Last(head,seqexpr,seqtp)
      val tr = solver.rules.getByHead(classOf[TypeRule],seqtp).headOption.getOrElse { return None }
      tm match {
        case last(seq) =>
          solver.inferType(seq) match {
            case Some(tr.Seqtype(tp)) => Some(tp)
            case _ => None
          }
        case _ => None
      }
    }
  }

  case class Last(head : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends Rule {
    def apply(seq : Term) = OMA(OMS(`head`),List(seq))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`head`),List(seq)) =>
        Some(seq)
      case _ => None
    }
  }
  import info.kwarc.mmt.api.objects.Conversions._

  case class Compute(hhead : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends ComputationRule(hhead) {
    override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
      check match {
        case solver:Solver =>
          val last = Last(head,seqexpr,seqtp)
          val tr = solver.rules.getByHead(classOf[TypeRule],seqtp).headOption.getOrElse { return Simplifiability.NoRecurse }
          tm match {
            case last(tr.Seqexpr(ls)) if ls.nonEmpty =>
              Simplify(ls.last)
            case last(_) =>
              RecurseOnly(List(1))
            case _ =>
              Simplifiability.NoRecurse
          }
        case _ => Simplifiability.NoRecurse
      }
    }
  }


}