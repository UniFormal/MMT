package info.kwarc.mmt.stex.rules

import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History, InferenceAndTypingRule, InferenceRule, InhabitableRule, Solver, UniverseRule}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.{Context, Inhabitable, OMA, OMBINDC, OMS, Stack, Term, Typing, Universe}
import info.kwarc.mmt.api.uom.{RecurseOnly, Simplifiability, Simplify}
import info.kwarc.mmt.api.{GlobalName, LocalName, ParametricRule, Rule, RuleSet}
import info.kwarc.mmt.stex.{SCtx, SHTML, SHTMLHoas, STerm}

object SeqRules extends ParametricRule {

  case class SeqInfTypeRule(tpsym: GlobalName, expr: GlobalName) extends InferenceAndTypingRule(expr, tpsym) {
    override def apply(solver: Solver, tm: Term, tp: Option[Term], covered: Boolean)(implicit stack: Stack, history: History): (Option[Term], Option[Boolean]) = tm match {
      case SHTMLHoas.OmaSpine(hoas,OMS(`expr`), h :: ls) =>
        tp match {
          case Some(SHTMLHoas.OmaSpine(hoas2,OMS(`tpsym`),List(tpA))) if hoas2 == hoas =>
            if (!covered) {
              (h :: ls).foreach { tm =>
                solver.check(Typing(stack, tm, tpA))
              }
            }
            (tp, Some(true))
          case Some(s@SHTMLHoas.OmaSpine(hoas2,OMS(`expr`), _)) if hoas == hoas2 =>
            if (!covered) {
              solver.inferType(s)
            }
            // TODO ?
            (tp, Some(true))
          case _ =>
            solver.inferType(h) match {
              case None => (None, None)
              case Some(tpA) =>
                ls match {
                  case Nil => (Some(SHTMLHoas.OmaSpine(hoas,OMS(`tpsym`),List(tpA))), Some(true))
                  case _ =>
                    // TODO
                    (None,None)
                    /*
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
                     */
                }
            }
        }
      case _ => (None, None)
    }
  }

  case class InhabRule(tp : GlobalName) extends InhabitableRule(tp) {
    override def applicable(t: Term): Boolean = t match {
      case OMA(OMS(`tp`),List(_)) => true
      case _ => false
    }

    override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = term match {
      case OMA(OMS(`tp`),List(a)) =>
        Some(solver.check(Inhabitable(stack,a)))
      case _ => None
    }
  }

  case class UnivRule(tp : GlobalName) extends UniverseRule(tp) {
    override def applicable(t: Term): Boolean = t match {
      case OMA(OMS(`tp`), List(_)) => true
      case _ => false
    }

    override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = term match {
      case OMA(OMS(`tp`), List(a)) =>
        Some(solver.check(Universe(stack, a)))
      case _ => None
    }

  }

  case class SeqTypeRule(tpsym : GlobalName) extends InferenceRule(tpsym,tpsym) {
    def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
      case SHTMLHoas.OmaSpine(_,OMS(`tpsym`),List(tpA)) =>
        if (!covered) {
          solver.check(Inhabitable(stack,tpA))
        }
        solver.inferType(tpA,covered)
      case _ =>
        None
    }
  }
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(sq),OMS(ell),OMS(tp)) =>
      RuleSet(InhabRule(tp),SeqInfTypeRule(tp,sq),SeqTypeRule(tp),UnivRule(tp))
    case _ =>
      ???
  }
}


object FoldRRule extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(head),OMS(seqexpr),OMS(seqtp)) =>
      RuleSet(Compute(head,seqexpr,seqtp),Infer(head,seqexpr,seqtp))
    case _ =>
      ???
  }

  import info.kwarc.mmt.api.objects.Conversions._

  case class Fold(hhead : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends Rule {
    def apply(h : Option[SHTMLHoas.HoasRule], init:Term,seq : Term,v1 : LocalName,tp1:Term,v2:LocalName,tp2:Term,f : Term) = h match {
      case Some(h) =>
        h.HOMB(OMS(hhead),List(STerm(init),STerm(seq),SCtx(Context(v1 % tp1)),SCtx(Context(v2 % tp2)),STerm(f)))
      case None =>
        OMBINDC(OMS(hhead), Context(v1 % tp1, v2 % tp2), init :: seq :: f :: Nil)
    }
    def unapply(tm : Term) = tm match {
      case SHTMLHoas.Omb(h,OMS(`hhead`),List(STerm(init),STerm(seq),SCtx(Context(v1)),SCtx(Context(v2)),STerm(f))) if v1.tp.isDefined && v2.tp.isDefined =>
        Some((Some(h),seq,init,v1.name,v1.tp.get,v2.name,v2.tp.get,f))
      case OMBINDC(OMS(`hhead`), Context(v1, v2), init :: seq :: f :: Nil) if v1.tp.isDefined && v2.tp.isDefined =>
        Some((None, seq, init, v1.name, v1.tp.get, v2.name, v2.tp.get, f))
      case _ => None
    }
  }

  case class Infer(hhead : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends InferenceRule(hhead,SHTML.judgmentholds.sym) {
    val fold = Fold(head,seqexpr,seqtp)
    override def applicable(t: Term): Boolean = t match {
      case fold(_,_,_,_,_,_,_,_) => true
      case _ => false
    }
    override def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      //val tr = solver.rules.getByHead(classOf[TypeRule],seqtp).headOption.getOrElse { return None }
      tm match {
        case fold(h,seq,init,v1n,v1tp,v2n,v2tp,f) =>
          val tp = solver.inferType(init).getOrElse { return None }
          val tp2 = solver.inferType(seq) match {
            case Some(SHTML.flatseq.tp(a)) => a
            case _ => return None
          }
          if (!covered) {
            solver.check(Typing(stack,init,v1tp))
            solver.check(Typing(stack ++ (v1n % tp) ++ (v2n % tp2),f,v2tp))
          }
          solver.inferType(f)(stack ++ (v1n % tp) ++ (v2n % tp2),history)
        case _ => None
      }
    }
  }

  case class Compute(hhead : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends ComputationRule(hhead) {
    val fold = Fold(head, seqexpr, seqtp)
    override def applicable(t: Term): Boolean = t match {
      case fold(_, _, _, _, _, _, _, _) => true
      case _ => false
    }
    override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
      check match {
        case solver:Solver =>
          //val tr = solver.rules.getByHead(classOf[TypeRule],seqtp).headOption.getOrElse { return Simplifiability.NoRecurse }
          tm match {
            case fold(h,SHTML.flatseq(ls),init,v1n,v1tp,v2n,v2tp,f) =>
              val ret = ls.foldRight(init)((t,i) => {
                if (!covered) {
                  solver.check(Typing(stack, t, v1tp))
                  solver.check(Typing(stack, i, v2tp))
                }
                (f ^? (v1n / t)) ^? (v2n / i)
              })
              Simplify(ret)
            case fold(_,_,_,_,_,_,_,_) =>
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
    case List(OMS(head), OMS(seqexpr), OMS(seqtp)) =>
      RuleSet(Compute(head, seqexpr, seqtp), Infer(head, seqexpr, seqtp))
    case _ =>
      ???
  }

  case class Last(head: GlobalName, seqexpr: GlobalName, seqtp: GlobalName) extends Rule {
    def apply(h: Option[SHTMLHoas.HoasRule], seq: Term) = SHTMLHoas.OmaSpine(h, OMS(`head`), List(seq)) // SOMA(OMS(`head`), seq)

    def unapply(tm: Term) = tm match {
      case SHTMLHoas.OmaSpine(h, OMS(`head`), List(seq)) =>
        Some((h, seq))
      case _ => None
    }
  }

  case class Infer(hhead: GlobalName, seqexpr: GlobalName, seqtp: GlobalName) extends InferenceRule(hhead, SHTML.judgmentholds.sym) {
    val last = Last(head, seqexpr, seqtp)

    override def applicable(t: Term): Boolean = t match {
      case last(_, _) => true
      case _ => false
    }

    override def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      //val tr = solver.rules.getByHead(classOf[TypeRule],seqtp).headOption.getOrElse { return None }
      tm match {
        case last(_, seq) =>
          solver.inferType(seq) match {
            case Some(SHTML.flatseq.tp(tp)) => Some(tp)
            case _ => None
          }
        case _ => None
      }
    }
  }

  case class Compute(hhead: GlobalName, seqexpr: GlobalName, seqtp: GlobalName) extends ComputationRule(hhead) {
    val last = Last(head, seqexpr, seqtp)

    override def applicable(t: Term): Boolean = t match {
      case last(_, _) => true
      case _ => false
    }

    override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
      tm match {
        case last(_, SHTML.flatseq(ls)) if ls.nonEmpty =>
          Simplify(ls.last)
        case last(_) =>
          RecurseOnly(List(1))
        case _ =>
          Simplifiability.NoRecurse
      }
      /*
      check match {
        case solver:Solver =>
          //val tr = solver.rules.getByHead(classOf[TypeRule],seqtp).headOption.getOrElse { return Simplifiability.NoRecurse }
          tm match {
            case last(_,SHTML.flatseq(ls)) if ls.nonEmpty =>
              Simplify(ls.last)
            case last(_) =>
              RecurseOnly(List(1))
            case _ =>
              Simplifiability.NoRecurse
          }
        case _ => Simplifiability.NoRecurse
      }
    }
       */
    }
  }
}

object SeqInitRule extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(head),OMS(seqexpr),OMS(seqtp)) =>
      RuleSet(Compute(head,seqexpr,seqtp),Infer(head,seqexpr,seqtp))
    case _ =>
      ???
  }


  case class Init(head: GlobalName, seqexpr: GlobalName, seqtp: GlobalName) extends Rule {
    //def apply(seq: Term) = SOMA(OMS(`head`), seq)

    def unapply(tm: Term) = tm match {
      case SHTMLHoas.OmaSpine(h,OMS(`head`),List(seq)) =>
        Some((h,seq))
      case _ => None
    }
  }

  case class Infer(hhead : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends InferenceRule(hhead,SHTML.judgmentholds.sym) {
    val inits = Init(head, seqexpr, seqtp)
    override def applicable(t: Term): Boolean = t match {
      case inits(_) => true
      case _ => false
    }
    override def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
      //val tr = solver.rules.getByHead(classOf[TypeRule],seqtp).headOption.getOrElse { return None }
      tm match {
        case inits(_,seq) =>
          solver.inferType(seq)
        case _ => None
      }
    }
  }

  import info.kwarc.mmt.api.objects.Conversions._

  case class Compute(hhead : GlobalName, seqexpr : GlobalName, seqtp : GlobalName) extends ComputationRule(hhead) {
    val inits = Init(head, seqexpr, seqtp)
    override def applicable(t: Term): Boolean = t match {
      case inits(_) => true
      case _ => false
    }
    override def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
      tm match {
        case inits(_, SHTML.flatseq(ls)) if ls.nonEmpty =>
          Simplify(SHTML.flatseq(ls.init))
        case inits(_) =>
          RecurseOnly(List(1))
        case _ =>
          Simplifiability.NoRecurse
      }
      /*check match {
        case solver:Solver =>
          //val tr = solver.rules.getByHead(classOf[TypeRule],seqtp).headOption.getOrElse { return Simplifiability.NoRecurse }
          tm match {
            case inits(_,SHTML.flatseq(ls)) if ls.nonEmpty =>
              Simplify(SHTML.flatseq(ls.init))
            case inits(_) =>
              RecurseOnly(List(1))
            case _ =>
              Simplifiability.NoRecurse
          }
        case _ => Simplifiability.NoRecurse
      } */
    }
  }

}

/*
import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History, InferenceAndTypingRule, InferenceRule, Solver, UniverseRule}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom.{RecurseOnly, Simplifiability, Simplify}
import info.kwarc.mmt.api.{GlobalName, LocalName, ParametricRule, Rule, RuleSet}
import info.kwarc.mmt.stex.{SCtx, SOMA, SOMB, STeX, STerm}

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
      case OMA(OMS(`seqtppath`),List(t/*,l,u*/)) =>
        Some(t)
      case _ => None
    }
    def apply(tp : Term/*,lower: Term,upper:Term*/) = OMA(OMS(seqtppath),List(tp/*,lower,upper*/))
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

case class UnivRule(seqtppath : GlobalName,seqexprpath : GlobalName) extends UniverseRule(seqtppath) with SeqRule {
  override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = term match {
    case Seqtype(tp) =>
    Some(solver.check(Universe(stack,tp)))
    case _ => None

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


object SequenceLike extends ParametricRule {

  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(seqexpr),OMS(seqtp)) =>
      RuleSet(TypeRule(seqtp,seqexpr),SeqInfTypeRule(seqtp, seqexpr),UnivRule(seqtp,seqexpr))
    case _ =>
      ???
  }
}







 */