package info.kwarc.mmt.stex.rules

import info.kwarc.mmt.api.{ParametricRule, Rule, RuleSet}
import info.kwarc.mmt.api.checking.{History, InferenceAndTypingRule, InhabitableRule, Solver, SubtypingRule, UniverseRule}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.{Stack, Term}

object UniverseRule extends ParametricRule {
  case class UnivRule(pattern : Pattern) extends UniverseRule(pattern.head) with UsesPatterns {
    override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = Some(true) // by applicability
  }

  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(tm) =>
      val pattern = new Pattern(tm)
      RuleSet(UnivRule(pattern),InhabitableRule.InhabRule(pattern))
    case _ =>
      ???
  }
}

object InhabitableRule extends ParametricRule {

  case class InhabRule(pattern: Pattern) extends InhabitableRule(pattern.head) with UsesPatterns {
    override def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Option[Boolean] = Some(true) // by applicability
  }

  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(tm) =>
      val pattern = new Pattern(tm)
      InhabRule(pattern)
    case _ =>
      ???
  }
}

object HasType extends ParametricRule {
  case class HasTypeRule(tmp : Pattern,tpp : Pattern) extends InferenceAndTypingRule(tmp.head,tpp.head) {

    override def applicable(t: Term): Boolean = t match {
      case tmp(_) => true
      case _ => false
    }
    override def apply(solver: Solver, tm: Term, tp: Option[Term], covered: Boolean)(implicit stack: Stack, history: History): (Option[Term], Option[Boolean]) = (tm,tp) match {
      case (tmp(lsA),Some(tpp(lsB))) =>
        if (!covered) {
          (lsA ::: lsB).foreach{ case (_,t) =>
            solver.inferType(t,covered)
          }
        }
        (tp,Some(true))
      case (tmp(ls),None) =>
        val ret = tpp.instantiate(ls)
        (ret,ret.map(_ => true))
      case _ => (None,None)
    }
  }
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(tm,tp) =>
      val tmp = new Pattern(tm)
      val tpp = new Pattern(tp)
      RuleSet(HasTypeRule(tmp,tpp))
    case _ =>
      ???
  }
}


object SubType extends ParametricRule {
  case class SubTypeRule(tmp : Pattern,tpp : Pattern) extends SubtypingRule { //InferenceAndTypingRule(tmp.head,tpp.head) {
    val head = tmp.head
    override def applicable(t: Term,tp:Term): Boolean = (t,tp) match {
      case (tmp(_),tpp(_)) => true
      case _ => false
    }

    override def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History): Option[Boolean] = (tp1,tp2) match {
      case (tmp(lsA),tpp(lsB)) =>
        (lsA ::: lsB).foreach{ case (_,t) =>
          solver.inferType(t,false)
        }
        Some(true)
      case _ => None
    }
  }
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(tm,tp) =>
      val tmp = new Pattern(tm)
      val tpp = new Pattern(tp)
      RuleSet(SubTypeRule(tmp,tpp))
    case _ =>
      ???
  }
}