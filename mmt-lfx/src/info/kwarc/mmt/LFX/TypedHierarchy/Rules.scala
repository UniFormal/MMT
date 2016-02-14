package info.kwarc.mmt.LFX.TypedHierarchy

import info.kwarc.mmt.api.checking.TypingRule.NotApplicable
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects.{OMS, Stack, Term}
import info.kwarc.mmt.lf.OfType

object TypeLevelUniverse extends UniverseRule(TypeLevel.path) {
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Boolean = tm match {
    case TypeLevel(i) => true
    case _ => false
  }
}

object TypeLevelSubRule extends SubtypingRule {
  val head = TypeLevel.path

  def applicable(tp1: Term, tp2: Term) : Boolean = tp1 match {
    case TypeLevel(i) => true
    case _ => false
  }

  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean] = tp1 match {
    case TypeLevel(i) =>
      tp2 match {
        case TypeLevel(j) => Some(i<=j)
        case t @ TypeUniverse() => Some(true)
        case _ => Some(false)
      }
    case _ => throw TypingRule.NotApplicable
  }
}

object LevelTyping extends TypingRule(TypeLevel.path) {
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History): Boolean = tm match {
    case TypeLevel(i) => tp match {
      case TypeLevel(j) => i<=j
      case t @ TypeUniverse() => true
      case _ => throw TypingRule.NotApplicable
    }
    case _ => throw TypingRule.NotApplicable
  }
}

object LevelType extends InferenceRule(TypeLevel.path,OfType.path) {
  def apply(solver : Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case TypeLevel(i) => Some(TypeLevel(i+1))
    case _ => throw TypingRule.NotApplicable
  }
}