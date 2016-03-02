package info.kwarc.mmt.LFX.TypedHierarchy

import info.kwarc.mmt.LFX.NatLiterals
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects
import info.kwarc.mmt.api.objects.{OMV, OMS, Stack, Term}
import info.kwarc.mmt.lf.{Pi, OfType}
import objects.Conversions._


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
    case DefinedTypeLevel(i) =>
      tp2 match {
        case DefinedTypeLevel(j) => Some(i<=j)
        case TypeUniverse.term => Some(true)
        case OMS(TypeUniverse.altpath) => Some(true)
        case _ => Some(false)
      }
    case TypeLevel(t) => tp2 match {
      case TypeUniverse.term => Some(true)
      case OMS(TypeUniverse.altpath) => Some(true)
      case _ => Some(false)
    }
    case _ => throw RuleNotApplicable
  }
}

object LevelTyping extends TypingRule(TypeLevel.path) {
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History): Boolean =
    solver.inferType(tm).map(solver.safeSimplifyUntil(_)(TypeLevel.unapply)._1).getOrElse(throw RuleNotApplicable) match {
    case DefinedTypeLevel(i) => tp match {
      case DefinedTypeLevel(j) => i<=j
      case t if TypeUniverse.unapply(t) => true
      case _ => throw RuleNotApplicable
    }
    case TypeLevel(t2) => tp match {
      case t if TypeUniverse.unapply(t) => true
      case _ => throw RuleNotApplicable
    }
    case _ => throw RuleNotApplicable
  }
}

object LevelType extends InferenceRule(TypeLevel.path,OfType.path) {
  def apply(solver : Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case DefinedTypeLevel(i) => Some(DefinedTypeLevel(i + 1))
    case TypeLevel(t) => Some(TypeUniverse.term)
    case _ => throw RuleNotApplicable
  }
}

object PiLevel extends FormationRule(Pi.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case Pi(x,NatLiterals.synType,Pi(y,TypeLevel(OMV(x2)),t)) if x==x2 =>
        //if (!covered) isType(solver,a)
        solver.inferType(t,covered)(stack ++ x%NatLiterals.synType ++ y%TypeLevel(OMV(x2)),history).getOrElse(return None)
        Some(TypeUniverse.term)
      case _ => throw RuleNotApplicable
    }
  }
}