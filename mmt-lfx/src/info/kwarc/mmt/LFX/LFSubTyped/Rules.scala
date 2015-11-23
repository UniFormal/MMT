package info.kwarc.mmt.LFX.LFSubTyped

import info.kwarc.mmt.api.checking.{InferenceRule, History, Solver, UniverseRule}
import info.kwarc.mmt.api.objects.{Inhabitable, OMS, Stack, Term}
import info.kwarc.mmt.lf.OfType

object SubUniverseRule extends UniverseRule(subtypeOf.path) {
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Boolean = tm match {
    case subtypeOf(t) => solver.check(Inhabitable(stack,t))
    case _ => false
  }
}

object SubUniverseType extends InferenceRule(subtypeOf.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
    case subtypeOf(t) => solver.inferType(t,covered)
    case _ => None
  }
}