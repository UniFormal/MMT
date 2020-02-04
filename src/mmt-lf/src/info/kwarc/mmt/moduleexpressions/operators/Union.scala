package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._

object DiagramUnion extends BinaryConstantScala(Combinators._path, "diagram_union")

object ComputeDiagramUnion extends ComputationRule(DiagramUnion.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = tm match {
    case DiagramUnion(tm1, tm2) =>
      (Common.asAnonymousDiagram(solver, tm1), Common.asAnonymousDiagram(solver, tm2)) match {
        case (Some(diag1), Some(diag2)) => Simplify(diag2.toTerm)
        case _ => RecurseOnly(List(1, 2))
      }

    case _ =>
      RecurseOnly(List(1, 2))
  }
}