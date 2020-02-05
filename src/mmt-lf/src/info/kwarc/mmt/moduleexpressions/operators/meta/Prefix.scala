package info.kwarc.mmt.moduleexpressions.operators.meta

import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.lf.Strings
import info.kwarc.mmt.moduleexpressions.operators.Combinators

object DiagramPrefix extends BinaryConstantScala(Combinators._path, "diagram_prefix")

object ComputePrefixedDiagram extends ComputationRule(DiagramPrefix.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = tm match {
    case DiagramPrefix(diagTerm, Strings(prefix)) =>
      Blah.doIt(solver, List(diagTerm), {
        case List(diag) =>
          val prefixedDiagram = diag.relabel(_.prefixOrCreateLastSimpleStep(prefix))
          Simplify(prefixedDiagram.toTerm)
      })

    case DiagramPrefix(_, _) => RecurseOnly(List(2))
  }
}
