package info.kwarc.mmt.moduleexpressions.operators.meta

import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.uom.Simplifiability.NoRecurse
import info.kwarc.mmt.moduleexpressions.operators.{Combinators, Common}

object DiagramUnion extends FlexaryConstantScala(Combinators._path, "diagram_union")

object ComputeDiagramUnion extends ComputationRule(DiagramUnion.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = tm match {
    case DiagramUnion(terms @ _*) if terms.nonEmpty =>
      Blah.doIt(solver, terms, diags => {
        AnonymousDiagram.union(diags : _*) match {
          case Some(unionDiag) => Simplify(unionDiag.toTerm)
          case _ =>
            solver.error("Refusing to union given diagrams due to name clashes")
            NoRecurse
        }
      })

    case DiagramUnion() =>
      NoRecurse
  }
}