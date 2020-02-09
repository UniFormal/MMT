package info.kwarc.mmt.moduleexpressions.operators.meta

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.lf.Strings
import info.kwarc.mmt.moduleexpressions.operators.{Combinators, Common}

object DiagramDifferenceByDiagram extends BinaryConstantScala(Combinators._path, "diagram_difference_by_diagram")

object DiagramDifferenceByLabel extends BinaryConstantScala(Combinators._path, "diagram_difference_by_label")

object Blah {
  def doIt(solver: CheckingCallback, terms: Seq[Term], fun: List[AnonymousDiagram] => Simplifiability)(implicit stack: Stack, history: History): Simplifiability = {
    val potentialDiags = terms.map(Common.asAnonymousDiagram(solver, _)).toList

    val noneIndices = potentialDiags.zipWithIndex.collect { case (None, idx) => idx }
    if (noneIndices.nonEmpty) {
      RecurseOnly(noneIndices)
    } else {
      fun(potentialDiags.map(_.get))
    }
  }
}

object ComputeDiagramDifferenceByDiagram extends ComputationRule(DiagramDifferenceByDiagram.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = tm match {
    case DiagramDifferenceByDiagram(diagTerm, diagToSubtractTerm) =>
      Blah.doIt(solver, List(diagTerm, diagToSubtractTerm), {
        case List(diag, diagToSubtract) =>
          Simplify((diag - diagToSubtract).toTerm)
      })
  }
}

object ComputeDiagramDifferenceByLabel extends ComputationRule(DiagramDifferenceByLabel.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = tm match {
    case DiagramDifferenceByLabel(diagTerm, Strings(label)) =>
      Blah.doIt(solver, List(diagTerm), {
        case List(diag) => Simplify((diag - LocalName(label)).toTerm)
      })

    case DiagramDifferenceByLabel(_, _) => RecurseOnly(List(2))
  }
}