package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.{GeneralError, LocalName}

object DiagramAccessor extends BinaryConstantScala(Combinators._path, "diagram_accessor")

object ComputeDiagramAccess extends ComputationRule(DiagramAccessor.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val DiagramAccessor(diagramTerm, label) = tm
    val diagram = Common.asAnonymousDiagram(solver, diagramTerm).getOrElse(return RecurseOnly(List(1)))

    label match {
      case literal @ OMLIT(literalValue, RealizedType(_, StandardString)) =>
        // May either be a diagram node or diagram arrow
        val entityToExtract = LocalName(literal.valueString)

        lazy val foundNode = diagram.getNode(entityToExtract)
        lazy val foundArrow = diagram.getArrow(entityToExtract)

        val extractedDiagram = (foundNode, foundArrow) match {
          case (Some(node), None) => AnonymousDiagram(List(node), Nil, Some(node.label))
          case (None, Some(arrow)) => AnonymousDiagram(
            List(
              diagram.getNode(arrow.from).getOrElse(throw GeneralError("Got passed an invalid diagram where arrow's domain references a non-existing node.")),
              diagram.getNode(arrow.to).getOrElse(throw GeneralError("Got passed an invalid diagram where arrow's codomain references a non-existing node.")),
            ),
            List(arrow),
            None
          )
          case (Some(_), Some(_)) =>
            solver.error("Ambiguous DiagramAccess usage where label appears as label of a node and an arrow.")
            return Recurse
          case (None, None) =>
            solver.error("Neither node nor arrow found in diagram with given label.")
            return Recurse
        }

        Simplify(extractedDiagram.toTerm)

      case _ => Recurse
    }
  }
}