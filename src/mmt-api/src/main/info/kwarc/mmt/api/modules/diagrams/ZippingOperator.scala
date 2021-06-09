package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.ContentPath
import info.kwarc.mmt.api.libraries.Library
import info.kwarc.mmt.api.symbols.{Constant, Declaration, IncludeData, Structure}

/**
  * Transforms a diagram by multiple linear transformers in parallel,
  * which are all run step-by-step.
  *
  * Upon a declaration `d`, first all transformers are run on `d`, and only
  * then is the next declaration considered.
  *
  * This is useful when linear transformers are interdependent, as is the case
  * in [[PushoutOperator]], for example.
  *
  * TODO (WARNING): the input diagram must be ordered by dependency already due to implementation details.
  */
class ZippingOperator(operators: List[LinearOperator], focussedOp: Option[Integer] = None) extends LinearOperator {
  require(focussedOp.forall(idx => 0 <= idx && idx < operators.size))

  // Overriding prevents nested zipping operators, which is what users expect when using :: as notation
  // E.g., this makes `(op1 :: op2 :: op3).withFocus(2)` well-behaved and what the user expects (op3 being
  // focussed).
  override def ::(first: LinearOperator): ZippingOperator = {
    new ZippingOperator(first :: operators, focussedOp.map(_ + 1))
  }

  def withFocus(newFocus: Integer): ZippingOperator = {
    if (newFocus >= 0) {
      require(0 <= newFocus && newFocus < operators.size, "new focus out of bounds")
      new ZippingOperator(operators, Some(newFocus))
    } else {
      require(newFocus >= -operators.size, "new focus out of bounds")
      withFocus(operators.size + newFocus)
    }
  }

  override def applyDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Option[Diagram] = {
    implicit val library: Library = interp.ctrl.library

    if (beginDiagram(diag)) {
      diag.modules.map(interp.ctrl.getModule).foreach(applyContainer)
      endDiagram(diag)

      // TODO: hacky workaround here by Navid:
      //   to collect all output diagrams, we employ the hack to call applyDiagram
      //   on every operator. This assumes that things are not recomputed, otherwise we're
      //   pretty inefficient
      val outDiagram = focussedOp match {
        case Some(focussedOp) => operators(focussedOp).applyDiagram(diag).get
        case _ => Diagram.union(operators.flatMap(_.applyDiagram(diag)))
      }

      Some(outDiagram)
    } else {
      None
    }
  }

  override def beginDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Boolean =
    operators.forall(_.beginDiagram(diag))

  override def endDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Unit =
    operators.foreach(_.endDiagram(diag))

  override def initState(container: Container): Unit =
    operators.foreach(_.initState(container))

  override def inheritState(into: ContentPath, from: ContentPath): Unit =
    operators.foreach(_.inheritState(into, from))

  override def registerSeenDeclaration(d: Declaration): Unit =
    operators.foreach(_.registerSeenDeclaration(d))

  override def registerSkippedDeclarations(d: Declaration): Unit =
    operators.foreach(_.registerSkippedDeclarations(d))

  override def beginContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Boolean =
    operators.forall(_.beginContainer(inContainer))

  override def endContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Unit =
    operators.foreach(_.endContainer(inContainer))

  override def applyDeclaration(decl: Declaration, container: Container)(implicit interp: DiagramInterpreter): Unit =
    operators.foreach(_.applyDeclaration(decl, container))

  override def applyConstant(c: Constant, container: Container)(implicit interp: DiagramInterpreter): Unit =
    require(requirement = false, "unreachable")

  override def applyIncludeData(include: IncludeData, structure: Structure, container: Container)(implicit interp: DiagramInterpreter): Unit =
    require(requirement = false, "unreachable")
}
