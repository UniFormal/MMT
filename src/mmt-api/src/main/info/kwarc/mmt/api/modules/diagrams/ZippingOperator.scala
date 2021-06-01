package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.ContentPath
import info.kwarc.mmt.api.symbols.{Constant, Declaration, IncludeData}

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
class ZippingOperator(transformers: List[LinearOperator]) extends LinearOperator {
  override def applyDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Option[Diagram] = {
    if (beginDiagram(diag)) {
      diag.modules.map(interp.ctrl.getModule).foreach(applyContainer)

      // TODO: hacky workaround here by Navid:
      //   to collect all output diagrams, we employ the hack to call applyDiagram
      //   on every transformer. This assumes that things are not recomputed, otherwise we're
      //   pretty inefficient
      val outDiagram = Diagram.union(transformers.flatMap(_.applyDiagram(diag)))(interp.ctrl.library)

      endDiagram(diag)
      Some(outDiagram)
    } else {
      None
    }
  }

  override def beginDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Boolean =
    transformers.forall(_.beginDiagram(diag))

  override def endDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Unit =
    transformers.foreach(_.endDiagram(diag))

  override def initState(container: Container): Unit =
    transformers.foreach(_.initState(container))

  override def inheritState(into: ContentPath, from: ContentPath): Unit =
    transformers.foreach(_.inheritState(into, from))

  override def registerSeenDeclaration(d: Declaration): Unit =
    transformers.foreach(_.registerSeenDeclaration(d))

  override def registerSkippedDeclarations(d: Declaration): Unit =
    transformers.foreach(_.registerSkippedDeclarations(d))

  override def beginContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Boolean =
    transformers.forall(_.beginContainer(inContainer))

  override def endContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Unit =
    transformers.foreach(_.endContainer(inContainer))

  override def applyDeclaration(decl: Declaration, container: Container)(implicit interp: DiagramInterpreter): Unit =
    transformers.foreach(_.applyDeclaration(decl, container))

  override def applyConstant(c: Constant, container: Container)(implicit interp: DiagramInterpreter): Unit =
    require(requirement = false, "unreachable")

  override def applyIncludeData(include: IncludeData, container: Container)(implicit interp: DiagramInterpreter): Unit =
    require(requirement = false, "unreachable")
}
