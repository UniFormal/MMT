package info.kwarc.mmt.api.modules.diagops
/*

import info.kwarc.mmt.api.{DPath, LocalName}
import info.kwarc.mmt.api.modules.{DiagramInterpreter, ModuleOrLink, Theory}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, IncludeData}
import info.kwarc.mmt.api.utils.URI

class InParallelTransformer(transformers: List[LinearTransformer]) extends LinearTransformer {

  override protected def applyModuleName(name: LocalName): LocalName = ???

  override protected type LinearState = ThisState

  class ThisState(val txStates: Array[MinimalLinearState], override val diagramState: DiagramState, override var inContainer: ModuleOrLink) extends MinimalLinearState {
    override def outContainer: Container = dummyOutContainer
    override def outContainer_=(m: Container): Unit = {}

    override def registerDeclaration(decl: Declaration): Unit = {}
    override val processedDeclarations: List[Declaration] = Nil

    override def registerSkippedDeclaration(decl: Declaration): Unit = {}
    override val skippedDeclarations: List[Declaration] = Nil

    override def inherit(other: ThisState): Unit = {}
  }

  private val dummyOutContainer: Container = Theory.empty(
    DPath(URI("InParallelTransformer:/")),
    LocalName("dummyContainer"),
    mt = None
  )

  override protected def initLinearState(diagramState: LinearDiagramState, inContainer: Container): LinearState = {
    val txStates = transformers.zipWithIndex.map {
      case (tx, i) => tx.initLinearState(diagramState.asInstanceOf[tx.DiagramState], inContainer)
    }.map(_.asInstanceOf[MinimalLinearState]).toArray

    new ThisState(txStates, diagramState, inContainer)
  }

  override protected def beginContainer(inContainer: Container, state: LinearState)(implicit interp: DiagramInterpreter): Option[Container] = {
    transformers.zipWithIndex.foreach {
      case (tx, i) =>
        tx.beginContainer(inContainer, state.txStates(i).asInstanceOf[tx.LinearState])
    }
    Some(dummyOutContainer)
  }

  override protected def endContainer(inContainer: Container, state: ThisState)(implicit interp: DiagramInterpreter): Unit = {}

  override protected def applyDeclaration(decl: Declaration, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = {
    transformers.zipWithIndex.foreach {
      case (tx, i) =>
        tx.applyDeclaration(decl, container)(state.txStates(i).asInstanceOf[tx.LinearState], interp)
    }
  }

  override protected def applyConstant(c: Constant, container: Container)(implicit state: ThisState, interp: DiagramInterpreter): Unit = { require(requirement = false, "unreachable") }

  override protected def applyIncludeData(include: IncludeData, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = { require(requirement = false, "unreachable") }
}
*/