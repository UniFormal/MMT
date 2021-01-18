package info.kwarc.mmt.api.modules.diagops

import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Module, ModuleOrLink}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, IncludeData}

class InParalleLinearTransformer(transformers: List[LinearTransformer]) extends LinearTransformer {
  type DiagramState = LinearDiagramState
  type LinearState = ThisState

  override def initDiagramState(toplevelModules: Map[MPath, Module], interp: DiagramInterpreter): DiagramState = {
    transformers.map(tx => tx.initDiagramState(toplevelModules, interp))
    new LinearDiagramState(toplevelModules)
  }

  class ThisState(val txStates: Array[MinimalLinearState], override val diagramState: DiagramState, override var inContainer: ModuleOrLink) extends MinimalLinearState {
    override def registerDeclaration(decl: Declaration): Unit = {}
    override val processedDeclarations: List[Declaration] = Nil

    override def registerSkippedDeclaration(decl: Declaration): Unit = {}
    override val skippedDeclarations: List[Declaration] = Nil

    override def inherit(other: ThisState): Unit = {}
  }

  // cannot concisely express return type in Scala's type system, hence Object
  // ideally, return type would be a sigma type: `Σ(tx: LinearTransformer). tx.LinearState`
  private def foreachTransformer[T](f: (LinearTransformer, Object) => T)(implicit state: LinearState): Unit = {
    transformers.zipWithIndex.foreach {
      case (tx, i) =>
        f(tx, state.txStates(i).asInstanceOf[tx.LinearState])
    }
  }

  override def initLinearState(diagramState: DiagramState, inContainer: Container): LinearState = {
    val txStates = transformers.map(tx => {
      tx.initLinearState(diagramState.asInstanceOf[tx.DiagramState], inContainer)
    }).map(_.asInstanceOf[MinimalLinearState]).toArray

    new ThisState(txStates, diagramState, inContainer)
  }

  override def beginContainer(inContainer: Container, state: LinearState)(implicit interp: DiagramInterpreter): Boolean = {
    foreachTransformer((tx, txState) => {
      tx.beginContainer(inContainer, txState.asInstanceOf[tx.LinearState])
    })(state)
    true
  }

  override def endContainer(inContainer: Container, state: ThisState)(implicit interp: DiagramInterpreter): Unit = {
    foreachTransformer((tx, txState) => {
      tx.endContainer(inContainer, txState.asInstanceOf[tx.LinearState])
    })(state)
  }

  override def applyDeclaration(decl: Declaration, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = {
    foreachTransformer((tx, txState) => {
      tx.applyDeclaration(decl, container)(txState.asInstanceOf[tx.LinearState], interp)
    })(state)
  }

  override def applyConstant(c: Constant, container: Container)(implicit state: ThisState, interp: DiagramInterpreter): Unit = { require(requirement = false, "unreachable") }

  override def applyIncludeData(include: IncludeData, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = { require(requirement = false, "unreachable") }
}
