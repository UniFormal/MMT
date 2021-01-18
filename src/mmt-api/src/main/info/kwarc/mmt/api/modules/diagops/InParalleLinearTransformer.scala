package info.kwarc.mmt.api.modules.diagops

import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Module, ModuleOrLink}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, IncludeData}

class InParalleLinearTransformer(transformers: List[LinearTransformer]) extends LinearTransformer {
  type DiagramState = TxAggregationDiagramState
  type LinearState = LinearTxAggregationState

  override def initDiagramState(toplevelModules: Map[MPath, Module], interp: DiagramInterpreter): DiagramState = {
    val txDiagStates = transformers
      .map(tx => tx.initDiagramState(toplevelModules, interp).asInstanceOf[MinimalDiagramState])
      .toArray
    new TxAggregationDiagramState(toplevelModules, txDiagStates)
  }

  class TxAggregationDiagramState(inputToplevelModules: Map[MPath, Module], val txDiagStates: Array[MinimalDiagramState]) extends LinearDiagramState(inputToplevelModules)

  class LinearTxAggregationState(val txStates: Array[MinimalLinearState], override val diagramState: DiagramState, override var inContainer: ModuleOrLink) extends MinimalLinearState {
    override def registerDeclaration(decl: Declaration): Unit = {}
    override val processedDeclarations: List[Declaration] = Nil

    override def registerSkippedDeclaration(decl: Declaration): Unit = {}
    override val skippedDeclarations: List[Declaration] = Nil

    override def inherit(other: LinearTxAggregationState): Unit = {
      transformers.zip(txStates).zip(other.txStates).foreach {
        case ((tx, txState), txStateToInherit) =>
          txState.inherit(txStateToInherit.asInstanceOf[LinearState])
      }
    }
  }

  override def initLinearState(diagramState: DiagramState, inContainer: Container): LinearState = {
    val txStates = transformers.zip(diagramState.txDiagStates).map {
      case (tx, txDiagState) =>
        tx.initLinearState(txDiagState.asInstanceOf[tx.DiagramState], inContainer)
    }.map(_.asInstanceOf[MinimalLinearState]).toArray

    new LinearTxAggregationState(txStates, diagramState, inContainer)
  }

  // cannot concisely express return type in Scala's type system, hence LinearTransformer#LinearState
  // ideally, return type would be a sigma type: `Σ(tx: LinearTransformer). tx.LinearState`
  private def foreachTransformer[T](f: (LinearTransformer, LinearTransformer#LinearState) => T)(implicit state: LinearState): Unit = {
    transformers.zipWithIndex.foreach {
      case (tx, i) =>
        f(tx, state.txStates(i).asInstanceOf[tx.LinearState])
    }
  }

  override def beginContainer(inContainer: Container, state: LinearState)(implicit interp: DiagramInterpreter): Boolean = {
    foreachTransformer((tx, txState) => {
      tx.beginContainer(inContainer, txState.asInstanceOf[tx.LinearState])
    })(state)
    true
  }

  override def endContainer(inContainer: Container, state: LinearTxAggregationState)(implicit interp: DiagramInterpreter): Unit = {
    foreachTransformer((tx, txState) => {
      tx.endContainer(inContainer, txState.asInstanceOf[tx.LinearState])
    })(state)
  }

  override def applyDeclaration(decl: Declaration, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = {
    foreachTransformer((tx, txState) => {
      tx.applyDeclaration(decl, container)(txState.asInstanceOf[tx.LinearState], interp)
    })(state)
  }

  override def applyConstant(c: Constant, container: Container)(implicit state: LinearTxAggregationState, interp: DiagramInterpreter): Unit = { require(requirement = false, "unreachable") }

  override def applyIncludeData(include: IncludeData, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = { require(requirement = false, "unreachable") }

  override def applyDiagram(modulePaths: List[MPath])(implicit interp: DiagramInterpreter): List[MPath] = {
    val modules: Map[MPath, Module] = modulePaths.map(p => (p, interp.ctrl.getModule(p))).toMap
    val state = initDiagramState(modules, interp)

    modulePaths.map(interp.ctrl.getModule).foreach(module => {
      applyContainer(module)(state, interp)
    })

    transformers.flatMap(_.applyDiagram(modulePaths))
  }
}
