package info.kwarc.mmt.api.modules.diagops

import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Module, ModuleOrLink}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, IncludeData}

import scala.collection.mutable

class InParalleLinearTransformer(transformers: List[LinearTransformer]) extends LinearTransformer {
  type DiagramState = TxAggregationDiagramState
  type LinearState = LinearTxAggregationState

  override def initDiagramState(toplevelModules: Map[MPath, Module], interp: DiagramInterpreter): DiagramState = {
    val txDiagStates = transformers
      .map(tx => {
        val diagState = tx.initDiagramState(toplevelModules, interp)
        diagState.asInstanceOf[LinearTransformer#LinearDiagramState]
      })
      .toArray
    new TxAggregationDiagramState(toplevelModules, txDiagStates)
  }

  class TxAggregationDiagramState(inputToplevelModules: Map[MPath, Module], val txDiagStates: Array[LinearTransformer#LinearDiagramState]) extends LinearDiagramState(inputToplevelModules) {
    override def initAndRegisterNewLinearState(inContainer: Container): LinearTxAggregationState = {
      txDiagStates.foreach(txDiagState =>
        txDiagState.initAndRegisterNewLinearState(inContainer)
      )
      InParalleLinearTransformer.this.initLinearState(this, inContainer)
    }
  }

  class LinearTxAggregationState(
                                  val txStates: Array[MinimalLinearState],
                                  override val diagramState: DiagramState,
                                  override var inContainer: ModuleOrLink
                                ) extends MinimalLinearState {

    var applicableStates: mutable.Set[Int] = mutable.HashSet()

    override def registerDeclaration(decl: Declaration): Unit = {
      txStates.foreach(_.registerDeclaration(decl))
    }
    override def registerSkippedDeclaration(decl: Declaration): Unit = {
      txStates.foreach(_.registerSkippedDeclaration(decl))
    }

    override def inherit(other: LinearTxAggregationState): Unit = {
      txStates.zip(other.txStates).foreach {
        case (txState, txStateToInherit) =>
          txState.inherit(txStateToInherit.asInstanceOf[LinearState])
      }
    }

    // unused
    override val processedDeclarations: List[Declaration] = Nil
    override val skippedDeclarations: List[Declaration] = Nil
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
  private def foreachTransformer[T](f: (LinearTransformer, LinearTransformer#LinearState, Int) => T)(implicit state: LinearState): Unit = {
    transformers.zipWithIndex.foreach {
      case (tx, i) =>
        f(tx, state.txStates(i).asInstanceOf[tx.LinearState], i)
    }
  }

  override def beginContainer(inContainer: Container, state: LinearState)(implicit interp: DiagramInterpreter): Boolean = {
    foreachTransformer((tx, txState, i) => {
      if (tx.beginContainer(inContainer, txState.asInstanceOf[tx.LinearState])) {
          state.applicableStates += i
        }
    })(state)

    true
  }

  override def endContainer(inContainer: Container, state: LinearTxAggregationState)(implicit interp: DiagramInterpreter): Unit = {
    foreachTransformer((tx, txState, i) => {
      if (state.applicableStates.contains(i)) {
        tx.endContainer(inContainer, txState.asInstanceOf[tx.LinearState])
      }
    })(state)
  }

  override def applyDeclaration(decl: Declaration, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = {
    foreachTransformer((tx, txState, i) => {
      if (state.applicableStates.contains(i)) {
        tx.applyDeclaration(decl, container)(txState.asInstanceOf[tx.LinearState], interp)
      }
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
