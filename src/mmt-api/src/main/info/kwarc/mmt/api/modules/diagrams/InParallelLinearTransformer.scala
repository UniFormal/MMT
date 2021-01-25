package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.modules.ModuleOrLink
import info.kwarc.mmt.api.symbols.{Constant, Declaration, IncludeData}

import scala.collection.mutable

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
class InParallelLinearTransformer(transformers: List[LinearTransformer]) extends LinearTransformer {
  type DiagramState = TxAggregationDiagramState
  type LinearState = LinearTxAggregationState

  override def initDiagramState(diag: Diagram, interp: DiagramInterpreter): DiagramState = {
    val txDiagStates = transformers
      .map(tx => {
        val diagState = tx.initDiagramState(diag, interp)
        diagState.asInstanceOf[LinearTransformer#LinearDiagramState]
      })
      .toArray
    new TxAggregationDiagramState(diag, txDiagStates)
  }

  class TxAggregationDiagramState(diag: Diagram, val txDiagStates: Array[LinearTransformer#LinearDiagramState]) extends LinearDiagramState(diag)

  class LinearTxAggregationState(
                                  val txStates: Array[MinimalLinearState],
                                  override val diagramState: DiagramState,
                                  override var inContainer: ModuleOrLink
                                ) extends MinimalLinearState {

    /**
      * The [[transformers]] (by index) which signaled applicability
      * on [[inContainer]] by their [[LinearTransformer.beginContainer()]] function
      * having returned true.
      */
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
    val txStates = diagramState.txDiagStates
      .toList // without this, scalac complains about "missing class tag"
      .map(_.initAndRegisterNewLinearState(inContainer))
      .map(_.asInstanceOf[MinimalLinearState])
      .toArray

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
    foreachTransformer((tx, txState_, i) => {
      if (state.applicableStates.contains(i)) {
        tx.applyDeclaration(decl, container)(txState_.asInstanceOf[tx.LinearState], interp)
      }
    })(state)
  }

  override def applyConstant(c: Constant, container: Container)(implicit state: LinearTxAggregationState, interp: DiagramInterpreter): Unit = { require(requirement = false, "unreachable") }

  override def applyIncludeData(include: IncludeData, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = { require(requirement = false, "unreachable") }

  override def beginDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Boolean = {
    transformers.forall(_.beginDiagram(diag))
  }

  override def endDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Unit = {
    transformers.foreach(_.endDiagram(diag))
  }

  override def applyDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Option[Diagram] = {
    val state = initDiagramState(diag, interp)

    if (beginDiagram(diag)) {
      diag.modules.map(interp.ctrl.getModule).foreach(module => {
        applyContainer(module)(state, interp)
      })

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
}
