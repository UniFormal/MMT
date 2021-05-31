package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.{Link, Module, ModuleOrLink, Theory}
import info.kwarc.mmt.api.objects.Context
import info.kwarc.mmt.api.symbols.Declaration

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait DiagramTransformerState {
  /**
    * The type of diagram states.
    *
    * Most likely, in your subclass of [[ModuleOperator]] you want to narrow down this type field
    * to a class larger than [[MinimalFunctorialOpState]].
    *
    * Since we're speaking of state, that class may very well carry mutable state. It is encouraged to do so.
    */
  protected type DiagramState <: MinimalDiagramState


  /**
    * Factory method to initialize a [[DiagramState]].
    *
    * @param diagram The full input diagram expression.
    * @param toplevelModules The extracted modules in the input diagram
    */
  def initDiagramState(diag: Diagram, interp: DiagramInterpreter): DiagramState

  protected trait MinimalDiagramState {
    def inputDiagram: Diagram
  }
}

/**
  * State declarations and factory methods for [[ModuleOperator]]s.
  *
  * The idea is that a (subclass of) [[ModuleOperator]] should be able to keep track of some state while
  * all modules in the input diagram are being processed.
  * Moreover, that state should be subject to type safety. Hence, this trait declares a type field
  * ''DiagramState'' that subclasses of [[ModuleOperator]] may narrow to (a more enriched) state class.
  */
trait ModuleTransformerState extends DiagramTransformerState {
  /**
    * The type of diagram states.
    *
    * Most likely, in your subclass of [[ModuleOperator]] you want to narrow down this type field
    * to a class larger than [[MinimalFunctorialOpState]].
    *
    * Since we're speaking of state, that class may very well carry mutable state. It is encouraged to do so.
    */
  protected type DiagramState <: MinimalModuleState

  protected trait MinimalModuleState extends MinimalDiagramState {
    /**
      * All the processed modules so far: a map from the [[MPath]] of the input [[Module]] to the
      * generated output [[Module]].
      */
    val processedElements: mutable.Map[Path, ContentElement] = mutable.Map()

    @deprecated("idk what the use is or should be")
    val seenModules: mutable.Set[MPath] = mutable.Set(inputDiagram.modules : _*)
  }
}

trait LinearTransformerState extends DiagramTransformerState {
  protected type LinearState <: MinimalLinearState

  /**
    * Side effect-free (!) factory method to initialize a [[LinearState]].
    *
    * If you want to initialize a new linear state *and* register it to some [[DiagramState]], then
    * call [[LinearDiagramState.initAndRegisterNewLinearState()]].
    *
    * @param diagramState The current diagram state
    * @param container The container for which the linear state ought to be created
    */
  def initLinearState(diagramState: DiagramState, inContainer: ModuleOrLink): LinearState

  protected trait MinimalLinearState {
    def diagramState: DiagramState

    def inContainer: ModuleOrLink
    def inContainer_=(m: ModuleOrLink): Unit

    def processedDeclarations: Seq[Declaration]
    def registerDeclaration(decl: Declaration): Unit

    def skippedDeclarations: Seq[Declaration]
    def registerSkippedDeclaration(decl: Declaration): Unit

    final def skippedDeclarationPaths: Seq[GlobalName] = skippedDeclarations.map(_.path)

    /**
      * Declarations either processed or skipped.
      */
    final def seenDeclarations: Seq[Declaration] = processedDeclarations ++ skippedDeclarations

    /**
      * Inherits another linear state into this.
      *
      * For example [[LinearOperator]] calls this method when it comes across an inclusion of a theory with
      * ''other'' being the linear state (as previously computed and acted upon) for the included theory.
      */
    def inherit(other: LinearState): Unit
  }

  type DiagramState <: LinearDiagramState
  /*override def initDiagramState(toplevelModules: Map[MPath, Module], interp: DiagramInterpreter): DiagramState = new LinearDiagramState(toplevelModules)*/

  protected class LinearDiagramState(override val inputDiagram: Diagram) extends MinimalDiagramState {
    // todo: actually name ContainerState
    val linearStates: mutable.Map[Path, LinearState] = mutable.Map()

    def initAndRegisterNewLinearState(inContainer: ModuleOrLink): LinearState = {
      if (linearStates.contains(inContainer.path)) {
        throw ImplementationError("Tried to initialize linear state twice, this must be a bug in the diag ops framework.")
      }

      val linearState = LinearTransformerState.this.initLinearState(this.asInstanceOf[DiagramState], inContainer)
      linearStates.put(inContainer.path, linearState)

      linearState
    }

    def getLinearState(path: Path): LinearState = {
      linearStates.getOrElseUpdate(path, throw ImplementationError(
        s"No linear state known for $path yet. Did the diag op framework ignore inclusion order and processed, " +
          s" say a module, too early before another one?"
      ))
    }
  }
}

/**
  * State declarations and factory methods for [[LinearOperator]]s.
  *
  * Akin to [[ModuleOperatorState]], a linear operator should also be able to carry state. However, since a linear
  * operator works declaration-by-declaration, it should get a fine-granular state (a ''LinearState'').
  * Especially, a ''LinearState'' stores all declarations that have been processed so far -- from the POV
  * of a module and its transitively included modules.
  * In other words, from the POV of a linear operator, theories and views are flat, and so is ''LinearState''.
  */
trait LinearModuleTransformerState extends ModuleTransformerState with LinearTransformerState {
  type LinearState <: MinimalLinearModuleState
  type DiagramState <: MinimalLinearModuleDiagramState

  final protected class MinimalLinearModuleDiagramState(inputDiagram: Diagram) extends LinearDiagramState(inputDiagram) with MinimalModuleState {
  }

  /**
    * A linear state is bound to an `inContainer` and an `outContainer`,
    * storing all declarations processed so far (also transitively).
    */
  protected trait MinimalLinearModuleState extends MinimalLinearState {
    def inTheoryContext: MPath

    def outContainer: ModuleOrLink
    def outContainer_=(m: ModuleOrLink): Unit

    // todo: look up all usages of outContext, I (Navid) have the suspicion most are wrong
    def outContext: Context = Context(outContainer.modulePath)
  }

  /**
    * A linear state keeping track of processed and skipped declarations.
    */
  protected class SkippedDeclsExtendedLinearState(override val diagramState: DiagramState, override var inContainer: ModuleOrLink) extends MinimalLinearModuleState {
    override val inTheoryContext: MPath = inContainer match {
      case thy: Theory => thy.path
      case link: Link => link.to.toMPath
    }

    final var _processedDeclarations: ListBuffer[Declaration] = mutable.ListBuffer()

    final override def processedDeclarations: Seq[Declaration] = _processedDeclarations.toSeq

    final override def registerDeclaration(decl: Declaration): Unit = _processedDeclarations += decl

    final val _skippedDeclarations: ListBuffer[Declaration] = mutable.ListBuffer()

    final override def skippedDeclarations: Seq[Declaration] = _skippedDeclarations.toSeq

    final override def registerSkippedDeclaration(decl: Declaration): Unit = _skippedDeclarations += decl

    final override var outContainer: ModuleOrLink = _

    override def inherit(other: LinearState): Unit = {
      _processedDeclarations ++= other.processedDeclarations
      _skippedDeclarations ++= other.skippedDeclarations
    }
  }
}

// todo: better name
trait LackingLinearStateOperator extends LinearModuleTransformer {
  final override type DiagramState = MinimalLinearModuleDiagramState

  final override def initDiagramState(diag: Diagram, interp: DiagramInterpreter): DiagramState = new MinimalLinearModuleDiagramState(diag)
}

/**
  * Some generally useful [[LinearModuleOperatorState]] for [[LinearOperator]]s.
  *
  * It provides a state with methods to register processed and skipped declarations and keeps track of them
  * as flat lists buffers.
  */
trait DefaultLinearStateOperator extends LinearModuleTransformerState {
  final override type DiagramState = MinimalLinearModuleDiagramState
  final override type LinearState = SkippedDeclsExtendedLinearState

  final override def initLinearState(diagramState: DiagramState, inContainer: ModuleOrLink): LinearState = new SkippedDeclsExtendedLinearState(diagramState, inContainer)

  final override def initDiagramState(diag: Diagram, interp: DiagramInterpreter): DiagramState = new MinimalLinearModuleDiagramState(diag)
}
