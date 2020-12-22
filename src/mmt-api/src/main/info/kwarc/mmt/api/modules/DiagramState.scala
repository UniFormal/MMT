package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api.objects.Context
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api._

import scala.collection.mutable

/**
  * State declarations and factory methods for [[FunctorialOperator]]s.
  *
  * The idea is that a (subclass of) [[FunctorialOperator]] should be able to keep track of some state while
  * all modules in the input diagram are being processed.
  * Moreover, that state should be subject to type safety. Hence, this trait declares a type field
  * ''DiagramState'' that subclasses of [[FunctorialOperator]] may narrow to (a more enriched) state class.
  */
trait FunctorialOperatorState {
  /**
    * The type of diagram states.
    *
    * Most likely, in your subclass of [[FunctorialOperator]] you want to narrow down this type field
    * to a class larger than [[MinimalFunctorialOpState]].
    *
    * Since we're speaking of state, that class may very well carry mutable state. It is encouraged to do so.
    */
  protected type DiagramState <: MinimalFunctorialOpState

  /**
    * Factory method to initialize a [[DiagramState]].
    *
    * @param diagram The full input diagram expression.
    * @param toplevelModules The extracted modules in the input diagram
    */
  def initDiagramState(toplevelModules: Map[MPath, Module], interp: DiagramInterpreter): DiagramState

  protected trait MinimalFunctorialOpState {
    val inputToplevelModules: Map[MPath, Module]

    /**
      * All the processed modules so far: a map from the [[MPath]] of the input [[Module]] to the
      * generated output [[Module]].
      */
    val processedElements: mutable.Map[Path, ContentElement] = mutable.Map()

    // todo: why not def?
    val seenModules: mutable.Set[MPath] = mutable.Set(inputToplevelModules.keys.toSeq : _*)
  }
}

/**
  * State declarations and factory methods for [[LinearOperator]]s.
  *
  * Akin to [[FunctorialOperatorState]], a linear operator should also be able to carry state. However, since a linear
  * operator works declaration-by-declaration, it should get a fine-granular state (a ''LinearState'').
  * Especially, a ''LinearState'' stores all declarations that have been processed so far -- from the POV
  * of a module and its transitively included modules.
  * In other words, from the POV of a linear operator, theories and views are flat, and so is ''LinearState''.
  */
trait LinearOperatorState extends FunctorialOperatorState {
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
  protected def initLinearState(diagramState: DiagramState, container: ContainerElement[Declaration]): LinearState

  protected trait MinimalLinearState {
    def diagramState: DiagramState
    def outerContext: Context
    def processedDeclarations: List[Declaration]
    def registerDeclaration(decl: Declaration): Unit

    def skippedDeclarations: List[Declaration]
    def registerSkippedDeclaration(decl: Declaration)

    final def skippedDeclarationPaths: List[GlobalName] = skippedDeclarations.map(_.path)

    /**
      * Inherits another linear state into this.
      *
      * For example [[LinearOperator]] calls this method when it comes across an inclusion of a theory with
      * ''other'' being the linear state (as previously computed and acted upon) for the included theory.
      */
    def inherit(other: LinearState): Unit
  }

  final override type DiagramState = LinearDiagramState
  final override def initDiagramState(toplevelModules: Map[MPath, Module], interp: DiagramInterpreter): DiagramState = new LinearDiagramState(toplevelModules)

  final protected class LinearDiagramState(val inputToplevelModules: Map[MPath, Module]) extends MinimalFunctorialOpState {
    // todo: actually name ContainerState
    val linearStates: mutable.Map[Path, LinearState] = mutable.Map()

    def initAndRegisterNewLinearState(container: ContainerElement[Declaration]): LinearState = {
      if (linearStates.contains(container.path)) {
        throw ImplementationError("Tried to initialize linear state twice, this must be a bug in the diag ops framework.")
      }

      val linearState = LinearOperatorState.this.initLinearState(this, container)
      linearStates.put(container.path, linearState)

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
  * Some generally useful [[LinearOperatorState]] for [[LinearOperator]]s.
  *
  * It provides a state with methods to register processed and skipped declarations and keeps track of them
  * as flat lists buffers.
  */
trait DefaultLinearStateOperator extends LinearOperatorState {
  final override type LinearState = SkippedDeclsExtendedLinearState

  /**
    * A linear state keeping track of processed and skipped declarations.
    *
    * @param outerContext The context containing a single reference to the current [[Module]].
    *                     (This being a thing is a bit of a leaking abstraction since linear operators
    *                     should see everything as being flat.)
    */
  class SkippedDeclsExtendedLinearState(override val diagramState: DiagramState, override val outerContext: Context) extends MinimalLinearState {
    final var _processedDeclarations: mutable.ListBuffer[Declaration] = mutable.ListBuffer()
    final override def processedDeclarations: List[Declaration] = _processedDeclarations.toList
    final override def registerDeclaration(decl: Declaration): Unit = _processedDeclarations += decl

    final val _skippedDeclarations: mutable.ListBuffer[Declaration] = mutable.ListBuffer()
    final override def skippedDeclarations: List[Declaration] = _skippedDeclarations.toList
    final override def registerSkippedDeclaration(decl: Declaration): Unit = _skippedDeclarations += decl

    final override def inherit(other: SkippedDeclsExtendedLinearState): Unit = {
      _processedDeclarations ++= other.processedDeclarations
      _skippedDeclarations ++= other.skippedDeclarations
    }
  }

  final override protected def initLinearState(diagramState: DiagramState, container: ContainerElement[Declaration]): LinearState = container.path match {
    case p: ContentPath => new SkippedDeclsExtendedLinearState(diagramState, Context(p.toMPath))
    case _ => throw ImplementationError("Diag Op framework: Can only initialize linear state for containers who have an MPath")
  }
}
