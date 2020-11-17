package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api.{ContentPath, GeneralError, MPath}
import info.kwarc.mmt.api.objects.{Context, Term}
import info.kwarc.mmt.api.symbols.Declaration

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
    * @param modules The extracted modules in the input diagram
    */
  def initDiagramState(diagram: Term, modules: Map[MPath, Module], interp: DiagramInterpreter): DiagramState

  protected trait MinimalFunctorialOpState {
    val inputModules: Map[MPath, Module]

    /**
      * All the processed modules so far: a map from the [[MPath]] of the input [[Module]] to the
      * generated output [[Module]].
      */
    val processedModules: mutable.Map[MPath, Module] = mutable.Map()
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
    * call [[LinearDiagramState.initAndRegisterNewLinearSate()]].
    *
    * @param diagramState The current diagram state
    * @param module The module for which the linear state ought to be created
    */
  protected def initLinearState(diagramState: DiagramState, module: Module): LinearState

  protected trait MinimalLinearState {
    def outerContext: Context
    def processedDeclarations: List[Declaration]
    def registerDeclaration(decl: Declaration): Unit

    /**
      * Inherits another linear state into this.
      *
      * For example [[LinearOperator]] calls this method when it comes across an inclusion of a theory with
      * ''other'' being the linear state (as previously computed and acted upon) for the included theory.
      */
    def inherit(other: LinearState): Unit
  }

  final override type DiagramState = LinearDiagramState
  final override def initDiagramState(diagram: Term, modules: Map[MPath, Module], interp: DiagramInterpreter): LinearDiagramState = new LinearDiagramState(modules)

  final protected class LinearDiagramState(val inputModules: Map[MPath, Module]) extends MinimalFunctorialOpState {
    val linearStates: mutable.Map[MPath, LinearState] = mutable.Map()

    def initAndRegisterNewLinearSate(module: Module): LinearState = {
      linearStates.getOrElseUpdate(module.path, LinearOperatorState.this.initLinearState(this, module))
    }

    def getLinearState(modulePath: MPath): LinearState = {
      linearStates.getOrElseUpdate(modulePath, throw GeneralError(
        s"No linear state known for $modulePath yet. Did the diag op framework ignore inclusion order and processed " +
          s" a module too early before another one?"
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
  class SkippedDeclsExtendedLinearState(override val outerContext: Context) extends MinimalLinearState {
    final var _processedDeclarations: mutable.ListBuffer[Declaration] = mutable.ListBuffer()
    final override def processedDeclarations: List[Declaration] = _processedDeclarations.toList
    final override def registerDeclaration(decl: Declaration): Unit = _processedDeclarations += decl

    final val _skippedDeclarations: mutable.ListBuffer[Declaration] = mutable.ListBuffer()
    final def skippedDeclarations: List[Declaration] = _skippedDeclarations.toList
    final def registerSkippedDeclaration(decl: Declaration): Unit = _skippedDeclarations += decl

    final def skippedDeclarationPaths: List[ContentPath] = skippedDeclarations.map(_.path)

    final override def inherit(other: SkippedDeclsExtendedLinearState): Unit = {
      _processedDeclarations ++= other.processedDeclarations
      _skippedDeclarations ++= other.skippedDeclarations
    }
  }

  final override protected def initLinearState(diagramState: DiagramState, module: Module): LinearState = {
    new SkippedDeclsExtendedLinearState(Context(module.path))
  }
}
