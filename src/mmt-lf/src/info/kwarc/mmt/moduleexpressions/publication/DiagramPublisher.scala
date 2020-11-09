package info.kwarc.mmt.moduleexpressions.publication

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations.Marker
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.ConsoleWriter
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.uom.SimplificationUnit

import scala.collection.mutable

object DiagramPublisher {
  /**
    * @todo Eventually rename the structural feature for diagrams to "publish diagram"
    */
  val feature = "diagram"
}

/**
  * Module-level structural feature for publishing diagrams into the current document namespace.
  *
  * @example
  * '''
  * theory Empty = ❚
  * diagram Collection := ?Empty EXTEND { coll : type ⟶ type } ❚
  * '''
  *
  * It expects an [[AnonymousDiagramCombinator]] as its (normalized) definiens and then
  * publishes that diagram in the document namespace in which the structural feature itself is used.n
  */
class DiagramPublisher extends ModuleLevelFeature(DiagramPublisher.feature) {
  override def getHeaderNotation: List[Marker] = Nil
  /** */
  def check(dm: DerivedModule)(implicit env: ExtendedCheckingEnvironment): Unit = {}

  override def modules(dm: DerivedModule): List[Module] = {
    val df = dm.dfC.normalized.getOrElse(throw LocalError(s"diagram structural feature requires definiens (did you perhaps type = instead of :=?)"))

    val rules = RuleSet.collectRules(controller, dm.getInnerContext)

    // TODO: @Florian, the construction of this is probably wrong and doesn't make sense with the Inhabitable judgement
    val solver = new Solver(
      controller,
      CheckingUnit(Some(dm.path $ DefComponent),dm.getInnerContext, Context.empty, Inhabitable(Stack.empty, df)),
      rules
    )
    // todo: ask Florian how to get solver from controller
    val diagInterp = new DiagramInterpreter(dm.getInnerContext, controller, solver, rules)

    diagInterp(df) match {
      case Some(outputDiagram) =>
        diagInterp.committedModules.foreach(controller.presenter(_)(ConsoleWriter))
        println(s"\nAbove MMT surface syntax was printed by Scala class 'DiagramPublisher' for debugging reasons. Input diagram was: ${df}")

        dm.dfC.set(outputDiagram)
        diagInterp.committedModules

      case None =>
        throw GeneralError("Found diagram operator was partial on input diagram")
    }

  }
}