package info.kwarc.mmt

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.modules.diagops._
import info.kwarc.mmt.api.objects.{Context, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.uom.{SimplificationUnit, Simplifier}
import info.kwarc.mmt.api.utils.UnicodeStrings

import scala.collection.mutable
/*
final class LogicalRelationPushoutTransformer(initialLogrel: Term, mors: List[Term], commonLinkDomain: MPath, commonLinkCodomain: MPath) extends SimpleConstantsBasedModuleTransformer with OperatorDSL {

  override val operatorDomain: MPath = commonLinkDomain
  override val operatorCodomain: MPath = commonLinkCodomain

  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_logrel_pushed")

  override type LinearState = State
  protected class State(diagramState: DiagramState, inContainer: ModuleOrLink) extends SkippedDeclsExtendedLinearState(diagramState, inContainer) {

    val logrel: mutable.Map[GlobalName, Term] = ??? // initialLogrel

  }

  val copyRenamers: Array[Renamer[LinearState]] = mors.indices.map(i =>
    getRenamerFor(UnicodeStrings.subscriptInteger(i))
  ).toArray
  val related: Renamer[LinearState] = getRenamerFor("ᵀ")

  private def homomorphicallyExtendLogrel(logrel: Term => Term, state: LinearState): Term => Term = {
    t => {
      ???
    }
  }

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    // homomorphically extended logrel
    val logrel = (p: GlobalName) => {

    }


    val logicalRelation = new LogicalRelation(
      mors,
      p => interp.ctrl.globalLookup.ApplyMorphs(???, OMS(p)),
      interp.ctrl.globalLookup
    )
    def g(t: Term): Term = betaReduce(Context.empty, logicalRelation.getExpected(Context.empty, c.toTerm, t), interp.ctrl.simplifier)

    val lookup = interp.ctrl.globalLookup

    // todo: also map definienses
    val copies = mors.zip(copyRenamers).map {
      case (mor, r) => const(
        r(c.path),
        lookup.ApplyMorphs(mor, tp),
        df.map(lookup.ApplyMorphs(mor, _))
      )
    }
    val copiesRelated = const(
      related(c.path),
      logicalRelation(???, ???),
      None
    )

    copies :+ copiesRelated
  }

  private def betaReduce(ctx: Context, t: Term, simplifier: Simplifier): Term = simplifier(
    t,
    SimplificationUnit(ctx, expandDefinitions = false, fullRecursion = true),
    RuleSet(lf.Beta)
  )

  override type DiagramState = this.type

  /**
    * Side effect-free (!) factory method to initialize a [[LinearState]].
    *
    * If you want to initialize a new linear state *and* register it to some [[DiagramState]], then
    * call [[LinearDiagramState.initAndRegisterNewLinearState()]].
    *
    * @param diagramState The current diagram state
    * @param container    The container for which the linear state ought to be created
    */
  override def initLinearState(diagramState: LogicalRelationPushoutTransformer.this.type, inContainer: Container): State = ???

  /**
    * Factory method to initialize a [[DiagramState]].
    *
    * @param diagram         The full input diagram expression.
    * @param toplevelModules The extracted modules in the input diagram
    */
  override def initDiagramState(toplevelModules: Map[MPath, Module], interp: DiagramInterpreter): LogicalRelationPushoutTransformer.this.type = ???
}

object LogicalRelationPushoutOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?logrel_pushout_operator")

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearModuleTransformer] = {
    parameters match {
      case OMMOD(domain) +: OMMOD(codomain) +: mors :+ OMMOD(logrel) =>
        Some(new LogicalRelationPushoutTransformer(OMMOD(logrel), mors, domain, codomain))

      case _ =>
        None
    }
  }
}*/