package info.kwarc.mmt

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.modules.diagrams._
import info.kwarc.mmt.api.objects.{Context, OMCOMP, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.{Constant, IncludeData, Structure}
import info.kwarc.mmt.api.uom.SimplificationUnit

sealed case class LogicalRelationPushoutType(initialLogrel: MPath, initialLogrelType: LogicalRelationType) {
  def createPushoutTransformers()(implicit interp: DiagramInterpreter): List[LinearTransformer] = initialLogrelType.mors.map(mor =>
    PushoutOperator.instantiate(List(
      OMMOD(initialLogrelType.commonLinkDomain),
      OMMOD(initialLogrelType.commonLinkCodomain),
      mor
    )).get
  )

  lazy val pushoutPathTransformers: List[ModulePathTransformer] = initialLogrelType.mors.map(mor =>
    new PushoutTransformer.PathTransformer(
      initialLogrelType.commonLinkDomain,
      initialLogrelType.commonLinkCodomain,
      mor
    )
  )

  lazy val pushoutInjectionTransformers: List[ModulePathTransformer] = initialLogrelType.mors.map(mor =>
    new PushoutConnector.PathTransformer(mor)
  )

  def getLogicalRelationTypeFor(m: MPath): LogicalRelationType = {
    val currentMors: List[Term] = pushoutPathTransformers.map(_.applyModulePath(m)).map(mor => {
      OMCOMP(OMMOD(mor), ???)
    })
    val currentMorsDomain: MPath = m
    val currentMorsCodomain: MPath =
      new LogicalRelationPushoutTransformer.PathTransformer(this).applyModulePath(m)

    LogicalRelationType(currentMors, currentMorsDomain, currentMorsCodomain)
  }

  def getLogicalRelationFor(m: MPath): Term = {
    OMMOD(new LogicalRelationPushoutConnector.PathTransformer(this).applyModulePath(m))
  }
}

object LogicalRelationPushoutOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?logrel_pushout_operator")

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearTransformer] = {
    parameters match {
      case OMMOD(domain) +: OMMOD(codomain) +: mors :+ OMMOD(logrel) =>
        val pushoutType = LogicalRelationPushoutType(logrel, LogicalRelationType(mors, domain, codomain))
        val pushoutTransformers = pushoutType.createPushoutTransformers()

        // order is important!
        val transformers = pushoutTransformers ::: List(
          new LogicalRelationTransformer(pushoutType.getLogicalRelationTypeFor),
          new LogicalRelationPushoutTransformer(pushoutType),
          new LogicalRelationPushoutConnector(pushoutType)
        )

        Some(new InParalleLinearTransformer(transformers))

      case _ =>
        None
    }
  }
}

final class LogicalRelationPushoutTransformer(pushoutType: LogicalRelationPushoutType)
  extends LogicalRelationPushoutTransformer.PathTransformer(pushoutType)
    with SimpleLinearModuleTransformer
    with OperatorDSL {

  private val pushoutTransformers = pushoutType.pushoutPathTransformers

  val pushoutStructures: Array[StructureHelper] = pushoutTransformers.zipWithIndex.map {
    case (tx, i) =>
      getStructureHelper(LocalName(s"pushout_$i"), thy => tx.applyModulePath(thy))
  }.toArray

  val related: Renamer[LinearState] = getRenamerFor("_T") // "ᵀ"

  override def beginTheory(thy: Theory, state: LinearState)(implicit interp: DiagramInterpreter): Option[Theory] = {
    super.beginTheory(thy, state).map(outTheory => {
      pushoutStructures.map(_.structure(thy.path)).foreach(s => {
        interp.add(s)
        interp.endAdd(s)
      })

      outTheory
    })
  }

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {

    val lookup = interp.ctrl.globalLookup

    // The expressions in container are expressions over the theory
    // with the following module path:
    val theoryContext: MPath = state.outContainer match {
      case thy: Theory => thy.path
      case link: Link => link.to.toMPath
    }

    val logrelType = pushoutType.getLogicalRelationTypeFor(theoryContext)


    val logicalRelation = new LogicalRelation(
      logrelType.mors,
      p => lookup.ApplyMorphs(pushoutType.getLogicalRelationFor(theoryContext), OMS(p)),
      lookup
    )

    def betaReduce(t: Term): Term = {
      interp.ctrl.simplifier(
        t,
        SimplificationUnit(Context.empty, expandDefinitions = false, fullRecursion = true),
        RuleSet(lf.Beta)
      )
    }

    val newTp = c.tp.map(logicalRelation.getExpected(Context.empty, OMS(c.path), _)).map(betaReduce)

    List(Constant(
      home = state.outContainer.toTerm,
      name = related(c.name),
      alias = Nil,
      tp = newTp,
      df = None,
      rl = None
    ))
  }
}

object LogicalRelationPushoutTransformer {
  class PathTransformer(pushoutType: LogicalRelationPushoutType) extends ModulePathTransformer with RelativeBaseTransformer {

    override val operatorDomain: Diagram = Diagram.singleton(pushoutType.initialLogrelType.commonLinkDomain)
    override val operatorCodomain: Diagram = Diagram.singleton(pushoutType.initialLogrelType.commonLinkCodomain)

    // TODO: encode morphism names into name here?
    def applyModuleName(name: LocalName): LocalName =
      name.suffixLastSimple("_logrel_pushout")
  }
}

private class LogicalRelationPushoutConnector(pushoutType: LogicalRelationPushoutType)
  extends LogicalRelationPushoutConnector.PathTransformer(pushoutType)
    with SimpleLinearConnectorTransformer
    with OperatorDSL {

  // TODO: encode morphism names into name here?
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_logrel_pushout")

  override val in = new LogicalRelationTransformer(pushoutType.getLogicalRelationTypeFor)
  override val out = new LogicalRelationPushoutTransformer.PathTransformer(pushoutType)

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    List(assgn(c.path, OMS(out.applyModulePath(c.path.module) ? c.name)))
  }
}

object LogicalRelationPushoutConnector {
  class PathTransformer(pushoutType: LogicalRelationPushoutType) extends ModulePathTransformer {
    override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_logrel_pushout_logrel")
  }
}
