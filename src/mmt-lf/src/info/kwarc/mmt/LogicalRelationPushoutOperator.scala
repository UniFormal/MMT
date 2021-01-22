package info.kwarc.mmt

import info.kwarc.mmt
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.modules.diagrams._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.uom.SimplificationUnit

sealed case class LogicalRelationPushoutType(initialLogrel: MPath, initialLogrelType: LogicalRelationType) {
  lazy val pushoutInjectionTransformers: List[ModulePathTransformer] = initialLogrelType.mors.map(mor =>
    new PushoutConnector.PathTransformer(mor)
  )

  def getLogicalRelationTypeFor(m: MPath): LogicalRelationType = {
    val currentMors: List[Term] = initialLogrelType.mors.indices.map(i =>
      new LogrelPushoutConnector.PathTransformer(this, i).applyModulePath(m)
    ).map(OMMOD(_)).toList

    val currentMorsDomain: MPath = m
    val currentMorsCodomain: MPath =
      new LogrelPushoutTransformer.PathTransformer(this).applyModulePath(m)

    LogicalRelationType(currentMors, currentMorsDomain, currentMorsCodomain)
  }

  def getLogicalRelationFor(m: MPath): Term = {
    OMMOD(new mmt.LogrelPushoutLogrelConnector.PathTransformer(this).applyModulePath(m))
  }
}

object LogicalRelationPushoutOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?logrel_pushout_operator")

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearTransformer] = {
    parameters match {
      case OMMOD(domain) +: OMMOD(codomain) +: mors :+ OMMOD(logrel) =>
        val pushoutType = LogicalRelationPushoutType(logrel, LogicalRelationType(mors, domain, codomain))

        // order is important!
        val transformers = List(
          mors.indices.map(new LogrelPushoutConnector(pushoutType, _)),
          List(new LogrelPushoutTransformer(pushoutType)),
          List(new LogicalRelationTransformer(pushoutType.getLogicalRelationTypeFor)),
          List(new LogrelPushoutLogrelConnector(pushoutType))
        ).flatten

        Some(new InParalleLinearTransformer(transformers))

      case _ => None
    }
  }
}

final class LogrelPushoutTransformer(pushoutType: LogicalRelationPushoutType)
  extends LogrelPushoutTransformer.PathTransformer(pushoutType)
    with SimpleLinearModuleTransformer
    with OperatorDSL {

  val pushoutRenamers: Array[Renamer[LinearState]] = pushoutType.initialLogrelType.mors.indices.map(i =>
    getRenamerFor(s"_$i")
  ).toArray
  val related: Renamer[LinearState] = getRenamerFor("_T") // "ᵀ"

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    val lookup = interp.ctrl.globalLookup

    // The expressions in container are expressions over the theory
    // with the following module path:
    val theoryContext: MPath = state.inContainer match {
      case thy: Theory => thy.path
      case link: Link => link.to.toMPath
    }

    val logrelType = pushoutType.getLogicalRelationTypeFor(theoryContext)
    val logrel = pushoutType.getLogicalRelationFor(theoryContext)

    val homomorphicLogrel = new LogicalRelation(
      logrelType.mors,
      p => lookup.ApplyMorphs(logrel, OMS(p)),
      lookup
    )

    def translatePushout(i: Integer, t: Term): Term = {
      val relevantPushoutView = new LogrelPushoutConnector.PathTransformer(pushoutType, i).applyModulePath(theoryContext)
      interp.ctrl.globalLookup.ApplyMorphs(t, OMMOD(relevantPushoutView))
    }

    def betaReduce(t: Term): Term = {
      val su = SimplificationUnit(Context.empty, expandDefinitions = false, fullRecursion = true)
      interp.ctrl.simplifier(t, su, RuleSet(lf.Beta))
    }

    val pushedOutConstants = pushoutRenamers.zipWithIndex.map {
      case (renamer, i) =>
        Constant(
          home = state.outContainer.toTerm,
          name = renamer(c.name),
          alias = Nil,
          tp = c.tp.map(translatePushout(i, _)),
          df = c.df.map(translatePushout(i, _)),
          rl = None
        )
    }.toList

    val constantsRelated = Constant(
      home = state.outContainer.toTerm, // todo: probably wrong?
      name = related(c.name),
      alias = Nil,
      tp = c.tp.map(homomorphicLogrel.getExpected(Context.empty, OMS(c.path), _)).map(betaReduce),
      df = None,
      rl = None
    )

    pushedOutConstants :+ constantsRelated
  }
}

object LogrelPushoutTransformer {
  class PathTransformer(pushoutType: LogicalRelationPushoutType) extends ModulePathTransformer with RelativeBaseTransformer {

    override val operatorDomain: Diagram = Diagram.singleton(pushoutType.initialLogrelType.commonLinkDomain)
    override val operatorCodomain: Diagram = Diagram.singleton(pushoutType.initialLogrelType.commonLinkCodomain)

    // TODO: encode morphism names into name here?
    def applyModuleName(name: LocalName): LocalName =
      name.suffixLastSimple("_lopu_result")
  }
}

/**
  * e.g. creates logrel TypePres: TypeEras: HardProd -> SoftProd
  *
  * the logical relation over [[LogrelPushoutConnector]]
  */
private class LogrelPushoutLogrelConnector(pushoutType: LogicalRelationPushoutType)
  extends LogrelPushoutLogrelConnector.PathTransformer(pushoutType)
    with SimpleLinearConnectorTransformer
    with OperatorDSL {

  override val in = new LogicalRelationTransformer(pushoutType.getLogicalRelationTypeFor)
  override val out = new LogrelPushoutTransformer.PathTransformer(pushoutType)

  private def getRelatedRenamer(implicit state: LinearState): Renamer[LinearState] = {
    new LogrelPushoutTransformer(pushoutType).related.coercedTo(state)
  }

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    List(assgn(
      c.path,
      getRelatedRenamer(state)(c)
    ))
  }
}

object LogrelPushoutLogrelConnector {
  class PathTransformer(pushoutType: LogicalRelationPushoutType) extends ModulePathTransformer {
    // TODO: encode morphism names into name here?
    override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_lopu_logrel")
  }
}

/**
  * e.g. HardProd --> SoftProd
  */
private class LogrelPushoutConnector(pushoutType: LogicalRelationPushoutType, morIndex: Integer) extends LogrelPushoutConnector.PathTransformer(pushoutType, morIndex) with SimpleLinearConnectorTransformer with OperatorDSL {

  override val in: LinearFunctorialTransformer =
    LinearFunctorialTransformer.identity(pushoutType.initialLogrelType.commonLinkDomain)
  override val out: ModulePathTransformer with RelativeBaseTransformer =
    new LogrelPushoutTransformer.PathTransformer(pushoutType)

  override def applyMetaModule(m: Term): Term = m match {
    case OMMOD(p) if p == pushoutType.initialLogrelType.commonLinkDomain =>
      OMMOD(pushoutType.initialLogrelType.commonLinkDomain)

    case OMIDENT(OMMOD(p)) if p == pushoutType.initialLogrelType.commonLinkDomain =>
      pushoutType.initialLogrelType.mors(morIndex)

    case _ => throw ImplementationError("unreachable")
  }

  private def getPushoutRenamer(implicit state: LinearState): Renamer[LinearState] = {
    new LogrelPushoutTransformer(pushoutType).pushoutRenamers(morIndex).coercedTo(state)
  }

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    List(assgn(
      c.path,
      getPushoutRenamer(state)(c)
    ))
  }
}

object LogrelPushoutConnector {
  class PathTransformer(pushoutType: LogicalRelationPushoutType, morIndex: Integer) extends ModulePathTransformer {
    def applyModuleName(name: LocalName): LocalName =
      name.suffixLastSimple(s"_lopu_pushout$morIndex")
  }
}