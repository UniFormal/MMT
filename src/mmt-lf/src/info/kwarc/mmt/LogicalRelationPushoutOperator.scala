package info.kwarc.mmt

import info.kwarc.mmt
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.modules.diagrams._
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.uom.SimplificationUnit

/**
  * Some plain data object (+ utility methods) passed around between classes in this file.
  * Stores general information of what should be computed for the logical relation pushout.
  */
sealed case class LogrelPushoutInfo(initialLogrelInfo: LogicalRelationInfo) {
  val initialLogrel: Term = initialLogrelInfo.logrel
  val initialLogrelType: LogicalRelationType = initialLogrelInfo.logrelType
  val numMors: Int = initialLogrelInfo.logrelType.mors.size

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
        val logrelInfo = LogicalRelationInfo(
          LogicalRelationType(mors, domain, codomain),
          OMMOD(logrel)
        )
        val pushoutInfo = LogrelPushoutInfo(logrelInfo)

        // order is important!
        val transformers = List(
          mors.indices.map(new LogrelPushoutConnector(pushoutInfo, _)),
          List(new LogrelPushoutTransformer(pushoutInfo)),
          List(new LogicalRelationTransformer(
            pushoutInfo.getLogicalRelationTypeFor, Some(pushoutInfo.initialLogrelInfo)
          )),
          List(new LogrelPushoutLogrelConnector(pushoutInfo))
        ).flatten

        Some(new InParallelLinearTransformer(transformers))

      case _ => None
    }
  }
}

final class LogrelPushoutTransformer(pushoutInfo: LogrelPushoutInfo)
  extends LogrelPushoutTransformer.PathTransformer(pushoutInfo)
    with SimpleLinearModuleTransformer
    with OperatorDSL {

  val pushoutRenamers: Array[Renamer[LinearState]] = pushoutInfo.initialLogrelType.mors.indices.map(i =>
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

    val logrelType = pushoutInfo.getLogicalRelationTypeFor(theoryContext)
    val logrel = pushoutInfo.getLogicalRelationFor(theoryContext)

    val logrelRenamer: Renamer[LinearState] = {
      new LogicalRelationTransformer(_ => logrelType, Some(pushoutInfo.initialLogrelInfo))
        .logrel.coercedTo(state)
    }

    val homomorphicLogrel = new LogicalRelation(
      logrelType.mors,
      p => lookup.ApplyMorphs(OMS(logrelRenamer.applyAlways(p)), logrel),
      lookup
    )

    def translatePushout(i: Integer, t: Term): Term = {
      val relevantPushoutView = new LogrelPushoutConnector.PathTransformer(pushoutInfo, i).applyModulePath(theoryContext)
      interp.ctrl.globalLookup.ApplyMorphs(t, OMMOD(relevantPushoutView))
    }

    def betaReduce(t: Term): Term = {
      val su = SimplificationUnit(Context.empty, expandDefinitions = false, fullRecursion = true)
      interp.ctrl.simplifier(t, su, RuleSet(lf.Beta))
    }



    val pushedOutConstants = pushoutRenamers.zipWithIndex.map {
      case (renamer, i) =>
        val onlySingleMor = pushoutInfo.numMors == 1

        Constant(
          home = state.outContainer.toTerm,
          // only index original name if more than one morphism
          name = if (onlySingleMor) c.name else renamer(c.name),
          // but even in the case of one morphism, the indexed name should be available as an alias
          // (such that users can systematically rely on the operator's output)
          alias = if (onlySingleMor) List(renamer(c.name)) else Nil,
          tp = c.tp.map(translatePushout(i, _)).map(betaReduce),
          df = c.df.map(translatePushout(i, _)).map(betaReduce),
          rl = None,
          not = if (onlySingleMor) c.notC.copy() else NotationContainer.empty()
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
  class PathTransformer(pushoutInfo: LogrelPushoutInfo) extends ModulePathTransformer with RelativeBaseTransformer {

    override val operatorDomain: Diagram = Diagram.singleton(pushoutInfo.initialLogrelType.commonLinkDomain)
    override val operatorCodomain: Diagram = Diagram.singleton(pushoutInfo.initialLogrelType.commonLinkCodomain)

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
private class LogrelPushoutLogrelConnector(pushoutInfo: LogrelPushoutInfo)
  extends LogrelPushoutLogrelConnector.PathTransformer(pushoutInfo)
    with SimpleLinearConnectorTransformer
    with OperatorDSL {

  override val in = new LogicalRelationTransformer(pushoutInfo.getLogicalRelationTypeFor, Some(pushoutInfo.initialLogrelInfo))
  override val out = new LogrelPushoutTransformer.PathTransformer(pushoutInfo)

  override def applyMetaModule(t: Term): Term = t match {
    case OMMOD(p) if p == pushoutInfo.initialLogrelType.commonLinkDomain =>
      OMMOD(in.applyModulePath(pushoutInfo.initialLogrelType.commonLinkDomain))

    case OMIDENT(OMMOD(p)) if p == pushoutInfo.initialLogrelType.commonLinkDomain =>
      pushoutInfo.initialLogrel

    case t => t
  }

  private def getRelatedRenamer(implicit state: LinearState): Renamer[LinearState] = {
    new LogrelPushoutTransformer(pushoutInfo).related.coercedTo(state)
  }

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    val logrelRenamer: Renamer[LinearState] = in.logrel.coercedTo(state)

    List(assgn(
      logrelRenamer(c.path),
      getRelatedRenamer(state)(c)
    ))
  }
}

object LogrelPushoutLogrelConnector {
  class PathTransformer(pushoutInfo: LogrelPushoutInfo) extends ModulePathTransformer {
    // TODO: encode morphism names into name here?
    override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_lopu_logrel")
  }
}

/**
  * e.g. HardProd --> SoftProd
  */
private class LogrelPushoutConnector(pushoutInfo: LogrelPushoutInfo, morIndex: Integer) extends LogrelPushoutConnector.PathTransformer(pushoutInfo, morIndex) with SimpleLinearConnectorTransformer with OperatorDSL {

  override val in: LinearFunctorialTransformer =
    LinearFunctorialTransformer.identity(pushoutInfo.initialLogrelType.commonLinkDomain)
  override val out: ModulePathTransformer with RelativeBaseTransformer =
    new LogrelPushoutTransformer.PathTransformer(pushoutInfo)

  override def applyMetaModule(m: Term): Term = m match {
    case OMMOD(p) if p == pushoutInfo.initialLogrelType.commonLinkDomain =>
      OMMOD(pushoutInfo.initialLogrelType.commonLinkDomain)

    case OMIDENT(OMMOD(p)) if p == pushoutInfo.initialLogrelType.commonLinkDomain =>
      pushoutInfo.initialLogrelType.mors(morIndex)

    case _ => throw ImplementationError("unreachable")
  }

  private def getPushoutRenamer(implicit state: LinearState): Renamer[LinearState] = {
    new LogrelPushoutTransformer(pushoutInfo).pushoutRenamers(morIndex).coercedTo(state)
  }

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    List(assgn(
      c.path,
      getPushoutRenamer(state)(c)
    ))
  }
}

object LogrelPushoutConnector {
  class PathTransformer(pushoutInfo: LogrelPushoutInfo, morIndex: Integer) extends ModulePathTransformer {
    def applyModuleName(name: LocalName): LocalName =
      name.suffixLastSimple(s"_lopu_pushout$morIndex")
  }
}