package info.kwarc.mmt

import info.kwarc.mmt
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.modules.diagrams._
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.uom.SimplificationUnit

import scala.annotation.tailrec

/**
  * Some plain data object (+ utility methods) passed around between classes in this file.
  * Stores general information of what should be computed for the logical relation pushout.
  */
private sealed case class LogrelPushoutInfo(initialLogrelInfo: ConcreteLogrel) {
  val initialLogrel: Term = initialLogrelInfo.logrel
  val initialLogrelType: LogrelType = initialLogrelInfo.logrelType
  val numMors: Int = initialLogrelInfo.logrelType.mors.size

  def getLogicalRelationTypeFor(m: MPath): LogrelType = {
    val currentMors: List[Term] = initialLogrelType.mors.indices.map(i =>
      new LogrelPushoutConnector.PathTransformer(this, i).applyModulePath(m)
    ).map(OMMOD(_)).toList

    val currentMorsDomain: MPath = m
    val currentMorsCodomain: MPath =
      new LogrelPushoutTransformer.PathTransformer(this).applyModulePath(m)

    LogrelType(currentMors, currentMorsDomain, currentMorsCodomain)
  }

  def getLogicalRelationFor(m: MPath): Term = {
    OMMOD(new LogrelPushoutLogrelConnector.PathTransformer(this).applyModulePath(m))
  }

  def getPartialLogrel(theoryContext: MPath, seenModules: Set[MPath], logrelRenamer: Renamer[_])
                      (implicit lookup: Lookup): PartialLogrel = {
    @tailrec
    def nonUnit(t: Term): Boolean = t match {
      case OMS(p) => p.name != LocalName("Unit")
      case OMBIND(_, _, body) => nonUnit(body)
      case _ => true
    }

    def getLogrelBaseAssignment(logrel: Term, p: GlobalName): Option[Term] = {
      val applicable =
        seenModules.contains(p.module) || lookup.hasImplicit(p.module, initialLogrelType.commonMorDomain)
      if (applicable) {
        Some(lookup.ApplyMorphs(OMS(logrelRenamer.applyAlways(p)), logrel)).filter(nonUnit)
      } else {
        None
      }
    }

    val logrelType = getLogicalRelationTypeFor(theoryContext)
    val logrelBase = getLogicalRelationFor(theoryContext)

    new PartialLogrel(logrelType.mors, getLogrelBaseAssignment(logrelBase, _), lookup)
  }

  /*def getFullLogrel(theoryContext: MPath, seenModules: Set[MPath], logrelRenamer: Renamer[_])
                      (implicit lookup: Lookup): GlobalName => Term = {

    val logrelBase = getLogicalRelationFor(theoryContext)
    p => {
      val applicable =
        seenModules.contains(p.module) || lookup.hasImplicit(p.module, initialLogrelType.commonMorDomain)
      if (applicable) {
        lookup.ApplyMorphs(OMS(logrelRenamer.applyAlways(p)), logrelBase)
      } else {
        ???
      }
    }
  }*/
}

abstract class LogrelPushoutOperator extends ParametricLinearOperator {
  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearTransformer] = {
    parameters match {
      case mors :+ OMMOD(logrel) =>
        LogrelOperator.parseLogRelInfo(mors)(interp.ctrl.globalLookup).map(logrelType => {
          val pushoutInfo = LogrelPushoutInfo(ConcreteLogrel(logrelType, OMMOD(logrel)))

          // order is important!
          val transformers = List(
            // the transformers creating the pushout injections
            mors.indices.map(new LogrelPushoutConnector(pushoutInfo, _)),
            // the transformer creating the pushout result as such
            List(new LogrelPushoutTransformer(pushoutInfo)),
            // the transformer creating the logrel interface theory
            List(new LogrelTransformer(
              pushoutInfo.getLogicalRelationTypeFor, Some(pushoutInfo.initialLogrelInfo)
            )),
            // the transformer realizing the logrel interface theory
            List(new LogrelPushoutLogrelConnector(pushoutInfo))
          ).flatten

          new InParallelLinearTransformer(transformers)
        })

      case _ => None
    }
  }
}

object FlexaryLogrelPushoutOperator extends LogrelPushoutOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?flexary_logrel_pushout_operator")
}

object UnaryLogrelPushoutOperator extends LogrelPushoutOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?unary_logrel_pushout_operator")
}

private final class LogrelPushoutTransformer(pushoutInfo: LogrelPushoutInfo)
  extends LogrelPushoutTransformer.PathTransformer(pushoutInfo)
    with SimpleLinearModuleTransformer
    with OperatorDSL {

  val pushoutRenamers: Array[Renamer[LinearState]] = pushoutInfo.initialLogrelType.mors.indices.map(i =>
    getRenamerFor(s"_$i")
  ).toArray
  val related: Renamer[LinearState] = getRenamerFor("_T") // "ᵀ"

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    // a lot of helper declarations follow
    // =======================================
    implicit val lookup: Lookup = interp.ctrl.globalLookup

    // The expressions in container are expressions over the theory
    // with the following module path:
    val theoryContext: MPath = state.inContainer match {
      case thy: Theory => thy.path
      case link: Link => link.to.toMPath
    }

    val logrelRenamer: Renamer[LinearState] = {
      val logrelType = pushoutInfo.getLogicalRelationTypeFor(theoryContext)
      new LogrelTransformer(_ => logrelType, Some(pushoutInfo.initialLogrelInfo))
        .logrelRenamer.coercedTo(state)
    }

    val logrel = pushoutInfo.getPartialLogrel(theoryContext, state.diagramState.seenModules.toSet, logrelRenamer)

    def translatePushout(i: Integer, t: Term): Term = {
      val relevantPushoutView = new LogrelPushoutConnector.PathTransformer(pushoutInfo, i).applyModulePath(theoryContext)
      try {
        interp.ctrl.globalLookup.ApplyMorphs(t, OMMOD(relevantPushoutView))
      } catch {
        case err: GetError if err.toString.contains("no assignment") => t
      }
    }

    def betaReduce(t: Term): Term = {
      val su = SimplificationUnit(Context.empty, expandDefinitions = false, fullRecursion = true)
      interp.ctrl.simplifier(t, su, RuleSet(lf.Beta))
    }

    // end of helper declarations
    // =======================================
    // now do actual work:

    val pushedOutConstants = pushoutRenamers.zipWithIndex.map {
      case (renamer, i) =>
        val onlySingleMor = pushoutInfo.numMors == 1

        Constant(
          home = state.outContainer.toTerm,
          // only index original name if more than one morphism
          name = if (onlySingleMor) c.name else renamer(c.name),
          // but even in the case of one morphism, the indexed name should be available as an alias
          // (not in views; irrelevant there), such that users can systematically rely on the operator's output
          alias = if (onlySingleMor && !state.inContainer.isInstanceOf[View]) List(renamer(c.name)) else Nil,
          tp = c.tp.map(translatePushout(i, _)).map(betaReduce),
          df = c.df.map(translatePushout(i, _)).map(betaReduce),
          rl = c.rl,
          not = if (onlySingleMor) c.notC.copy() else NotationContainer.empty()
        )
    }.toList

    if (c.name == LocalName("pair")) {
      val dbg = logrel(Context.empty, c.tp.get)
    }
    val constantsRelatedTp = c.tp.flatMap(logrel.getExpected(Context.empty, OMS(c.path), _)).map(betaReduce)
    val constantsRelatedDf = c.df.flatMap(logrel.apply(Context.empty, _)).map(betaReduce)

    constantsRelatedTp match {
      case Some(_) =>
        val constantsRelated = Constant(
          home = state.outContainer.toTerm, // todo: probably wrong?
          name = related(c.name),
          alias = Nil,
          tp = constantsRelatedTp,
          df = constantsRelatedDf,
          rl = None
        )

        pushedOutConstants :+ constantsRelated

      case None => pushedOutConstants
    }
  }
}

private object LogrelPushoutTransformer {
  class PathTransformer(pushoutInfo: LogrelPushoutInfo) extends ModulePathTransformer with RelativeBaseTransformer {

    override val operatorDomain: Diagram = Diagram.singleton(pushoutInfo.initialLogrelType.commonMorDomain)
    override val operatorCodomain: Diagram = Diagram.singleton(pushoutInfo.initialLogrelType.commonMorCodomain)

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

  override val in = new LogrelTransformer(pushoutInfo.getLogicalRelationTypeFor, Some(pushoutInfo.initialLogrelInfo))
  override val out = new LogrelPushoutTransformer.PathTransformer(pushoutInfo)

  override def applyMetaModule(t: Term)(implicit lookup: Lookup): Term = t match {
    case OMMOD(p) if p == pushoutInfo.initialLogrelType.commonMorDomain =>
      OMMOD(in.applyModulePath(pushoutInfo.initialLogrelType.commonMorDomain))

    case OMIDENT(OMMOD(p)) if p == pushoutInfo.initialLogrelType.commonMorDomain =>
      pushoutInfo.initialLogrel

    case t => t
  }

  private def getRelatedRenamer(implicit state: LinearState): Renamer[LinearState] = {
    new LogrelPushoutTransformer(pushoutInfo).related.coercedTo(state)
  }

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    val logrelRenamer: Renamer[LinearState] = in.logrelRenamer.coercedTo(state)
    /*val relatedConstant = getRelatedRenamer(state)(c)

    val assignment = if (interp.ctrl.getO(relatedConstant.path).isDefined) {
      relatedConstant
    } else {
      /*val logrel = pushoutInfo.getPartialLogrel(state.inTheoryContext, state.diagramState.seenModules.toSet, logrelRenamer)(interp.ctrl.globalLookup)
      logrel.synthesize(p => OMV("unit"), ???)(Context.empty, tp)*/
      OMV("TODO: we should synthesize this")
    }*/

    List(assgn(
      logrelRenamer(c.path),
      OMV("TODO: we should synthesize this") // assignment
    ))
  }
}

private object LogrelPushoutLogrelConnector {
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
    LinearFunctorialTransformer.identity(pushoutInfo.initialLogrelType.commonMorDomain)
  override val out: ModulePathTransformer with RelativeBaseTransformer =
    new LogrelPushoutTransformer.PathTransformer(pushoutInfo)

  override def applyMetaModule(m: Term)(implicit lookup: Lookup): Term = m match {
    case OMMOD(p) if p == pushoutInfo.initialLogrelType.commonMorDomain =>
      OMMOD(pushoutInfo.initialLogrelType.commonMorDomain)

    case OMIDENT(OMMOD(p)) if lookup.hasImplicit(p, pushoutInfo.initialLogrelType.commonMorDomain) =>
      pushoutInfo.initialLogrelType.mors(morIndex)

    // TODO(@FR,@NR): this is actually wrong
    case x => x
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

private object LogrelPushoutConnector {
  class PathTransformer(pushoutInfo: LogrelPushoutInfo, morIndex: Integer) extends ModulePathTransformer {
    def applyModuleName(name: LocalName): LocalName =
      name.suffixLastSimple(s"_lopu_pushout$morIndex")
  }
}