package info.kwarc.mmt

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.modules.diagops._
import info.kwarc.mmt.api.objects.{Context, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.{Constant, Structure}
import info.kwarc.mmt.api.uom.SimplificationUnit

object LogicalRelationPushoutOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?logrel_pushout_operator")

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearTransformer] = {
    parameters match {
      case OMMOD(domain) +: OMMOD(codomain) +: mors :+ OMMOD(logrel) =>
        val pushoutTransformers = mors.map(mor =>
          PushoutOperator.instantiate(List(OMMOD(domain), OMMOD(codomain), mor))
        ).map(_.get)

        // order is important!
        val transformers = pushoutTransformers ::: List(
          new LogicalRelationTransformer(mors, domain, codomain),
          new LogicalRelationPushoutTransformer(OMMOD(logrel), mors, domain, codomain),
          new LogicalRelationPushoutConnector(OMMOD(logrel), mors, domain, codomain)
        )

        Some(new InParalleLinearTransformer(transformers))

      case _ =>
        None
    }
  }
}

final class LogicalRelationPushoutTransformer(
                                               initialLogrel: Term,
                                               mors: List[Term],
                                               commonLinkDomain: MPath,
                                               commonLinkCodomain: MPath
                                             )
  extends LogicalRelationPushoutTransformer.PathTransformer(initialLogrel, mors, commonLinkDomain, commonLinkCodomain)
    with SimpleLinearModuleTransformer
    with OperatorDSL {

/*  val copyRenamers: Array[Renamer[LinearState]] = mors.indices.map(i =>
    getRenamerFor(UnicodeStrings.subscriptInteger(i))
  ).toArray*/
  val related: Renamer[LinearState] = getRenamerFor("ᵀ")

  private val pushoutTransformers = mors.map(mor =>
    new PushoutTransformer.PathTransformer(commonLinkDomain, commonLinkCodomain, mor)
  )
  private val pushoutInjectionTransformers = mors.map(mor =>
    new PushoutConnector.PathTransformer(mor)
  )

  override def beginTheory(thy: Theory, state: LinearState)(implicit interp: DiagramInterpreter): Option[Theory] = {
    super.beginTheory(thy, state).map(outTheory => {
      pushoutTransformers.zipWithIndex.foreach {
        case (tx, i) =>
          val structure = Structure(
            home = outTheory.toTerm,
            name = LocalName("struct_" + i.toString),
            from = OMMOD(tx.applyModulePath(thy.path)),
            isImplicit = false,
            isTotal = true // TODO: is this correct?
          )

          interp.add(structure)
          interp.endAdd(structure)
      }

      outTheory
    })
  }

  // The expressions in container are expressions over the theory
  // with the following module path:
  private def getApplicableContext(container: Container): MPath = container match {
    case t: Theory => t.path
    case v: View => v.to.toMPath
  }

  private def getApplicableLogrel(implicit state: LinearState): Term = {
    OMMOD(new LogicalRelationPushoutConnector.PathTransformer(initialLogrel, mors, commonLinkDomain, commonLinkCodomain).applyModulePath(getApplicableContext(state.inContainer)))
  }

  private def getApplicableMors(implicit state: LinearState): List[Term] = {
    val applicableContext = getApplicableContext(state.inContainer)
    pushoutInjectionTransformers.map(tx => OMMOD(tx.applyModulePath(applicableContext)))
  }

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {

    val logicalRelation = new LogicalRelation(
      getApplicableMors,
      p => interp.ctrl.globalLookup.ApplyMorphs(getApplicableLogrel, OMS(p)),
      interp.ctrl.globalLookup
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
  class PathTransformer(initialLogrel: Term, mors: List[Term], commonLinkDomain: MPath, commonLinkCodomain: MPath) extends ModulePathTransformer with RelativeBaseTransformer {

    override val operatorDomain: DiagramT = DiagramT.singleton(commonLinkDomain)
    override val operatorCodomain: DiagramT = DiagramT.singleton(commonLinkCodomain)

    // TODO: encode morphism names into name here?
    def applyModuleName(name: LocalName): LocalName =
      name.suffixLastSimple("_logrel_pushout")
  }
}

private class LogicalRelationPushoutConnector(initialLogrel: Term, mors: List[Term], commonLinkDomain: MPath, commonLinkCodomain: MPath)
  extends LogicalRelationPushoutConnector.PathTransformer(initialLogrel, mors, commonLinkDomain, commonLinkCodomain)
    with SimpleLinearConnectorTransformer
    with OperatorDSL {

  // TODO: encode morphism names into name here?
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_logrel_pushout")

  override val in = new LogicalRelationTransformer(mors, commonLinkDomain, commonLinkCodomain)
  override val out = new LogicalRelationPushoutTransformer.PathTransformer(initialLogrel, mors, commonLinkDomain, commonLinkCodomain)

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    List(assgn(c.path, OMS(out.applyModulePath(c.path.module) ? c.name)))
  }
}

object LogicalRelationPushoutConnector {
  class PathTransformer(initialLogrel: Term, mors: List[Term], commonLinkDomain: MPath, commonLinkCodomain: MPath) extends ModulePathTransformer {
    override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_logrel_pushout_logrel")
  }
}
