package info.kwarc.mmt

import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects.{Context, OMIDENT, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.{Constant, Include, PlainInclude}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.diagops.{OperatorDSL, ParametricLinearOperator, Renamer, SimpleLinearModuleTransformer, SystematicRenamingUtils}
import info.kwarc.mmt.api.uom.{SimplificationUnit, Simplifier}
import info.kwarc.mmt.lf.LF

final class LogicalRelationTransformer(mors: List[Term], commonLinkDomain: MPath, commonLinkCodomain: MPath) extends SimpleLinearModuleTransformer with OperatorDSL {

  override val operatorDomain: MPath = LF.theoryPath
  override val operatorCodomain: MPath = LF.theoryPath

  // todo: encode links in name?
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_logrel")

  override protected def beginTheory(thy: Theory, state: LinearState)(implicit interp: DiagramInterpreter): Option[Theory] = {
    super.beginTheory(thy, state).map(outTheory => {
      val include = PlainInclude(commonLinkCodomain, outTheory.path)
      interp.add(include)
      interp.endAdd(include)

      outTheory
    })
  }

  override protected def beginView(view: View, state: LinearState)(implicit interp: DiagramInterpreter): Option[View] = {
    super.beginView(view, state).map(outView => {
      val include = Include.assignment(
        outView.toTerm,
        commonLinkCodomain,
        Some(OMIDENT(OMMOD(commonLinkCodomain)))
      )
      interp.add(include)
      interp.endAdd(include)

      outView
    })
  }

  val logrel: Renamer[LinearState] = getRenamerFor("ʳ")

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    val lr = (p: GlobalName) => {
      if (state.processedDeclarations.exists(_.path == p)) {
        OMS(logrel(p))
      } else {
        return NotApplicable(c, "refers to constant not previously processed. Implementation error?")
      }
    }
    val logicalRelation = new LogicalRelation(mors, lr, interp.ctrl.globalLookup)
    def g(t: Term): Term = betaReduce(Context.empty, logicalRelation.getExpected(Context.empty, c.toTerm, t), interp.ctrl.simplifier)

    // todo: also map definienses
    List(const(logrel(c.path), g(tp), None))
  }

  private def betaReduce(ctx: Context, t: Term, simplifier: Simplifier): Term = simplifier(
    t,
    SimplificationUnit(ctx, expandDefinitions = false, fullRecursion = true),
    RuleSet(lf.Beta)
  )
}

object LogicalRelationOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?logrel_operator")

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[SimpleLinearModuleTransformer] = {
    parameters match {
      case OMMOD(domain) :: OMMOD(codomain) :: mors =>
        Some(new LogicalRelationTransformer(mors, domain, codomain))

      case _ =>
        None
    }
    /*val links = parameters.map {
      case OMMOD(linkPath) => interp.ctrl.getAs(classOf[Link], linkPath)
      case t =>
        interp.errorCont(InvalidObject(t, "cannot parse as path to link"))
        return None
    }

    val (domain, codomain) = {
      val domainsCodomains = links.map(link => (link.from, link.to)).unzip
      (domainsCodomains._1.distinct, domainsCodomains._2.distinct) match {
        case (List(dom), List(cod)) => (dom, cod)
        case _ =>
          interp.errorCont(GeneralError("passed links to logical relation operators must all have same domain/codomain"))
          return None
      }
    }

    Some(new LogicalRelationTransformer(links, domain.toMPath, codomain.toMPath))
    */
  }
}