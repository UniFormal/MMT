package info.kwarc.mmt

import info.kwarc.mmt.api.{GeneralError, GlobalName, InvalidObject, LocalName, MPath, Path}
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Link, ParametricLinearOperator, Renamer, SimpleLinearModuleTransformer, SimpleLinearOperator, SystematicRenamingUtils}
import info.kwarc.mmt.api.objects.{Context, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.lf.LF

final class LogicalRelationTransformer(links: List[Link], commonLinkDomain: MPath, commonLinkCodomain: MPath) extends SimpleLinearModuleTransformer with SystematicRenamingUtils {

  override val operatorDomain: MPath = commonLinkDomain
  override val operatorCodomain: MPath = commonLinkCodomain

  // todo: encode links in name?
  override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_logrel")

  val par: Renamer[LinearState] = getRenamerFor("ᵖ")
  val logrel: Renamer[LinearState] = getRenamerFor("ʳ")

  override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term, Option[Term])] = {
    val lr = (p: GlobalName) => {
      if (p == c.path || state.processedDeclarations.exists(_.path == p)) {
        OMS(logrel(p))
      } else {
        return NotApplicable(c, "refers to constant not previously processed. Implementation error?")
      }
    }
    val logicalRelation = new LogicalRelation(links.map(_.toTerm), lr, interp.ctrl.globalLookup)

    List(
      (par(name), par(tp), df.map(par(_))),
      // todo: also map definienses
      (logrel(name), logicalRelation.getExpected(Context.empty, c.toTerm, tp), None)
    )
  }
}

object LogicalRelationOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?logrel_operator")

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[SimpleLinearModuleTransformer] = {
    val links = parameters.map {
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
  }
}