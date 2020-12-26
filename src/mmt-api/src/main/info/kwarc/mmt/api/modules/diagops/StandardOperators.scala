package info.kwarc.mmt.api.modules.diagops

/**
  * Foundation-independent diagram operators.
  * Prime example is the pushout operator.
  */

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DiagramInterpreter
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant

object CopyOperator extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(opSymbol), OMMOD(dom), OMMOD(cod)) => new CopyOperator(opSymbol, dom, cod)
    case _ => throw ParseError("invalid usage. correct usage: rule ...?CopyOperator <operator symbol " +
      "to tie with> <domain theory OMMOD> <codomain theory OMMOD>")
  }
}


class CopyOperator(override val head: GlobalName, override val operatorDomain: MPath, override val operatorCodomain: MPath) extends SimpleLinearOperator with OperatorDSL {

  override def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_copy")

  val copy1: Renamer[LinearState] = getRenamerFor("1")
  val copy2: Renamer[LinearState] = getRenamerFor("2")

  override def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    List(
      const(copy1(c.path), copy1(tp), df.map(copy1.apply(_))),
      const(copy2(c.path), copy2(tp), df.map(copy2.apply(_)))
    )
  }
}

class CopyConnectorFirst(override val head: GlobalName, copyOp: CopyOperator) extends SimpleLinearConnector with OperatorDSL {

  override val in: LinearModuleTransformer = new IdentityLinearTransformer(copyOp.operatorDomain)
  override val out: LinearModuleTransformer = copyOp

  override def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_copy1")

  override def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {

    val copy1 = copyOp.copy1.coercedTo(state)

    List(assgn(c.path, copy1(c)))
  }
}

object PushoutOperator {}

/* to be reinstantiated by Navid (this comment is from 2020-12-11):

object PushoutOperator extends DiagramOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories/modexp-test?DiagramOperators?pushout_operator")

  override def apply(diagram: Term, interp: DiagramInterpreter)(implicit ctrl: Controller): Option[Term] = diagram match {
    case OMA(OMS(`head`), List(OMMOD(translationViewPath), diagram)) =>
      val translationView = ctrl.getAs(classOf[View], translationViewPath)

      new SimpleLinearOperator with DefaultLinearStateOperator {
        override val head: GlobalName = PushoutOperator.head
        override protected val operatorDomain: MPath = translationView.from.toMPath
        override protected val operatorCodomain: MPath = translationView.to.toMPath

        override protected val connectionTypes = List(
          new InToOutMorphismConnectionType {
            // suffixing complex steps doesn't work: ComplexStep(viewtranslationViewPath) / "projection"
            override def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_pushout_projection")
          }
        )

        // suffixing complex step doesn't work: / ComplexStep(translationViewPath)
        override protected def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_pushout")

        override protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: LinearState): List[List[(LocalName, Term, Option[Term])]] = {

          // cannot use [[ApplyMorphs]] here as the View we're constructing in parallel does not even exist yet
          // todo: isn't creation of a new OMSReplacer every time inefficient? How about creating a parametrized OMSReplacers?
          val pushout: Term => Term = {
            val replacer = new OMSReplacer {
              override def replace(p: GlobalName): Option[Term] = {
                // Using [[Lookup.getO]] (instead of [[Controller.getO]]) provides us with the logic
                // of assignments being looked up in included views and the identity view on the meta theory
                // of domain/codomain as well.
                // E.g. when working in LF and if p points to LF's arrow symbol, then most views users would write
                // don't map it. Rather it is mapped by the implicitly assumed identity on the domain/codomain's
                // meta theory (LF). And this logic is only implemented in [[Lookup.getO]].
                ctrl.globalLookup.getO(translationView.toTerm, LocalName(p.module) / p.name) match {
                  case Some(assignment@(_: Constant)) => Some(assignment.df.get)
                  case Some(_) => ???
                  case None if state.processedDeclarations.exists(_.path == p) =>
                    Some(OMS(applyModulePath(container.modulePath) ? p.name))

                  case None => throw GeneralError("encountered unbound OMS reference for pushout")
                }
              }
            }

            replacer(_, state.outerContext)
          }


          List(
            List((name, pushout(tp), df.map(pushout))),
            List((name, tp, Some(OMID(applyModulePath(container.modulePath) ? name))))
          )
        }
      }.apply(OMA(OMS(head), List(diagram)), interp)

    case _ => None
  }
}

*/