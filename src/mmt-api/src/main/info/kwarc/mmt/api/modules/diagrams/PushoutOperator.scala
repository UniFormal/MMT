package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.{OMIDENT, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.{GlobalName, ImplementationError, LocalName, MPath, Path}

object PushoutOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?pushout_operator")

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearTransformer] = parameters match {
    case List(OMMOD(dom), OMMOD(cod), mor) =>
      Some(new ZippingTransformer(List(
        new PushoutTransformer(dom, cod, mor),
        new PushoutConnector(dom, cod, mor)
      )))

    case _ => None
  }

  private class PushoutTransformer(
                                    dom: MPath,
                                    cod: MPath,
                                    mor: Term)
      extends SimpleLinearFunctor {

    override val operatorDomain: Diagram = Diagram(List(dom))
    override val operatorCodomain: Diagram = Diagram(List(cod))
    override def applyDomainModule(path: MPath): MPath = path match {
      case `dom` => cod
    }

    def applyModuleName(name: LocalName): LocalName =
      name.suffixLastSimple("_pushout_over_" + mor.toStr(shortURIs = true))

    private def getMorphismIntoPushout(container: Container): Term = {
      // The expressions in container are expressions over the theory
      // with the following module path:
      val exprContext: MPath = container match {
        case t: Theory => t.path
        case v: View => v.to.toMPath
      }

      OMMOD(new PushoutConnector(dom, cod, mor).applyModulePath(exprContext))
    }

    override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter): List[Constant] = {
      val pushoutMor = getMorphismIntoPushout(interp.ctrl.getModule(c.path.module))
      def translate(t: Term): Term = interp.ctrl.library.ApplyMorphs(t, pushoutMor)

      List(Constant(
        home = OMMOD(applyModulePath(c.path.module)),
        name = c.name,
        alias = c.alias,
        tp = c.tp.map(translate),
        df = c.df.map(translate),
        rl = c.rl,
        not = c.notC.copy() // probably need to tweak argument positions, no?
      ))
    }
  }

  private class PushoutConnector(dom: MPath, cod: MPath, mor: Term) extends SimpleLinearConnector {
    def applyModuleName(name: LocalName): LocalName =
      name.suffixLastSimple("_pushout_view_over_" + mor.toStr(shortURIs = true))

    override val in: LinearFunctor = LinearFunctor.identity(dom)
    override val out = new PushoutTransformer(dom, cod, mor)
    override def applyDomainTheory(thy: MPath): Term = thy match {
      case `dom` => mor
    }

    override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter): List[Constant] = {
      List(assgn(c.path, OMS(out.applyModulePath(c.path.module) ? c.name)))
    }
  }
}
