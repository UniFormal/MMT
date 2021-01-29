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
      Some(new InParallelLinearTransformer(List(
        new PushoutTransformer(dom, cod, mor),
        new PushoutConnector(dom, cod, mor)
      )))

    case _ => None
  }
}

/**
  * Pushes out theories and views over some morphism `mor`.
  *
  * @param operatorDomain   The domain of `mor`.
  * @param operatorCodomain The codomain of `mor`.
  * @param mor              The morphism, possibly a complex term; fed to `Lookup.ApplyMorphs`.
  */
private class PushoutTransformer(
                                  dom: MPath,
                                  cod: MPath,
                                  mor: Term)
  extends PushoutTransformer.PathTransformer(dom, cod, mor)
    with SimpleLinearModuleTransformer
    with OperatorDSL {

  override val operatorDomain: Diagram = Diagram(List(dom))
  override val operatorCodomain: Diagram = Diagram(List(cod))

  private def getMorphismIntoPushout(container: Container): Term = {
    // The expressions in container are expressions over the theory
    // with the following module path:
    val exprContext: MPath = container match {
      case t: Theory => t.path
      case v: View => v.to.toMPath
    }

    OMMOD(new PushoutConnector.PathTransformer(mor).applyModulePath(exprContext))
  }

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    def translate(t: Term): Term =
      interp.ctrl.library.ApplyMorphs(t, getMorphismIntoPushout(state.inContainer))

    List(Constant(
      home = state.outContainer.toTerm,
      name = c.name,
      alias = c.alias,
      tp = c.tp.map(translate),
      df = c.df.map(translate),
      rl = c.rl,
      not = c.notC.copy()
    ))
  }
}

object PushoutTransformer {
  class PathTransformer(dom: MPath, cod: MPath, mor: Term) extends ModulePathTransformer with RelativeBaseTransformer {

    override val operatorDomain: Diagram = Diagram(List(dom))
    override val operatorCodomain: Diagram = Diagram(List(cod))

    def applyModuleName(name: LocalName): LocalName =
      name.suffixLastSimple("_pushout_over_" + mor.toStr(shortURIs = true))
  }
}

private class PushoutConnector(dom: MPath, cod: MPath, mor: Term) extends PushoutConnector.PathTransformer(mor) with SimpleLinearConnectorTransformer with OperatorDSL {

  override val in: LinearFunctorialTransformer = LinearFunctorialTransformer.identity(dom)
  override val out = new PushoutTransformer.PathTransformer(dom, cod, mor)
  override def applyMetaModule(m: Term)(implicit lookup: Lookup): Term = m match {
    case OMMOD(`dom`) => OMMOD(dom)
    case OMIDENT(OMMOD(p)) if lookup.hasImplicit(p, dom) => mor
    case _ => throw ImplementationError("unreachable")
  }

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    List(assgn(c.path, OMS(out.applyModulePath(c.path.module) ? c.name)))
  }
}

object PushoutConnector {
  class PathTransformer(mor: Term) extends ModulePathTransformer {
    def applyModuleName(name: LocalName): LocalName =
      name.suffixLastSimple("_pushout_view_over_" + mor.toStr(shortURIs = true))
  }
}
