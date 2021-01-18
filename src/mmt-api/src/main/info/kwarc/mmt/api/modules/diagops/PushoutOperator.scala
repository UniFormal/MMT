package info.kwarc.mmt.api.modules.diagops

import info.kwarc.mmt.api.modules.DiagramInterpreter
import info.kwarc.mmt.api.objects.{OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath, Path}

private class PushoutPathTransformer(override val operatorDomain: MPath, override val operatorCodomain: MPath, mor: Term) extends ModulePathTransformer with RelativeBaseTransformer {
  def applyModuleName(name: LocalName): LocalName =
    name.suffixLastSimple(mor.toStr(shortURIs = true) + "_pushout")
}

private class PushoutConnectorPathTransformer(mor: Term) extends ModulePathTransformer {
  def applyModuleName(name: LocalName): LocalName =
    name.suffixLastSimple(mor.toStr(shortURIs = true) + "_pushout_view")
}

private class PushoutTransformer(
                          override val operatorDomain: MPath,
                          override val operatorCodomain: MPath,
                          mor: Term)
  extends PushoutPathTransformer(operatorDomain, operatorCodomain, mor)
    with SimpleLinearModuleTransformer
    with OperatorDSL {

  private def getPushoutView(container: Container): MPath =
    new PushoutConnectorPathTransformer(mor).applyModulePath(container.modulePath)

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    def translate(t: Term): Term =
      interp.ctrl.globalLookup.ApplyMorphs(t, OMMOD(getPushoutView(state.inContainer)))

    List(Constant(
      home = OMMOD(applyModulePath(c.path.module)),
      name = c.name,
      alias = c.alias,
      tp = c.tp.map(translate),
      df = c.df.map(translate),
      rl = c.rl,
      not = c.notC.copy
    ))
  }
}

private class PushoutConnector(dom: MPath, cod: MPath, mor: Term) extends PushoutConnectorPathTransformer(mor) with SimpleLinearConnectorTransformer with OperatorDSL {

  override val in = new IdentityLinearTransformer(dom)
  override val out = new PushoutPathTransformer(dom, cod, mor)
  override val translationView: Term = mor

  override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    List(assgn(c.path, OMS(out.applyModulePath(c.path.module) ? c.name)))
  }
}

object PushoutOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?pushout_operator")

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearTransformer] = parameters match {
    case List(OMMOD(dom), OMMOD(cod), mor) =>
      Some(new InParalleLinearTransformer(List(
        new PushoutConnector(dom, cod, mor),
        new PushoutTransformer(dom, cod, mor)
      )))

    case _ => None
  }
}
