package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.modules.{Link, Theory, View}
import info.kwarc.mmt.api.objects.{OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath, Path}

object PushoutOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?pushout_operator")

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearOperator] = parameters match {
    case List(OMMOD(dom), OMMOD(cod), mor) =>
      val connection = DiagramConnection.singleton(dom, cod, mor)
      Some(new PushoutFunctor(connection) :: new PushoutConnector(connection))

    case _ => None
  }

  private class PushoutFunctor(connection: DiagramConnection) extends LinearFunctor {
    def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_pushout")

    override val dom: Diagram = connection.dom
    override val cod: Diagram = connection.cod
    override def applyDomainModule(m: MPath): MPath = connection.functor(m).toMPath

    // lazy due to cyclic instance creation with PushoutConnector
    private lazy val connector = new PushoutConnector(connection)

    override def applyConstant(c: Constant, container: Container)(implicit interp: DiagramInterpreter): Unit = {
      val translationMor: Term = OMMOD(connector.applyModulePath(container match {
        case t: Theory => t.path
        case l: Link => l.to.toMPath
      }))
      def translate(t: Term): Term = interp.ctrl.library.ApplyMorphs(t, translationMor)

      interp.add(Constant(
        home = OMMOD(equiNamer(c.path).module),
        name = equiNamer(c.path).name,
        alias = c.alias,
        tp = c.tp.map(translate),
        df = c.df.map(translate),
        rl = c.rl,
        not = c.notC.copy() // probably need to tweak argument positions, no?
      ))
    }
  }

  private class PushoutConnector(connection: DiagramConnection) extends InwardsLinearConnector {
    def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_pushout_view")

    override val out = new PushoutFunctor(connection)
    override def applyDomainTheory(thy: MPath): Term = connection.applyTheory(thy)

    override def applyConstant(c: Constant, container: Container)(implicit interp: DiagramInterpreter): Unit = {
      interp.add(assgn(c.path, OMS(out.applyModulePath(c.path.module) ? c.name)))
    }
  }
}
