package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Library
import info.kwarc.mmt.api.modules.{Link, Theory}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{Constant, Declaration}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.uom.SimplificationUnit

sealed case class PushoutNames(viewNames: MPath => Option[MPath], pushoutNames: MPath => Option[MPath])
object PushoutNames {
  val default: PushoutNames = PushoutNames(_ => None, _ => None)
}

class PushoutFunctor(connection: DiagramConnection, names: PushoutNames = PushoutNames.default) extends LinearFunctor {
  override def applyModulePath(mpath: MPath): MPath = names.pushoutNames(mpath).getOrElse(super.applyModulePath(mpath))
  def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_pushout")

  override val dom: Diagram = connection.dom
  override val cod: Diagram = connection.cod

  override def applyDomainModule(m: MPath): MPath = connection.functor(m).toMPath

  // lazy due to cyclic instance creation with PushoutConnector
  private lazy val connector = new PushoutConnector(connection, names)

  override def translateConstant(c: Constant)(implicit interp: DiagramInterpreter): List[Declaration] = {
    val translationMor: Term = OMMOD(connector.applyModulePath(expressionContext(c).toMPath))

    val su = SimplificationUnit(
      Context(applyModulePath(c.path.module)),
      expandDefinitions = false,
      fullRecursion = true
    )

    def translate(t: Term): Term =
      interp.ctrl.simplifier.apply(interp.ctrl.library.ApplyMorphs(t, translationMor), su)

    List(Constant(
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

class PushoutConnector(connection: DiagramConnection, names: PushoutNames = PushoutNames.default) extends InwardsLinearConnector {
  override def applyModulePath(mpath: MPath): MPath = names.viewNames(mpath).getOrElse(super.applyModulePath(mpath))
  def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_pushout_view_over" + connection.toString)

  override val out = new PushoutFunctor(connection, names)

  override def applyDomainTheory(thy: MPath): Term = connection.applyTheory(thy)

  override def translateConstant(c: Constant)(implicit interp: DiagramInterpreter): List[Declaration] = {
    List(assgn(c.path, OMS(out.applyModulePath(c.path.module) ? c.name)))
  }
}

object GenericPushoutOperator extends ParametricLinearOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?generic_pushout_operator")

  override def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearOperator] = parameters match {
    case List(OMMOD(dom), OMMOD(cod), mor) =>
      val connection = DiagramConnection.Singleton(dom, cod, mor)
      Some(new PushoutFunctor(connection) :: new PushoutConnector(connection))

    case _ => None
  }
}

object SimplePushoutOperator extends NamedDiagramOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?simple_pushout")

  final override def apply(invocation: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term] = invocation match {
    case OMA(OMS(`head`), List(inputDiagramTerm, mor, viewNamesTerm, pushoutNamesTerm)) =>
      implicit val library: Library = interp.ctrl.library

      val List(inputDiagram, viewNames, pushoutNames) = List(inputDiagramTerm, viewNamesTerm, pushoutNamesTerm).map(
        interp(_).getOrElse(return None)
      )

      if (List(inputDiagram, viewNames, pushoutNames).exists(_.mt.nonEmpty)) {
        interp.errorCont(InvalidObject(
          invocation,
          s"Diagrams passed to ${getClass.getSimpleName} must not possess any meta diagrams; they are inferred anyway."
        ))
      }

      // try inferring the morphism's (co)domain as MPaths
      val (dom, cod) = (Morph.domain(mor), Morph.codomain(mor)) match {
        case (Some(OMMOD(dom_)), Some(OMMOD(cod_))) => (dom_, cod_)
        case _ =>
          interp.errorCont(InvalidObject(
            invocation,
            s"Cannot infer atomic domain and codomain (i.e., MPaths) from morphism passed to ${getClass.getSimpleName}`"
          ))
          return None
      }
      val connection = DiagramConnection.Singleton(dom, cod, mor)
      val diagram = inputDiagram.copy(mt = Some(connection.dom))
      val names = PushoutNames(
        viewNames = m => diagram.modules.zip(viewNames.modules).find(_._1 == m).map(_._2),
        pushoutNames = m => diagram.modules.zip(pushoutNames.modules).find(_._1 == m).map(_._2),
      )

      (new PushoutFunctor(connection, names) :: new PushoutConnector(connection, names))
        .applyDiagram(diagram).map(_.toTerm)

    case _ => None
  }
}
