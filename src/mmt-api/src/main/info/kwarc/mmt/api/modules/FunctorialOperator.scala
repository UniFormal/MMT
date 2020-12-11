package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.{GeneralError, GlobalName, InvalidElement, InvalidObject, LocalName, MPath}

abstract class FunctorialOperator extends DiagramOperator with FunctorialTransformer {
  protected def acceptDiagram(diagram: Term)(implicit interp: DiagramInterpreter): Option[List[MPath]]
  protected def submitDiagram(newModules: List[MPath]): Term

  final override def apply(diagram: Term, interp: DiagramInterpreter)(implicit ctrl: Controller): Option[Term] = diagram match {
    case OMA(OMS(`head`), List(inputDiagram)) =>
      interp(inputDiagram).flatMap(acceptDiagram(_)(interp)) match {
        case Some(modulePaths) =>
          val modules: Map[MPath, Module] = modulePaths.map(p => (p, interp.ctrl.getAs(classOf[Module], p))).toMap
          val state = initDiagramState(diagram, modules, interp)

          val newModulePaths = modulePaths.flatMap(modulePath => {
            applyModule(interp.get(modulePath))(interp, state).map(newModule => {

              state.processedElements.get(modulePath) match {
                case Some(`newModule`) => // ok
                case Some(m) if m != newModule =>
                  throw new Exception("...")

                case None =>
                  throw new Exception("...")
              }
              if (!interp.hasToplevelResult(newModule.path)) {
                throw GeneralError("diagram operators' applyModule should insert resulting module to DiagramInterpreter")
              }

              newModule.path
            })
          })
          // todo: instead get new module paths from interp?
          Some(submitDiagram(newModulePaths))

        case None => None
      }

    case _ => None
  }
}

trait RelativeBaseOperator extends FunctorialOperator with RelativeBaseTransformer {
  final override def acceptDiagram(diagram: Term)(implicit interp: DiagramInterpreter): Option[List[MPath]] = diagram match {
    case SimpleDiagram(`operatorDomain`, modulePaths) => Some(modulePaths)
    case SimpleDiagram(dom, _) if dom != operatorDomain =>
      // todo check for implicit morphism from `domain` to actual domain
      interp.errorCont(InvalidObject(diagram, s"Operator ${this.getClass.getSimpleName} only applicable " +
        s"on diagrams over $operatorDomain. Given diagram was over $dom."))
      None
    case _ =>
      interp.errorCont(InvalidObject(diagram, s"Operator ${this.getClass.getSimpleName} only applicable " +
        "on simple diagrams. Did simplification not kick in?"))
      None
  }
}

abstract class LinearOperator extends FunctorialOperator with LinearModuleTransformer with RelativeBaseOperator {
  final override def submitDiagram(newModules: List[MPath]): Term = SimpleDiagram(operatorCodomain, newModules)
}

abstract class LinearConnector extends FunctorialOperator with LinearConnectorTransformer with RelativeBaseOperator {
  final override def submitDiagram(newModules: List[MPath]): Term = SimpleDiagram(operatorCodomain, newModules)
}

/**
  * A [[LinearOperator]] that works (type, definiens)-by-(type, definiens): all declarations that are
  * not [[FinalConstant]]s (with a type) are treated appropriately and subclasses only need to implement a method
  * receiving a [[Term]] for the type component and an ''Option[Term]'' for an optional definiens.
  *
  * Moreover, for convenience the linear state is fixed to be the one from [[DefaultLinearStateOperator]].
  */
abstract class SimpleLinearOperator extends LinearOperator with SimpleLinearModuleTransformer

abstract class SimpleLinearConnector extends LinearConnector with SimpleLinearConnectorTransformer

class IdentityLinearOperator(private val domain: MPath) extends LinearModuleTransformer with DefaultLinearStateOperator {
  override val operatorDomain: MPath = domain
  override val operatorCodomain: MPath = domain

  override protected def applyModuleName(name: LocalName): LocalName = name

  final override protected def applyDeclaration(container: Container, containerState: LinearState, decl: Declaration)(implicit interp: DiagramInterpreter, state: DiagramState): Unit = {}
}

/**
  * todo: shouldn't applyConstantSimple only output an Option[Term] here? Why name?
  */
abstract class SimpleInwardsConnector(final override val head: GlobalName, val operator: SimpleLinearOperator)
  extends SimpleLinearConnector {
  final override val in: LinearModuleTransformer = new IdentityLinearOperator(operator.operatorDomain)
  final override val out: operator.type = operator
}

/*

a nice DSL for specifying operators together with connectors:

  // helper functions to make up a nice DSL
  final protected def MainResults(decls: SimpleConstant*): List[List[SimpleConstant]] = List(decls.toList)
  final protected def MainResults(initDecls: Seq[SimpleConstant], decls: SimpleConstant*): List[List[SimpleConstant]] = MainResults((initDecls.toList ::: decls.toList) : _*)

  // Conn results are almost always morphisms, hence definiens must always be given (no Option[Term])
  final protected def ConnResults(decls: (LocalName, Term, Term)*): List[List[SimpleConstant]] =
    List(decls.map(d => (d._1, d._2, Some(d._3))).toList)

    final protected val NoResults: List[List[SimpleConstant]] = Nil
*/