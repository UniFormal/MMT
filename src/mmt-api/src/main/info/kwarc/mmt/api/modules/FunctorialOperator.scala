package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.{GlobalName, InvalidObject, LocalName, MPath}

abstract class FunctorialOperator extends DiagramOperator with FunctorialTransformer {
  protected def acceptDiagram(diagram: Term)(implicit interp: DiagramInterpreter): Option[List[MPath]]
  protected def submitDiagram(newModules: List[MPath]): Term

  final override def apply(diagram: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term] = diagram match {
    case OMA(OMS(`head`), List(inputDiagram)) =>
      interp(inputDiagram)
        .flatMap(acceptDiagram(_)(interp))
        .flatMap(applyDiagram)
        .map(submitDiagram)
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

  final override def submitDiagram(newModules: List[MPath]): Term = SimpleDiagram(operatorCodomain, newModules)
}

abstract class LinearOperator extends FunctorialOperator with LinearModuleTransformer with RelativeBaseOperator

abstract class ParametricLinearOperator extends DiagramOperator {
  def applyDiagram(diagram: Term): Option[(SimpleLinearModuleTransformer, Term)]

  final override def apply(diagram: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term] = {
    applyDiagram(diagram).flatMap {
      case (op, diag) => diag match {
        // TODO: ideally reuse code from RelativeBaseOperator
        case SimpleDiagram(dom, modulePaths) if dom == op.operatorDomain =>
          op.applyDiagram(modulePaths).map(SimpleDiagram(op.operatorCodomain, _))

        case _ =>
          interp.errorCont(InvalidObject(diagram, s"Parametric linear operator ${this.getClass.getSimpleName} not applicable"))
          None
      }
    }
  }
}

abstract class LinearConnector extends FunctorialOperator with LinearConnectorTransformer with RelativeBaseOperator

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