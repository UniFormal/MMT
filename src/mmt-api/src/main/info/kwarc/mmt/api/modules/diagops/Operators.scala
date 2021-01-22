package info.kwarc.mmt.api.modules.diagops

/**
  * Classes for named functorial operators, i.e., operators bound to an MMT symbol.
  *
  * All of these classes are minor; almost all of the logic lies in the mixed in traits
  * from LinearTransformer.scala for anonymous functorial operators.
  *
  * @see LinearTransformer.scala for anonymous functorial operators
  */

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.{GlobalName, InvalidObject, MPath}

abstract class ModuleOperator extends DiagramOperator with DiagramTransformer {
  protected def acceptDiagram(diagram: Term)(implicit interp: DiagramInterpreter): Option[List[MPath]]
  protected def submitDiagram(newModules: List[MPath]): Term

  final override def apply(diagram: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term] = diagram match {
    case OMA(OMS(`head`), List(inputDiagram)) =>
      interp(inputDiagram)
        .flatMap(acceptDiagram(_)(interp))
        .map(applyDiagram)
        .map(submitDiagram)

    case _ => None
  }
}

trait RelativeBaseOperator extends ModuleOperator with RelativeBaseTransformer {
  final override def acceptDiagram(diagram: Term)(implicit interp: DiagramInterpreter): Option[List[MPath]] = diagram match {
    case DiagramTermBridge(diag) if diag.mt.exists(operatorDomain.subsumes(_)(interp.ctrl.globalLookup)) =>
      Some(diag.modules)

    case DiagramTermBridge(diag) =>
      interp.errorCont(InvalidObject(diagram, s"Operator ${this.getClass.getSimpleName} only applicable " +
        s"on diagrams over $operatorDomain. Given diagram was over $diag."))
      None

    case _ =>
      interp.errorCont(InvalidObject(diagram, s"Operator ${this.getClass.getSimpleName} only applicable " +
        "on atomic diagrams expressions. Did simplification not kick in?"))
      None
  }

  final override def submitDiagram(newModules: List[MPath]): Term =
    DiagramT(newModules, Some(operatorCodomain)).toTerm
}

abstract class LinearOperator extends RelativeBaseOperator with LinearModuleTransformer

// def toTransformer(t: Term): Transformer

abstract class ParametricLinearOperator extends DiagramOperator {
  // todo: we need a way in instantiate to report InvalidObjects to interp.errorCont!
  def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearTransformer]

  final override def apply(diagram: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term] = diagram match {
    case OMA(OMS(`head`), parameters :+ actualDiagram) =>

      instantiate(parameters).flatMap(tx => {
        val modulePaths: Option[List[MPath]] = interp(actualDiagram).collect {
          case DiagramTermBridge(diag) => Some(diag.modules)
          case _ => None
        }.flatten

        modulePaths.map(tx.applyDiagram(_))
      }).map(outputPaths => RawDiagram(outputPaths))
      /*instantiate(parameters).flatMap(op => interp(actualDiagram) match {
        // TODO: ideally reuse code from RelativeBaseOperator
        case Some(BasedDiagram(dom, modulePaths)) if interp.ctrl.globalLookup.hasImplicit(op.operatorDomain, dom) =>
          Some(BasedDiagram(op.operatorCodomain, op.applyDiagram(modulePaths)))

        case Some(unsupportedDiag) =>
          interp.errorCont(InvalidObject(unsupportedDiag, s"Parametric linear operator ${this.getClass.getSimpleName} not applicable on diagrams that aren't SimpleDiagrams (even after simplification)"))
          None

        case _ =>
          interp.errorCont(InvalidObject(diagram, s"Parametric linear operator ${this.getClass.getSimpleName} not applicable"))
          None
      })*/

    case _ => None
  }
}

abstract class LinearConnector extends LinearOperator with LinearConnectorTransformer with RelativeBaseOperator

/**
  * A [[LinearOperator]] that works (type, definiens)-by-(type, definiens): all declarations that are
  * not [[FinalConstant]]s (with a type) are treated appropriately and subclasses only need to implement a method
  * receiving a [[Term]] for the type component and an ''Option[Term]'' for an optional definiens.
  *
  * Moreover, for convenience the linear state is fixed to be the one from [[DefaultLinearStateOperator]].
  */
abstract class SimpleLinearOperator extends LinearOperator with LinearFunctorialTransformer with SimpleLinearModuleTransformer

abstract class SimpleLinearConnector extends LinearConnector with SimpleLinearConnectorTransformer

/**
  * todo: shouldn't applyConstantSimple only output an Option[Term] here? Why name?
  */
abstract class SimpleInwardsConnector(final override val head: GlobalName, val operator: SimpleLinearOperator)
  extends SimpleLinearConnector {
  final override val in: LinearFunctorialTransformer = LinearFunctorialTransformer.identity(operator.operatorDomain)
  final override val out: operator.type = operator
}

/**
  * todo: shouldn't applyConstantSimple only output an Option[Term] here? Why name?
  */
abstract class SimpleOutwardsConnector(final override val head: GlobalName, val operator: SimpleLinearOperator)
  extends SimpleLinearConnector {
  final override val in: operator.type = operator
  final override val out: LinearFunctorialTransformer = LinearFunctorialTransformer.identity(operator.operatorDomain)
}
