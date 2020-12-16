package info.kwarc.mmt.api.modules


/**
  * Classes for named functorial operators, i.e., operators bound to an MMT symbol.
  *
  * All of these classes are minor; almost all of the logic lies in the mixed in traits
  * from DiagramTransformer.scala for anonymous functorial operators.
  *
  * @see DiagramTransformer.scala for anonymous functorial operators
  */

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.{GlobalName, ImplementationError, InvalidObject, LocalName, MPath}

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
    case BasedDiagram(dom, modulePaths) if interp.ctrl.globalLookup.hasImplicit(dom, operatorDomain) =>
      Some(modulePaths)
    case BasedDiagram(dom, _) =>
      // todo check for implicit morphism from `domain` to actual domain
      interp.errorCont(InvalidObject(diagram, s"Operator ${this.getClass.getSimpleName} only applicable " +
        s"on diagrams over $operatorDomain. Given diagram was over $dom (and no implicits from $dom " +
        "to $operatorDomain available."))
      None
    case _ =>
      interp.errorCont(InvalidObject(diagram, s"Operator ${this.getClass.getSimpleName} only applicable " +
        "on simple diagrams. Did simplification not kick in?"))
      None
  }

  final override def submitDiagram(newModules: List[MPath]): Term = BasedDiagram(operatorCodomain, newModules)
}

abstract class LinearOperator extends RelativeBaseOperator with LinearModuleTransformer


// def toTransformer(t: Term): Transformer

abstract class ParametricLinearOperator extends DiagramOperator {
  def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[SimpleLinearModuleTransformer]

  final override def apply(diagram: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term] = {
    diagram match {
      case OMA(OMS(`head`), parameters :+ actualDiagram) =>
        instantiate(parameters).flatMap(op => interp(actualDiagram) match {
          // TODO: ideally reuse code from RelativeBaseOperator
          case Some(BasedDiagram(dom, modulePaths)) if interp.ctrl.globalLookup.hasImplicit(dom, op.operatorDomain) =>
            op.applyDiagram(modulePaths).map(BasedDiagram(op.operatorCodomain, _))

          case Some(unsupportedDiag) =>
            interp.errorCont(InvalidObject(unsupportedDiag, s"Parametric linear operator ${this.getClass.getSimpleName} not applicable on diagrams that aren't SimpleDiagrams (even after simplification)"))
            None

          case _ =>
            interp.errorCont(InvalidObject(diagram, s"Parametric linear operator ${this.getClass.getSimpleName} not applicable"))
            None
        })

      case _ => None
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

abstract class SimpleLinearConnector extends LinearConnector with SimpleLinearConnectorTransformer {
  final override def sanityCheck()(implicit interp: DiagramInterpreter): Unit = {
    super.sanityCheck()

    if (!interp.ctrl.globalLookup.hasImplicit(in.operatorCodomain, out.operatorCodomain)) {
      // todo:
      // throw ImplementationError(s"Connector ${this.getClass.getSimpleName} tried to connect operators in an incompatible way: there is no implicit morphism from ${in.operatorCodomain} to ${out.operatorCodomain}. This case does not make sense.")
    }
  }
}

/**
  * todo: shouldn't applyConstantSimple only output an Option[Term] here? Why name?
  */
abstract class SimpleInwardsConnector(final override val head: GlobalName, val operator: LinearModuleTransformer)
  extends SimpleLinearConnector {
  final override val in: LinearModuleTransformer = new IdentityLinearTransformer(operator.operatorDomain)
  final override val out: operator.type = operator
}

/**
  * todo: shouldn't applyConstantSimple only output an Option[Term] here? Why name?
  */
abstract class SimpleOutwardsConnector(final override val head: GlobalName, val operator: SimpleLinearOperator)
  extends SimpleLinearConnector {
  final override val in: operator.type = operator
  final override val out: LinearModuleTransformer = new IdentityLinearTransformer(operator.operatorDomain)
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