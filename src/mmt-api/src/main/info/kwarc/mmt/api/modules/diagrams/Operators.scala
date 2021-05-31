package info.kwarc.mmt.api.modules.diagrams

/**
  * Classes for named functorial operators, i.e., operators bound to an MMT symbol.
  *
  * All of these classes are minor; almost all of the logic lies in the mixed in traits
  * from LinearTransformer.scala for anonymous functorial operators.
  *
  * @see LinearTransformer.scala for anonymous functorial operators
  */

import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._

// TODO: rework this part of the class hierarchy, DiagramTransformer should correspond (and be named like) to SemanticDiagramOperator
abstract class SemanticDiagramOperator extends DiagramOperator with DiagramTransformer {
  override def apply(t: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term] = t match {
    case OMA(OMS(`head`), List(diag)) =>
      interp(diag).flatMap(applyDiagram).map(_.toTerm)

    case _ => None
  }
}

abstract class NamedLinearFunctor extends SemanticDiagramOperator with LinearFunctor
abstract class NamedLinearConnector extends SemanticDiagramOperator with LinearConnector

/**
  * todo: shouldn't applyConstantSimple only output an Option[Term] here? Why name?
  */
abstract class NamedInwardsConnector(final override val head: GlobalName, operator: LinearFunctor)
  extends NamedLinearConnector {
  final override val in: LinearFunctor = LinearFunctor.identity(operator.operatorDomain)
  final override val out: LinearFunctor = operator
}

/**
  * todo: shouldn't applyConstantSimple only output an Option[Term] here? Why name?
  */
abstract class SimpleOutwardsConnector(final override val head: GlobalName, operator: LinearFunctor)
  extends NamedLinearConnector {
  final override val in: LinearFunctor = operator
  final override val out: LinearFunctor = LinearFunctor.identity(operator.operatorDomain)
}


abstract class ParametricLinearOperator extends DiagramOperator {
  // todo: we need a way in instantiate to report InvalidObjects to interp.errorCont!
  def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearTransformer]

  final override def apply(diagram: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term] = diagram match {
    case OMA(OMS(`head`), parameters :+ actualDiagram) =>
      instantiate(parameters)
        .flatMap(tx => interp(actualDiagram).flatMap(tx.applyDiagram))
        .map(_.toTerm)

    case _ => None
  }
}