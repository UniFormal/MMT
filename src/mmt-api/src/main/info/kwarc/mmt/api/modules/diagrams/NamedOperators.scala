package info.kwarc.mmt.api.modules.diagrams

/**
  * Classes for named functorial operators, i.e., operators bound to an MMT symbol.
  *
  * All of these classes are minor; almost all of the logic lies in the mixed in traits
  * from LinearTransformer.scala for anonymous functorial operators.
  *
  * @see LinearTransformer.scala for anonymous functorial operators
  */

import info.kwarc.mmt.api.{GlobalName, Rule, SyntaxDrivenRule}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._


/**
  * A diagram operator whose main functionality is given by [[apply()]].
  *
  * The method [[apply()]] receives a diagram and a [[DiagramInterpreter]] instance and
  * may then inspect the input elements in the diagram and freely add new (output)
  * elements via calls to the [[DiagramInterpreter]].
  *
  * All diagram operators extend [[SyntaxDrivenRule]], i.e. are [[Rule MMT rules]] that can
  * be loaded into MMT theories -- thus suitable for a modular approach to making diagram
  * operators available to end users -- and they carry a [[head]] symbol identifying them.
  *
  * To implement a new operator:
  *
  *   1. create an untyped constant in some MMT theory (in surface syntax), e.g.
  *      ''my_diag_op # MY_DIAG_OP'',
  *
  *   2. subclass [[NamedDiagramOperator]] (or rather one of the many subclasses that suits you,
  *      most likely [[LinearOperator]]) and make [[head]] point to the URI of the just created symbol,
  *
  *   3. and load the diagram operator via ''rule <juri>scala://...'' (e.g. directly after the symbol from
  *      point 1).
  *
  * @see [[LinearOperator]]
  */
abstract class NamedDiagramOperator extends SyntaxDrivenRule {
  // make parent class' head function a mere field, needed to include it in pattern matching patterns (otherwise: "stable identifier required, but got head [a function!]")
  override val head: GlobalName


  /**
    * Call [[DiagramInterpreter.apply()]] on diagram arguments before inspecting them!
    *
    * // need access to Controller for generative operators (i.e. most operators)
    * // todo: upon error, are modules inconsistently added to controller? avoid that.
    *
    * @param diagram
    * @param interp
    * @param ctrl
    * @return
    */
  def apply(diagram: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term]
}

// TODO: rework this part of the class hierarchy, DiagramTransformer should correspond (and be named like) to SemanticDiagramOperator
abstract class SemanticDiagramOperator extends NamedDiagramOperator with DiagramOperator {
  override def apply(t: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term] = t match {
    case OMA(OMS(`head`), List(diag)) =>
      interp(diag).flatMap(applyDiagram).map(_.toTerm)

    case _ => None
  }
}

abstract class NamedLinearFunctor extends SemanticDiagramOperator with LinearFunctor
abstract class NamedLinearConnector extends SemanticDiagramOperator with LinearConnector

abstract class NamedInwardsConnector(final override val head: GlobalName, functor: Functor)
  extends NamedLinearConnector with InwardsLinearConnector {
  final override val out = functor
}

abstract class NamedOutwardsConnector(final override val head: GlobalName, functor: Functor)
  extends NamedLinearConnector with OutwardsLinearConnector {
  final override val in = functor
}

abstract class ParametricLinearOperator extends NamedDiagramOperator {
  // todo: we need a way in instantiate to report InvalidObjects to interp.errorCont!
  def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearOperator]

  final override def apply(diagram: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term] = diagram match {
    case OMA(OMS(`head`), parameters :+ actualDiagram) =>
      instantiate(parameters)
        .flatMap(tx => interp(actualDiagram).flatMap(tx.applyDiagram))
        .map(_.toTerm)

    case _ => None
  }
}