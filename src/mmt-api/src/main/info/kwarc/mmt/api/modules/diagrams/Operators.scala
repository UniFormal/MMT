package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.{GlobalName, SyntaxDrivenRule}

/**
  * Fundamental interfaces of operators:
  *
  * - [[DiagramOperator]] is the most general interface, providing only an [[DiagramOperator.apply]] method mapping
  *   [[info.kwarc.mmt.api.objects.Term Term]]s to [[info.kwarc.mmt.api.objects.Term Term]]s.
  * - [[NamedOperator]] is a [[DiagramOperator]] that additionally inherits from [[SyntaxDrivenRule]], representing
  *   operators that can be exposed and invoked from surface syntax
  * - [[UnaryOperator]] is a [[DiagramOperator]] that only maps certain terms, namely [[Diagram]]s to [[Diagram]]s.
  * - [[NamedUnaryOperator]] combines the previous two interfaces
  */

/**
  * An anonymous diagram operator that maps [[Term]]s to [[Term]]s and is generative (i.e., produces side effects to
  * the theory graph known to [[info.kwarc.mmt.api.frontend.Controller Controller]]).
  *
  * There are two use cases for this general interface over [[UnaryOperator]] and subtraits:
  *  - Operators that require parameters (e.g., pushout inputs a morphism) and thus need the flexibility of accepting
  *    terms (encoding parameter sets) instead of mere diagrams.
  *    Often in these cases, every set of valid parameters induces a [[UnaryOperator]]. This gives rise to the idiom
  *    of (i) creating an operator that inputs the parameters (e.g., like [[SimplePushoutOperator]]) and then delegates
  *    under the hood to (ii) a [[UnaryOperator]] such as a functor or connector (e.g., like [[PushoutFunctor]] and
  *    [[PushoutConnector]]).
  *  - Meta-level operators such as the [[UnionDiagramOperator]] or [[SequencedDiagramOperators]].
  *
  * @see [[UnaryOperator]] for operators that actually map [[Diagram]]s to [[Diagram]]s.
  * @see [[NamedOperator]] for operators that can also be exposed and invoked from surface syntax.
  */
trait DiagramOperator {
  /**
    * Applies the diagram operator.
    * Side effects, e.g., new modules, are created via `interp`, and a final result
    * [[DiagramInterpreter diagram expression]] is returned.
    *
    * @param t      Some arbitrary term; which terms are valid must be specified by implementors.
    *               Many diagram operators (e.g., [[NamedLinearFunctor]]s) accept
    *               [[DiagramInterpreter diagram expressions]] only. But some meta-level operators
    *               (e.g., [[SequencedDiagramOperators]]) accept additional parameters encoded in this input term.
    * @param interp An evaluator for diagram expressions and an interface to an instance of
    *               [[info.kwarc.mmt.api.frontend.Controller Controller]]. Most diagram operators should run this
    *               evaluator on (subterms of, e.g., diagram parameters) of `t`. Moreover, use this evaluator to produce
    *               side effects (e.g., to add modules to the theory graph known to MMT).
    * @return Some result [[DiagramInterpreter diagram expression]] upon success, [[None]] otherwise.
    * @todo upon error, are modules inconsistently added to controller? avoid that.
    */
  def apply(t: Term)(implicit interp: DiagramInterpreter): Option[Term]
}

/**
  * An anonymous diagram operator that maps [[Diagram]] to [[Diagram]].
  *
  * @see [[NamedUnaryOperator]] for unary operators operators that can also be exposed and invoked from surface syntax.
  */
trait UnaryOperator {
  /**
    * Applies the operator to a diagram.
    *
    * Implementors are advised to call [[beginDiagram()]] (and upon `false` to return [[None]]) and to call
    * [[endDiagram()]] to finalize before returning.
    */
  def applyDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Option[Diagram]

  // The methods beginDiagram and endDiagram must be accessible to ZippingOperator, thus need to be accessible at least
  // within the `info.kwarc.mmt.api.modules.diagrams` package.

  /**
    * Checks if the operator is applicable on a diagram.
    *
    * The checks need not be exhaustive: this method is only intended to quickly rule out diagrams that operators are
    * surely not applicable on (i.e., false positives are allowed, false negatives are forbidden).
    * For example, [[LinearModuleOperator]] implements this method and checks whether the input diagram's meta diagram
    * matches the operator's [[LinearModuleOperator.dom domain diagram]].
    */
  private[diagrams] def beginDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Boolean

  /**
    * Finalizes the processing of a diagram.
    *
    * Before calling, [[beginDiagram()]] should have been called and returned `true`.
    * By default does nothing. Overwrite if necessary.
    */
  private[diagrams] def endDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Unit = {}
}

/**
  * A named diagram operator that maps [[Term]]s to [[Term]]s.
  *
  * Being a [[SyntaxDrivenRule]], named operators can be exposed and invoked in surface syntax.
  * Exposing a named operator works as follows:
  *
  *  1. Create an untyped constant in some MMT theory of your choice: ''my_diag_op # MY_DIAG_OP 1 2 3''.
  *     In this example, the notation allows to invoke the operator with exactly three arguments.
  *  2. Add a corresponding rule statement to the same MMT theory: ''rule <juri>scala://...''.
  *  3. Implement the [[NamedOperator]] (or rather one of the many subclasses that suits you, most likely
  *      [[LinearFunctor]]) with the fully-qualified Scala named given to it in Step 2 and overwrite
  *      [[SyntaxDrivenRule.head]] to point to the URI of the constant created in Step 1.
  *
  * Invoking a named operator in a [[InstallDiagram diagram derived module]] works as follows:
  * {{{
  * diagram diag : ?MetaTheory := MY_DIAG_OP arg1 arg2 arg3
  * }}}
  * where `?MetaTheory` is a theory in which the constant/notation for the diagram operator is in scope (e.g., via
  * includes).
  *
  * @see [[NamedLinearFunctor]] and [[NamedLinearConnector]] for the most common special cases.
  */
abstract class NamedOperator extends DiagramOperator with SyntaxDrivenRule {
  // Concretizing the parent class' head from a `def`ed function to a `val` makes it possible to use `head` in pattern
  // matching (otherwise, scalac errors with "stable identifier required, but got head [being a function!]").
  override val head: GlobalName
}

/**
  * A named diagram operator that maps [[Diagram]]s to [[Diagram]]s.
  *
  * As input in the [[apply]] method, the operator only accepts terms of the form `OMA(OMS(head, List(diag))` where
  * `head` is [[NamedOperator.head]] and `diag` a [[DiagramInterpreter diagram expression]].
  *
  * @see [[NamedLinearFunctor]] and [[NamedLinearConnector]] for the most common special cases.
  */
abstract class NamedUnaryOperator extends NamedOperator with UnaryOperator {
  /**
    * @param t A term of the form `OMA(OMS(head, List(diag))` where `head` is [[head]] and `diag` a
    *          [[DiagramInterpreter diagram expression]].
    * @inheritdoc
    */
  override def apply(t: Term)(implicit interp: DiagramInterpreter): Option[Term] = t match {
    case OMA(OMS(`head`), List(diag)) => interp(diag).flatMap(applyDiagram).map(_.toTerm)
    case _ => None
  }
}

abstract class NamedLinearFunctor extends NamedUnaryOperator with LinearFunctor
abstract class NamedLinearConnector extends NamedUnaryOperator with LinearConnector

abstract class NamedInwardsConnector(final override val head: GlobalName, functor: Functor)
  extends NamedLinearConnector with InwardsLinearConnector {
  final override val out = functor
}

abstract class NamedOutwardsConnector(final override val head: GlobalName, functor: Functor)
  extends NamedLinearConnector with OutwardsLinearConnector {
  final override val in = functor
}

abstract class ParametricLinearOperator extends NamedOperator {
  // todo: we need a way in instantiate to report InvalidObjects to interp.errorCont!
  def instantiate(parameters: List[Term])(implicit interp: DiagramInterpreter): Option[LinearOperator]

  final override def apply(diagram: Term)(implicit interp: DiagramInterpreter): Option[Term] = diagram match {
    case OMA(OMS(`head`), parameters :+ actualDiagram) =>
      instantiate(parameters)
        .flatMap(tx => interp(actualDiagram).flatMap(tx.applyDiagram))
        .map(_.toTerm)

    case _ => None
  }
}