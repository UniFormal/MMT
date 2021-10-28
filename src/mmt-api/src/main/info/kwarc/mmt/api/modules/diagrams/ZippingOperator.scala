package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.ContentPath
import info.kwarc.mmt.api.libraries.Library
import info.kwarc.mmt.api.symbols.{Constant, Declaration, IncludeData, Structure}

/**
  * The diagram operator that applies a list of [[LinearOperator]]s in parallel.
  *
  * Concretely, diagrams are gone through module-by-module (in dependency order)
  * and declaration-by-declaration: for every declaration `d`, all `operators` are
  * consecutively applied to it (in the order given by the list).
  *
  * The result of `applyDiagram()` is the union of all operators' results, when no `focussedOp` is selected.
  * If a `focussedOp` is selected from the list of operators, the result will only be this operator's result.
  *
  * @param focussedOp An optional index `0 <= focussedOp < operators.size - 1`.
  *
  * @example You can use the `_ :: _` notation to chain multiple operators in parallel:
  *          {{{
  *          op0 :: op1 :: op2
  *          // if we only want op2 results to be returned by applyDiagram():
  *          (op0 :: op1 :: op2).withFocus(2)
  *          }}}
  *
  * Zipping operators is necessitated whenever multiple operators are interdependent:
  * @example The pushout operation carried out by [[GenericPushoutOperator]] utilizing the [[PushoutFunctor]] and the
  *          [[PushoutConnector]] needs to apply both of the latter operators in parallel for the following reason:
  *          The functor maps every theory `T` to, say, `Push(T)`, and the connector maps every theory `T` to, say, a
  *          view `PushConn(T): T -> Push(T)`. Now to construct `Push(T)` declaration-by-declaration, the functor's
  *          method [[PushoutFunctor.applyConstant()]] computes the image of some terms under the view `PushConn(T)`.
  *          Thus, `Push(T)` and `PushConn(T)` need to be created in parallel.
  *
  * TODO (WARNING): the input diagram must be ordered by dependency already due to implementation details; is this
  *    warning still current?
  *
  *
  */
class ZippingOperator(operators: List[LinearOperator], focussedOp: Option[Integer] = None) extends LinearOperator {
  require(focussedOp.forall(idx => 0 <= idx && idx < operators.size))

  /**
    * Zip with yet another [[LinearOperator]].
    *
    * NB: Overriding this method from [[LinearOperator.::()]] is necessary to avoid bracketing, e.g.,
    * this guarantees that `op0 :: op1 :: op2` really results in `ZippingOperator(List(op0, op1, op2))`
    * (and not in `ZippingOperator(List(op0, op1, ZippingOperator(List(op2))))` or some other variation)!
    */
  override def ::(first: LinearOperator): ZippingOperator = {
    new ZippingOperator(first :: operators, focussedOp.map(_ + 1))
  }

  /**
    * Creates a new [[ZippingOperator]] with the same operators, but with a (new) focus.
    */
  def withFocus(newFocus: Integer): ZippingOperator = {
    if (newFocus >= 0) {
      require(0 <= newFocus && newFocus < operators.size, "new focus out of bounds")
      new ZippingOperator(operators, Some(newFocus))
    } else {
      require(newFocus >= -operators.size, "new focus out of bounds")
      withFocus(operators.size + newFocus)
    }
  }

  override def applyDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Option[Diagram] = {
    implicit val library: Library = interp.ctrl.library

    if (beginDiagram(diag)) {
      diag.modules.map(interp.ctrl.getModule).foreach(applyContainer)
      endDiagram(diag)

      // TODO: hacky workaround here by Navid:
      //   to collect all output diagrams, we employ the hack to call applyDiagram
      //   on every operator. This assumes that things are not recomputed, otherwise we're
      //   pretty inefficient
      Some(focussedOp.map(operators(_)).flatMap(_.applyDiagram(diag)).getOrElse(
        Diagram.union(operators.flatMap(_.applyDiagram(diag)))
      ))
    } else {
      None
    }
  }

  override def beginDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Boolean =
    operators.forall(_.beginDiagram(diag))

  override def endDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Unit =
    operators.foreach(_.endDiagram(diag))

  override def initState(container: Container): Unit =
    operators.foreach(_.initState(container))

  override def inheritState(into: ContentPath, from: ContentPath): Unit =
    operators.foreach(_.inheritState(into, from))

  override def registerSeenDeclaration(d: Declaration): Unit =
    operators.foreach(_.registerSeenDeclaration(d))

  override def registerSkippedDeclarations(d: Declaration): Unit =
    operators.foreach(_.registerSkippedDeclarations(d))

  override def beginContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Boolean =
    operators.forall(_.beginContainer(inContainer))

  override def endContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Unit =
    operators.foreach(_.endContainer(inContainer))

  override def applyDeclaration(decl: Declaration, container: Container)(implicit interp: DiagramInterpreter): Unit =
    operators.foreach(_.applyDeclaration(decl, container))

  override def applyConstant(c: Constant, container: Container)(implicit interp: DiagramInterpreter): Unit =
    require(requirement = false, "unreachable")

  override def applyIncludeData(include: IncludeData, structure: Structure, container: Container)(implicit interp: DiagramInterpreter): Unit =
    require(requirement = false, "unreachable")
}
