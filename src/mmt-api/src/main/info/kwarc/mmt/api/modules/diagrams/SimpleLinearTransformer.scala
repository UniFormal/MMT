package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.symbols.{Constant, FinalConstant}
import info.kwarc.mmt.api.{GeneralError, InvalidElement}

/**
  * Linearly transforms theories to theories and views to views,
  * while hiding much of the complexity of their contents.
  *
  * Implementors only need to give a ''applyConstantSimple'' method.
  */
trait SimpleConstantsBasedModuleTransformer extends LinearFunctorialTransformer {
  /**
    * Transforms a constant to a list of new constants.
    *
    * When defining this method, you can just create new [[Constant]] objects on-the-fly,
    * without adding them to the controller.
    * The caller ([[applyConstant()]]) takes care of this.
    *
    * If the transformer is not applicable on `c`, signal an error via [[NotApplicable]].
    * Still, even when signalling an error like that, for usability reasons, you should
    * output as much output as possible.
    * E.g. if your transformer usually mapped every constant `c` to List(c_p, c_x)`, where
    * `c_p` is a "parent" copy and `c_x` something else (possibly computed), then even if
    * your transformer was not able to compute `c_x`, you would still output at least `c_p` --
    * besides non-fatally erroring with [[NotApplicable]].
    *
    * @param c The constant
    * @return In case the operator is not applicable on c, use the helper [[NotApplicable]] function:
    *         return ''NotApplicable(c, "optional error msg")''. Internally, this emits an error
    *         via interp.errorHandler and returns Nil.
    *
    * @example Most operator implementations start like this:
    * {{{
    *   tp match {
    *     // more cases
    *     case _ =>
    *       NotApplicable(c)
    *   }
    * }}}
    *
    * @example Operators producing qualified copies usually follow this pattern:
    * {{{
    *   val par : Renamer[LinearState] = getRenamerFor("p") // as a field on the operator object
    *
    *   override protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(
    *     implicit state: LinearState, interp: DiagramInterpreter): List[Constant] = {
    *
    *     val parCopy = (par(name), par(tp), df.map(par(_)))
    *
    *     parCopy :: (tp match {
    *       // more cases
    *       case _ =>
    *         NotApplicable(c)
    *     })
    *   }
    * }}}
    */
  protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant]

  final override def applyConstant(c: Constant, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = {
    val rawTp = c.tp.getOrElse({
      interp.errorCont(InvalidElement(c, s"Transformer `$getClass` not applicable on constants without type component"))
      return
    })

    val tp = interp.ctrl.library.ExpandDefinitions(rawTp, state.skippedDeclarationPaths)
    val df = c.df.map(interp.ctrl.library.ExpandDefinitions(_, state.skippedDeclarationPaths))

    applyConstantSimple(c, tp, df).foreach(interp.add)
  }
}

trait SimpleLinearModuleTransformer extends SimpleConstantsBasedModuleTransformer with DefaultLinearStateOperator

trait SimpleLinearConnectorTransformer extends LinearConnectorTransformer with DefaultLinearStateOperator {

  /**
    * Maps a constant to a list of assignments in the connecting morphism.
    *
    * @return A list of assignments (simpleName, assignmentTerm), which is used by [[applyConstant]]
    *         to build a [[FinalConstant]] with the right name, empty type container, and a definiens container
    *         containing assignmentTerm.
    */
  protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant]

  final override def applyConstant(c: Constant, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = {
    val rawTp = c.tp.getOrElse({
      interp.errorCont(GeneralError(s"Connector `$getClass` not applicable on constants without type component"))
      return
    })

    val tp = interp.ctrl.library.ExpandDefinitions(rawTp, state.skippedDeclarationPaths)
    val df = c.df.map(interp.ctrl.library.ExpandDefinitions(_, state.skippedDeclarationPaths))

    val outConstants = applyConstantSimple(c, tp, df)
    outConstants.foreach(interp.add)
  }
}