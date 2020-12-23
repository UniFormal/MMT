package info.kwarc.mmt.api.modules.diagops

import info.kwarc.mmt.api.{ComplexStep, GeneralError, InvalidElement, LocalName}
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Link, Theory, View}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMMOD, Term}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, RuleConstant, TermContainer}

/**
  * Linearly transforms theories to theories and views to views,
  * while hiding much of the complexity of their contents.
  *
  * Implementors only need to give a ''applyConstantSimple'' method.
  */
trait SimpleLinearModuleTransformer extends LinearModuleTransformer with DefaultLinearStateOperator {
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

  final override protected def applyConstant(c: Constant, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = {
    val rawTp = c.tp.getOrElse({
      interp.errorCont(InvalidElement(c, s"Transformer `$getClass` not applicable on constants without type component"))
      return
    })

    val tp = interp.ctrl.globalLookup.ExpandDefinitions(rawTp, state.skippedDeclarationPaths)
    val df = c.df.map(interp.ctrl.globalLookup.ExpandDefinitions(_, state.skippedDeclarationPaths))

    applyConstantSimple(c, tp, df).foreach(interp.add)
  }

/*
    // todo(FR said): not sure if simplify/complexify name worth it/really needed.
    // Since [[applyConstantSimple()]] takes a simplified name and outputs a simplified name again
    // we need to functions for simplifying and complexifying again:
    def simplifyName(name: LocalName) = container match {
      case _: Theory => name

      // view or structure
      case link: Link => name match {
        case LocalName(ComplexStep(mpath) :: domainSymbolName) if mpath == link.from.toMPath =>
          LocalName(domainSymbolName)
        case _ => name // fallback
      }
    }

    def complexifyName(name: LocalName) = container match {
      case _: Theory => name
      // todo: vvv this is wrong!  vvvvvvvvvvvvvvvvv might not be the same as ComplexStep(mpath) from above
      case link: Link => LocalName(link.from.toMPath) / name
    }

    val rawTp = c.tp.getOrElse({
      interp.errorCont(InvalidElement(c, s"Operator $getClass not applicable on constants without type component"))
      return
    })
    val rawDf = c.df

    val tp = interp.ctrl.globalLookup.ExpandDefinitions(rawTp, state.skippedDeclarationPaths)
    val df = rawDf.map(interp.ctrl.globalLookup.ExpandDefinitions(_, state.skippedDeclarationPaths))

    applyConstantSimple(container, c, simplifyName(c.name), tp, df).foreach {
      case (name, newTp, newDf) =>
        if (container.isInstanceOf[View] && newDf.isEmpty) {
          throw GeneralError(s"applyConstant of SimpleLinearOperator subclass ${this.getClass} returned empty definiens for view declaration ${c.path}")
        }

        // todo(FR said): use Constant apply method
        interp.add(new FinalConstant(
          home = OMMOD(applyModulePath(container.modulePath)),
          name = complexifyName(name), alias = Nil,
          tpC = TermContainer.asAnalyzed(newTp), dfC = TermContainer.asAnalyzed(newDf),
          rl = None, notC = NotationContainer.empty(), vs = c.vs
        ))
    }
  }*/
}

trait SimpleLinearConnectorTransformer extends LinearConnectorTransformer with DefaultLinearStateOperator {

  /**
    * Maps a constant to a list of assignments in the connecting morphism.
    *
    * @return A list of assignments (simpleName, assignmentTerm), which is used by [[applyConstant]]
    *         to build a [[FinalConstant]] with the right name, empty type container, and a definiens container
    *         containing assignmentTerm.
    */
  protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit state: LinearState, interp: DiagramInterpreter): List[Constant]

  final override protected def applyConstant(c: Constant, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = {
    val rawTp = c.tp.getOrElse({
      interp.errorCont(GeneralError(s"Connector `$getClass` not applicable on constants without type component"))
      return
    })

    val tp = interp.ctrl.globalLookup.ExpandDefinitions(rawTp, state.skippedDeclarationPaths)
    val df = c.df.map(interp.ctrl.globalLookup.ExpandDefinitions(_, state.skippedDeclarationPaths))

    val outConstants = applyConstantSimple(c, tp, df)
    outConstants.foreach(interp.add)
  }
}