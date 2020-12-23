package info.kwarc.mmt.api.modules.diagops

import info.kwarc.mmt.api.{ComplexStep, GeneralError, InvalidElement, LocalName}
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Link, Theory, View}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMMOD, Term}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, RuleConstant, TermContainer}

/**
  * A [[LinearTransformer]] that works constant-by-constant: structural features are elaborated before calling
  * the to-be-implemented method ''applyConstant''.
  */
trait ElaboratingLinearTransformer extends LinearTransformer {
  protected def applyConstant(container: Container, c: Constant)(implicit interp: DiagramInterpreter, state: LinearState): Unit

  final override protected def applyDeclaration(container: Container, containerState: LinearState, decl: Declaration)(implicit interp: DiagramInterpreter, state: DiagramState): Unit = {
    decl match {
      case c: Constant => applyConstant(container, c)(interp, containerState)
      case _: RuleConstant =>
        NotApplicable(decl, "RuleConstants cannot be processed")(interp, containerState)
      case _ =>
        // do elaboration, then call applyConstant
        // interp.errorCont(InvalidElement(decl, s"Linear operator ${getClass} cannot process this element " +
        //s"of u"))
        ???
    }
  }
}

/**
  * Linearly transforms theories to theories and views to views,
  * while hiding much of the complexity of their contents.
  *
  * Implementors only need to give a ''applyConstantSimple'' method.
  */
trait SimpleLinearModuleTransformer extends LinearModuleTransformer
  with ElaboratingLinearTransformer with DefaultLinearStateOperator {
  type SimpleConstant = (LocalName, Term, Option[Term])

  /**
    *
    * @param c The constant
    * @param name A simplified version of ''c.name''. E.g. if c is an assignment in a view,
    *             ''c.name'' is something like ''LocalName(domainTheory) / actualConstantName'',
    *             which is uncomfortable to deal with in most cases.
    *             Hence in such cases, ''name'' will be simply be ''actualConstantName''.
    * @return In case the operator is not applicable on c, use the helper [[NotApplicable]] function:
    *         return ''NotApplicable(c, "optional error msg")''. Internally, this emits an error
    *         via interp.errorHandler and returns Nil.
    * @example Most operator implementations start like this:
    * {{{
    *   tp match {
    *     // more cases
    *     case _ =>
    *       NotApplicable(c)
    *   }
    * }}}
    *
    * The general convention is that operators produce as much output as possible.
    * For instance, consider an operator that copies every constant ''c'' to ''c^p'' and furthermore creates some ''c^x
    * in some fashion. Now the derivation of ''c^x'' might not be possible for some constants c. But even in these cases,
    * the opreator should output c (and signal inapplicability as explained above).
    *
    * @example Operators producing copies usually follow this pattern:
    * {{{
    *   val par : Renamer[LinearState] = getRenamerFor("p") // as a field on the operator object
    *   val parCopy = (par(name), par(tp), df.map(par(_)))
    *
    *   parCopy :: (tp match {
    *     // more cases
    *     case _ =>
    *       NotApplicable(c)
    *   })
    * }}}
    */
  protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[SimpleConstant]

  final override protected def applyConstant(container: Container, c: Constant)(implicit interp: DiagramInterpreter, state: LinearState): Unit = {
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
  }
}

trait SimpleLinearConnectorTransformer extends LinearConnectorTransformer with ElaboratingLinearTransformer with DefaultLinearStateOperator {

  /**
    * Maps a constant to a list of assignments in the connecting morphism.
    *
    * @return A list of assignments (simpleName, assignmentTerm), which is used by [[applyConstant]]
    *         to build a [[FinalConstant]] with the right name, empty type container, and a definiens container
    *         containing assignmentTerm.
    */
  protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter, state: LinearState): List[(LocalName, Term)]

  final override protected def applyConstant(container: Container, c: Constant)(implicit interp: DiagramInterpreter, state: LinearState): Unit = {
    val rawTp = c.tp.getOrElse({
      interp.errorCont(GeneralError(s"Operator $getClass not applicable on constants without type component"))
      return
    })
    val rawDf = c.df

    val tp = interp.ctrl.globalLookup.ExpandDefinitions(rawTp, state.skippedDeclarationPaths)
    val df = rawDf.map(interp.ctrl.globalLookup.ExpandDefinitions(_, state.skippedDeclarationPaths))

    applyConstantSimple(container, c, c.name, tp, df).foreach {
      case (name, df) =>
        interp.add(new FinalConstant(
          home = OMMOD(applyModulePath(container.modulePath)),
          name = name, alias = Nil,
          tpC = TermContainer.empty(), dfC = TermContainer.asAnalyzed(df),
          rl = None, notC = NotationContainer.empty(), vs = c.vs
        ))
    }
  }
}

