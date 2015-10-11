package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import objects._
import frontend._

/**
 * A validation unit encapsulates the proof obligations produced by the [[MMTStructureChecker]] and passed on to the [[Solver]].
 * 
 * Each validation unit validates a single term that is part of a WFJudgement, i.e.,
 * the other parts of the judgement are assumed to be valid.
 * 
 * @param component the term component that is validated, e.g., namespace?theory?symbol?type
 * @param context the constant context
 * @param unknowns the unknown context
 * @param judgement the typing judgement to validate
 * 
 * A checking unit involves three contexts, which must be separated because they correspond to a quantifier alternation.
 * The constant context is the (universally quantified) global context that does not change during checking.
 * It includes in particular the theory relative to which a unit is formed.
 * The unknown context is the (existentially quantified) context of unknowns that are to be solved during checking.
 * The variable context is the context that arises from traversing binders during checking.
 * It changes during checking and is therefor stored within the judgement.
 */
case class CheckingUnit(component: CPath, context: Context, unknowns: Context, judgement: WFJudgement) {
  /** a toString method that may call a continuation on its objects
   */
  def present(implicit cont: Obj => String) = component.toString + ": " + judgement.present
}
