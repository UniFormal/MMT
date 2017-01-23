package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import objects._
import frontend._
import parser._

/**
 * A checking unit encapsulates the proof obligations produced by a [[StructureChecker]]
 * and passed on to an [[ObjectChecker]].
 * 
 * Typically, each checking unit checks a single term that is part of a WFJudgement, i.e.,
 * the other parts of the judgement are assumed to be valid.
 * 
 * @param component the term component that is validated, e.g., namespace?theory?symbol?type
 * @param context the constant context
 * @param unknowns the unknown context
 * @param judgement the typing judgement to check
 * 
 * A checking unit involves three contexts, which must be separated because they correspond to a quantifier alternation.
 * The constant context is the (universally quantified) global context that does not change during checking.
 * It includes in particular the theory relative to which a unit is formed.
 * The unknown context is the (existentially quantified) context of unknowns that are to be solved during checking.
 * The variable context is the context that arises from traversing binders during checking.
 * It changes during checking and is therefore stored within the judgement.
 */
case class CheckingUnit(component: Option[CPath], context: Context, unknowns: Context, judgement: WFJudgement) extends MMTTask {
  /** a toString method that may call a continuation on its objects
   */
  def present(implicit cont: Obj => String) = component.map(_.toString + ": ").getOrElse("") + judgement.present
}

object CheckingUnit {
   val unknownType = LocalName("") / "omitted_type"
   def byInference(cpath: Option[CPath], context: Context, unkt: Term): CheckingUnit = {
      val pr = ParseResult.fromTerm(unkt)
      byInference(cpath, context, pr)
   }
   
   def byInference(cpath: Option[CPath], context: Context, pr: ParseResult): CheckingUnit = {
      val j = Typing(Stack(pr.free), pr.term, OMV(unknownType))
      CheckingUnit(cpath, context, pr.unknown ++ VarDecl(unknownType,None,None,None), j)
   }
}

/**
 * A checking result encapsulates all information returned by an [[ObjectChecker]].
 * 
 * See [[CheckingUnit]].
 * 
 * @param solved true if term was checked successfully
 * @param term the checked version of the term (possibly approximate if check failed)
 * @param solution the substitution for the unknown context (possibly partial)
 */
case class CheckingResult(solved: Boolean, solution: Option[Context], term: Term) {
  def remainingUnknowns: Option[Context] = solution.map{sol => sol.filter {vd => vd.df.isEmpty}}
}