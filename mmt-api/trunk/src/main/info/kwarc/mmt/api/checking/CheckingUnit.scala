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
 * @param unknowns the unknowns parts of the expressions that should be inferred during validation
 * @param judgement the typing judegment to validate
 */
case class CheckingUnit(component: CPath, unknowns: Context, judgement: WFJudgement)
