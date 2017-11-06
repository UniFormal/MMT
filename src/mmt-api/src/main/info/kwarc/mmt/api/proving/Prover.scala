package info.kwarc.mmt.api.proving

import info.kwarc.mmt.api._
import checking._
import objects._
import frontend._


/**
 * represents a proof obligation
 * @param component the URI of the declaration component that triggered this proof obligation
 * @param context the left-hand side/antecedent, i.e., the global (via [[IncludeVarDecl]]) and local (via [[VarDecl]]) assumptions
 * @param tp the type to be inhabited, i.e., the right-hand side/succedent, i.e., the proof goal
 * @param logPrefix the log prefix to use
 */
case class ProvingUnit(component: Option[CPath], context: Context, tp: Term, logPrefix: String) extends MMTTask

/**
 * A prover conducts the proof search.
 */
abstract class Prover extends Extension {
   /**
    * tries to prove a proof obligation automatically
    * @param rules the proof rules to use
    * @param levels the depth of the breadth-first searches
    * @return true if the goal was solved and possibly a proof term
    */
   def apply(pu: ProvingUnit, rules: RuleSet, levels: Int): (Boolean, Option[Term])
   
   /**
    * primitive function for building interactive provers
    * @param levels the search depth for forward search 
    * @return the list of possible first steps (possibly with holes)
    */
   def interactive(pu: ProvingUnit, rules: RuleSet, levels: Int): List[Term]
}