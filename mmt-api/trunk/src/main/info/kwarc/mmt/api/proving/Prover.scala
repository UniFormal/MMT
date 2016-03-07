package info.kwarc.mmt.api.proving

import info.kwarc.mmt.api._
import checking._
import objects._
import frontend._


/**
 * represents a proof obligation
 * @param context the left-hand side/antecedent
 * @param tp the type to be inhabited, i.e., the right-hand side/succedent
 */
case class ProvingUnit(component: Option[CPath], context: Context, tp: Term, logPrefix: String)

/**
 * A prover conducts the proof search. A new instance is created for each proof obligation.
 * 
 * @param goal the goal to prove
 * @param intros the backward tactics to use
 * @param elims  the forward  tactics to use
 * 
 * A prover greedily applies invertible tactics to each new goal (called the expansion phase).
 * Then forward and backward breadth-first searches are performed in parallel.
 */
abstract class Prover extends Extension {

   /**
    * tries to prove a proof obligation
    * @param levels the depth of the breadth-first searches
    * @return true if the goal was solved
    */
   def apply(pu: ProvingUnit, rules: RuleSet, levels: Int): (Boolean, Option[Term])
   
   /**
    * a list of possible steps to be used in an interactive proof
    * @param levels the search depth for forward search 
    * @return the list of possible steps (possibly with holes)
    */
   def interactive(pu: ProvingUnit, rules: RuleSet, levels: Int): List[Term]
}