package info.kwarc.mmt.api.proving

import info.kwarc.mmt.api._
import checking._
import objects._
import frontend._

class RuleBasedProver extends Prover {
   /**
    * tries to prove a proof obligation
    * @param levels the depth of the breadth-first searches
    * @return true if the goal was solved
    */
   def apply(pu: ProvingUnit, rules: RuleSet, levels: Int) = {
      val gl = new Goal(pu.context, pu.tp)
      val searcher = new Searcher(controller, gl, rules, pu)
      val found = searcher(levels)
      val proof = if (found) Some(gl.proof) else None
      (found, proof)
   }
   
   /**
    * a list of possible steps to be used in an interactive proof
    * @param levels the search depth for forward search 
    * @return the list of possible steps (possibly with holes)
    */
   def interactive(pu: ProvingUnit, rules: RuleSet, levels: Int): List[Term] = {
      val gl = new Goal(pu.context, pu.tp)
      val searcher = new Searcher(controller, gl, rules, pu)
      searcher.interactive(levels)
   }
}