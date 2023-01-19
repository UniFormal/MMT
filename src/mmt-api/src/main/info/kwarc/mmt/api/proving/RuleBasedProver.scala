package info.kwarc.mmt.api.proving

import info.kwarc.mmt.api._
import checking._
import objects._
import frontend._

/**
  * a simple prover
  */
class RuleBasedProver extends Prover {
   override val priority: Int = 10

   def apply(pu: ProvingUnit, rules: RuleSet, levels: Int) = {
      val gl = new Goal(pu.context, pu.tp)
      val searcher = new Searcher(controller, gl, rules, pu)
      val found = searcher(levels)
      val proof = if (found) Some(gl.proof) else None
      (found, proof)
   }

   def interactive(pu: ProvingUnit, rules: RuleSet, levels: Int): List[Term] = {
      val gl = new Goal(pu.context, pu.tp)
      val searcher = new Searcher(controller, gl, rules, pu)
      searcher.interactive(levels)
   }
}
