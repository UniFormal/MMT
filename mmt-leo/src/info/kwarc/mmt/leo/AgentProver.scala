package info.kwarc.mmt.leo

import info.kwarc.mmt.api._
import checking._
import proving._
import objects._
import frontend._

class AgentProver extends Prover {
   def interactive(pu: ProvingUnit, rules: RuleSet, levels: Int) = Nil
   
   def apply(pu: ProvingUnit, rules: RuleSet, levels: Int) = (false, None)


}