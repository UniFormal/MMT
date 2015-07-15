package info.kwarc.mmt.leo.provers.lfprover

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.proving._

class TheoremProver extends Prover {
   def interactive(pu: ProvingUnit, rules: RuleSet, levels: Int) = Nil
   
   def apply(pu: ProvingUnit, rules: RuleSet, levels: Int) = (false, None)


}

