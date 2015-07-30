package info.kwarc.mmt.leo.provers

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.proving.{Prover,ProvingUnit}
import info.kwarc.mmt.leo.AgentSystem.GoalSystem._
import info.kwarc.mmt.leo.AgentSystem._

class AgentProver extends Prover {
   def interactive(pu: ProvingUnit, rules: RuleSet, levels: Int) = Nil


   def apply(pu: ProvingUnit, rules: RuleSet, levels: Int) = {
      implicit val c=controller
      val gl = new Goal(pu.context, pu.tp)
      val blackboard = new GoalBlackboard(rules, gl)
      //val exa = new ExpansionAgent
      val sba = new SearchBackwardAgent
      val sfa = new SearchForwardAgent
      val aa = new AuctionAgent()
      val ea = new ExecutionAgent()
      sfa.register(blackboard)
      sba.register(blackboard)
      //exa.register(blackboard)
      aa.register(blackboard.asInstanceOf[aa.BBType])
      ea.register(blackboard.asInstanceOf[ea.BBType])

      blackboard.run(7)
      val found = blackboard.finished
      val proof = if (found) Some(gl.proof) else None
      (found, proof)
   }


}

