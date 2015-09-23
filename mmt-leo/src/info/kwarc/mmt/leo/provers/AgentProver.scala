package info.kwarc.mmt.leo.provers

import java.util.concurrent.TimeoutException

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.proving.{Prover, ProvingUnit}
import info.kwarc.mmt.leo.AgentSystem.MMTSystem._
import info.kwarc.mmt.leo.AgentSystem._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class AgentProver extends Prover {
   def interactive(pu: ProvingUnit, rules: RuleSet, levels: Int) = Nil

   def apply(pu: ProvingUnit, rules: RuleSet, levels: Int) = {
      implicit val c=controller
      implicit val oLP=pu.logPrefix
      val gl = new Goal(pu.context, pu.tp)
      val blackboard = new MMTBlackboard(rules, gl)
      //val exa = new ExpansionAgent
      val sba = new SearchBackwardAgent(blackboard)
      val sfa = new SearchForwardAgent(blackboard)
      val aa = new AuctionAgent(blackboard)
      val ea = new ExecutionAgent(blackboard)


      implicit val ec = ExecutionContext.global
      val f = Future {
         blackboard.run(levels)
      }
      try {
         Await.result(f, Duration(10000, "millis"))
      }catch{
         case e:TimeoutException =>
      }

      //blackboard.run(levels)

      val found = blackboard.finished
      if (found){
         log("FINISHED PROOF IN TIME")
      }else{
         blackboard.terminate()
         log("PROOF NOT FOUND")
      }
      val proof = if (found) Some(gl.proof) else None
      (found, proof)
   }


}



