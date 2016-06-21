package info.kwarc.mmt.leo.provers

import java.util.concurrent.TimeoutException

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.proving.{Prover, ProvingUnit}
import info.kwarc.mmt.leo.AgentSystem.MMTSystem._
import info.kwarc.mmt.leo.AgentSystem._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class AgentProver extends Prover {
   def interactive(pu: ProvingUnit, rules: RuleSet, levels: Int) = Nil

   def apply(pu: ProvingUnit, rules: RuleSet, levels: Int): (Boolean, Option[Term]) = {
      if (levels==0) return (false, None)
      implicit val c=controller
      implicit val oLP=pu.logPrefix
      val gl = new Goal(pu.context, pu.tp)
      val blackboard = new MMTBlackboard(rules, gl)
      //val exa = new ExpansionAgent
      val sba = new SearchBackwardAgent(blackboard)
      val sfa = new SearchForwardAgent(blackboard)
      val ta = new TransitivityAgent(blackboard)
      val aa = new AuctionAgent(blackboard)
      val ea = new ExecutionAgent(blackboard)


      implicit val ec = ExecutionContext.global
      val timeout = 10000000

      val t0 = System.nanoTime()
      val f = Future {
         blackboard.run(levels)
      }
      try {
         Await.result(f, Duration(timeout, "millis"))
      }catch{
         case e:TimeoutException =>
      }

      //blackboard.run(levels)
      val found = blackboard.finished
      val t1 = System.nanoTime()

      if (found){
         val time = (t1-t0).toDouble/1000000000
         log("FINISHED PROOF IN TIME "+"Term: "+ pu.logPrefix +" Time: " + time + " Depth: " + blackboard.cycle)
      }else{
         blackboard.terminate()
         val time = timeout.toDouble/1000
         log("PROOF NOT FOUND "+"Term: "+ pu.logPrefix + " Time: " + time + " Depth: " + blackboard.cycle)
      }

      val proof = if (found) Some(gl.proof) else None
      (found, proof)
   }


}



