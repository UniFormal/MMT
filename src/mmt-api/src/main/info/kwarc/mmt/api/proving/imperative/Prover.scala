package info.kwarc.mmt.api.proving.imperative

import info.kwarc.mmt.api._
import objects._

class Prover {
   // for now: manually put all rules; eventually list generated from current MMT context
   private val rules: List[ProofStepRule] = Nil

   def prove(context: Context, goal: Term, proof: ImperativeProof) = {
     var state = new ProofState(context, goal)
     proof.steps foreach {step =>
       val ruleO = rules.find(_.applicable(step))
       ruleO match {
         case Some(r) => r(this)(state, step)
         case None => // report error
       }
     }
   }
}

class ProofState(initContext: Context, initGoal: Term) {
  var context = initContext
  var goal = initGoal
}

abstract class ProofStepRule {
  def applicable(step: ProofStep): Boolean
  def apply(prover: Prover)(state: ProofState, step: ProofStep): Unit
}

object CasesRule extends ProofStepRule {
  def applicable(step: ProofStep) = step match {case _:Cases => true case _ => false}

  def apply(prover: Prover)(state: ProofState, step: ProofStep) {
    val Cases(split, cases) = step
    // TODO proof split
    cases foreach {case (ass,pf) =>
      val assName = LocalName("a")
      val newCon = state.context ++ VarDecl(assName, ass)
      prover.prove(newCon, state.goal, pf)
    }
  }
}
