package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import objects._
import proving.imperative._

case class Fix(name: List[LocalName]) extends ProofStep

object FixRule extends ProofStepRule {
  def applicable(step: ProofStep) = step match {case _:Fix => true case _ => false}

  def apply(prover: Prover)(state: ProofState, step: ProofStep) {
    val Fix(ns) = step
    // TODO split state.goal as Arrow(from,to), Pi(newCond, newGoal), or maybe even Ded(Forall(newCon,newGoal))
    val newCon: Context = ???
    val newGoal = ???
    state.context = state.context ++ newCon
    state.goal = newGoal
  }
}

