package info.kwarc.mmt.api.proving.imperative

import info.kwarc.mmt.api._
import objects._

case class ImperativeProof(steps: List[ProofStep])

abstract class ProofStep {

}

case class Assume(name: LocalName, tp: Term) extends ProofStep
case class Let(name: List[LocalName], tp: Option[Term], df: Term) extends ProofStep
case class Hence(tp: Term, by: ImperativeProof, name: Option[LocalName]) extends ProofStep

case class Suffices(newGoal: Term, by: ImperativeProof) extends ProofStep
case class Cases(splitOn: Term, cases: List[(Term,ImperativeProof)]) extends ProofStep
case class LocalRef(name: LocalName) extends ProofStep
case class ProofTerm(pf: Term) extends ProofStep

