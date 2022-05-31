package info.kwarc.mmt.lf.itp

import info.kwarc.mmt.api.proving.itp.{Msg, Proof}

/**
  * this class represents a tactic
  */
trait Tactic {
  /**
    * method that executes the tactic
    * @param p for convenience this is passed separately even though it is containt in `ip`
    * @param ip the current proof manager
    * @return Error of an empty message
    */
  def applyToProof(p: Proof, ip: InteractiveProof): Msg


}


trait MetaTactic extends Tactic {
  def execute(p : Proof , ip : InteractiveProof) : Msg
}

trait ConcreteTactic extends Tactic {


}

trait ProofChangingTactic extends ConcreteTactic {
  abstract class ParamProofStateGeneration
  abstract class ReturnProofStateGeneration

  var cppsg : Option[ParamProofStateGeneration] = None
  var crpsg : Option[ParamProofStateGeneration] = None

  def genNewProofState() : Msg
}

trait TermGeneratingTactic extends ProofChangingTactic {
  abstract class ParamTermGeneration
  abstract class ReturnTermGeneration

  var cptg : Option[ParamTermGeneration] = None
  var crtg : Option[ReturnTermGeneration] = None

  def genTerm() : Msg

}




