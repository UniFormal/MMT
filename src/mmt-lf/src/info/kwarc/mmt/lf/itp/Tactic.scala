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
