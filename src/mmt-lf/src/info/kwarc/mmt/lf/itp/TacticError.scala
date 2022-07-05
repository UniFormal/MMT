package info.kwarc.mmt.lf.itp

/** this class extends the Exceptions with a custom Exception signaling an error in the prover
  *
  * @param s the errormessage
  */
case class TacticError(s  : String ) extends Exception {

}