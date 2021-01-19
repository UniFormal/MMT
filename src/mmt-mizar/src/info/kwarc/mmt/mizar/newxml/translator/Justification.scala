package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt._
import api._
import objects.Context
import mizar.newxml.syntax._

object justificationTranslator {
  def translate_Justification(just:Justification, claim: objects.Term)(implicit args: Context): Option[objects.Term] = just match {
    case Straightforward_Justification(pos, _refs) => None
    case Block(kind, pos, _items) =>
      //TODO: actually translate the proofs, may need additional arguments from the context, for instance the claim to be proven
      None
    case Scheme_Justification(posNr, idnr, schnr, spelling, _refs) => ???
  }
  def translate_Iterative_Equality_Proof(it: Iterative_Equality)(implicit args: Context): objects.Term = { ??? }
  def translate_Proved_Claim(provedClaim: ProvedClaim)(implicit args: Context) = {
    provedClaim.check()
    val claim = claimTranslator.translate_Claim(provedClaim._claim)
    val prf = (provedClaim._claim, provedClaim._just) match {
      case (_, Some(just)) => translate_Justification(just, claim)
      case (it: Iterative_Equality, None) => Some(translate_Iterative_Equality_Proof(it))
      case (claim, None) => throw ProvedClaimTranslationError("No proof given for claim, which is not an iterative-equality (proving itself). ", provedClaim)
    }
    (claim, prf)
  }
}