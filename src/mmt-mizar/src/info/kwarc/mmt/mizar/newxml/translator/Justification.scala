package info.kwarc.mmt.mizar.newxml.translator

import info.kwarc.mmt._
import mizar.newxml.syntax._
import api._


object justificationTranslator {
  def translate_Justification(just:Justification): Option[objects.Term] = just match {
    case Straightforward_Justification(pos, _refs) => None
    case Block(kind, pos, _items) =>
      //TODO: actually translate the proofs, may need additional arguments from the context, for instance the claim to be proven
      None
    case Scheme_Justification(posNr, idnr, schnr, spelling, _refs) => ???
  }
  def translate_Iterative_Equality_Proof(it: Iterative_Equality): objects.Term = { ??? }
}