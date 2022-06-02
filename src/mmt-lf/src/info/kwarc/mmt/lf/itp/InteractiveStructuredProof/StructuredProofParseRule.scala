package info.kwarc.mmt.lf.itp.InteractiveStructuredProof

import info.kwarc.mmt.api.Rule
import info.kwarc.mmt.lf.itp.{InteractiveProof, Tactic}

trait StructuredProofParseRule extends Rule {
  var fixity : Fixity
  def parseTactic(sp : StructuredProofParser , ip : InteractiveProof) : sp.Parser[Tactic]
}
