package info.kwarc.mmt.lf.itp.Tactics

import info.kwarc.mmt.api.Rule
import info.kwarc.mmt.lf.itp.{Tactic, TacticParser}

trait InteractiveTacticParseRule extends Rule {
  def parseTactic(tp : TacticParser) : tp.Parser[Tactic]
}
