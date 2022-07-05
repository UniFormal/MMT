package info.kwarc.mmt.lf.itp.Tactics

import info.kwarc.mmt.api.Rule
import info.kwarc.mmt.lf.itp.{Tactic, TacticParser}

/**
  * this rule is used to implement new tactics (to be more precise it is used to implement a parser which generates a tactic)
  */
trait InteractiveTacticParseRule extends Rule {
  /**
    * the parser to be implemented. Due to how the parser [[TacticParser]] is implmemented and due to how path dependent types
    * work, it is necessary to import the passed [[TacticParser]] tp
    * @param tp
    * @return
    */
  def parseTactic(tp : TacticParser) : tp.Parser[Tactic]
}
