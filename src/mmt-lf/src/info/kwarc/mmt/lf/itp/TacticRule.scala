package info.kwarc.mmt.lf.itp

import info.kwarc.mmt.api.{LocalName, Rule}
import info.kwarc.mmt.api.objects.{OMID, OMV, Term}
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.uom.ConstantScala


trait TokenTrait {
  def toTactic : TacticTrait
}

trait TacticTrait extends Constant with Tactic {

}

/**
  * class representing not hardcoded tactics
  */
abstract class TacticRule extends Rule {


  // type TT = TokenTrait

  /**
    * a tactic token is an intermediate representation of a tactic. The difference is that a tactic token has still
    * unparsed raw mmt strings in it
    */

  // val mmtTactic : T
  def ConcreteTacticParser : TacticParser#Parser[TacticTrait]



}
