package info.kwarc.mmt.oeis

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.notations._
import scala.collection._

object Arithmetics {
  implicit def makeNotation(s : String) = TextNotation.parse(s, NamespaceMap.empty)
  
  val constants = new mutable.HashMap[String, TextNotation]()
  constants("power") = "1 ^ 2 prec 100"
  constants("sqrt") = "√ 1 prec 1000"
  constants("plus") = "1+… prec 10"
  constants("minus") = "1-… prec 10"
  constants("unary_minus") = "- 1 prec 999"
  constants("divide") = "1 / 2 prec 2"
  constants("equal") = "1 = 2 prec 0"
  constants("=") = "1 = 2 prec 0"
  constants(">") = "1 > 2 prec 0"
  constants("<") = "1 < 2 prec 0"
  constants(">=") = "1 >= 2 prec 0"
  constants("<=") = "1 <= 2 prec 0"
  constants("divisble") = "1 | 2 prec 100"
  constants("times") = "1×… prec 2"
  constants("interval") = "[1,2] prec 2"
  constants("factorial") = "1 ! prec 1002"
  constants("set") = "⟮ 1,… ⟯  prec 2" 
  def getNotation(sym : String) : TextNotation = constants.get(sym).getOrElse {
    sym + "⟮ 1,… ⟯"
  }
  def contains(sym : String) = constants.isDefinedAt(sym)
  
}