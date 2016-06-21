package info.kwarc.mmt.tptp

import tptp.TptpParserOutput.BinaryConnective._
import tptp.TptpParserOutput.Quantifier._
import tptp.TptpParserOutput._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._

/**
 * Utility stuff.
 */
object TptpUtils {
  
  val LOG = false
  
  val baseURI = URI("http", "tptp.org")
  
  // meta theories
	val fofTh = DPath(URI("http", "latin.omdoc.org") / "logics" / "tptp") ? "FOF"
  
  val term = OMID(fofTh ? "$term")
  val form = OMID(fofTh ? "$form")
  val t = OMID(fofTh ? "$true")
  val f = OMID(fofTh ? "$false")
  val equals = OMID(fofTh ? "==")

  val PARSE_DIRS = List("Axioms", "Problems")
  val PARSE_EXTS = List("ax", "p")
  val FORM = "+"; // fof
  
  val OPERATORS = Map(
    And -> "&",
    Or -> "|",
    "not" -> "~",
    Equivalence -> "<=>",
    Implication -> "=>",
    ReverseImplication -> "<=",
    Disequivalence -> "<~>",
    NotOr -> "~|",
    NotAnd -> "~&",
    ForAll -> "!",
    Exists -> "?"
  )
  val UNKNOWN = "tmp"

  /**
   * @param s string of the form DDD NNN F V [.SSS] .T[T]
   * @return DDD NNN part of the file name
   */
  def filePrefix(s: String) = s.substring(0, 7)

  /**
   * @param s string of the form DDD NNN F V [.SSS] .T[T]
   * @param form the F part (+ for fof)
   * @return V part of the file name
   */
  def fileVersion(s: String) = s.substring(s.indexOf(FORM) + 1, s.indexOf("."))

  def removeExtension(s: String) = s.substring(0, s.lastIndexOf("."))

	def constant(name : String) : OMID = OMID(fofTh ? OPERATORS.getOrElse(name, UNKNOWN))
	def constant(name : BinaryConnective) : OMID = OMID(fofTh ? OPERATORS.getOrElse(name, UNKNOWN))
	def constant(name : Quantifier) : OMID = OMID(fofTh ? OPERATORS.getOrElse(name, UNKNOWN))
  
  def log(s: String) {
    if (LOG)
      println(s)
  }
}
