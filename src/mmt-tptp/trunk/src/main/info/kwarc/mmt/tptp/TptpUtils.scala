/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package info.kwarc.mmt.tptp

import tptp.TptpParserOutput.BinaryConnective._
import tptp.TptpParserOutput.Quantifier._

object TptpUtils {


    val PARSE_DIRS = List("Axioms", "Problems")
    val PARSE_EXTS = List("ax", "p")
    val FORM = "+"; // fof
    val BASE_THEORY = "tptp";
    val UNKNOWN_THEORY = "unknown";
    val OPERATORS = Map(
      And -> "and",
      Or -> "or",
      "not" -> "not",
      Equivalence -> "equivalent",
      Implication -> "implies",
      ReverseImplication -> "implied",
      Disequivalence -> "xor",
      NotOr -> "nor",
      NotAnd -> "nand",
      ForAll -> "forall",
      Exists -> "exists"
    )

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
    def fileVersion(s: String) =
        s.substring(s.indexOf(FORM) + 1, s.indexOf("."))
}
