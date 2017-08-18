package info.kwarc.mmt.imps

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.PackratParsers

package object impsMathParser {
  def parseIMPSMath(s: String): Option[IMPSMathExp] = {
    val cheating = false

    if (cheating) {
      /* CHEATING! TODO: REMOVE */
      if (s == "\"[-1]\"") {
        Some(IMPSMathSymbol(s))
      }
      else if (s == "\"1\"") {
        Some(IMPSMathSymbol(s))
      }
      else if (s == "\"lambda(n:zz, n = [-1] or n = 1)\"") {
        val vs: List[(IMPSVar, Option[IMPSSortRef])] = List((IMPSVar("n"), Some(IMPSSortRef("zz"))))
        val p: IMPSMathExp = IMPSEquals(IMPSVar("n"), IMPSMathSymbol("[-1]"))
        val q: IMPSMathExp = IMPSEquals(IMPSVar("n"), IMPSMathSymbol("1"))
        val t: IMPSMathExp = IMPSDisjunction(List(p, q))
        Some(IMPSLambda(vs, t))
      }
      else if (s == "\"lambda(b:boole, b = true%val)\"") {
        val vs = List((IMPSVar("b"), Some(IMPSSortRef("boole"))))
        val t = IMPSEquals(IMPSVar("b"), IMPSMathSymbol("true%val"))
        Some(IMPSLambda(vs, t))
      }
      else if (s == "\"lambda(b:boole, b = false%val)\"") {
        val vs = List((IMPSVar("b"), Some(IMPSSortRef("boole"))))
        val t = IMPSEquals(IMPSVar("b"), IMPSMathSymbol("false%val"))
        Some(IMPSLambda(vs, t))
      }
      else if (s == "\"is%true(true%val) iff truth\"") {
        val p: IMPSMathExp = IMPSApply(IMPSMathSymbol("is%true"), List(IMPSMathSymbol("true%val")))
        Some(IMPSIff(p, IMPSTruth()))
      }
      else if (s == "\"is%true(false%val) iff falsehood\"") {
        val p: IMPSMathExp = IMPSApply(IMPSMathSymbol("is%true"), List(IMPSMathSymbol("false%val")))
        Some(IMPSIff(p, IMPSFalsehood()))
      }
      else if (s == "\"is%false(true%val) iff falsehood\"") {
        val p: IMPSMathExp = IMPSApply(IMPSMathSymbol("is%false"), List(IMPSMathSymbol("true%val")))
        Some(IMPSIff(p, IMPSFalsehood()))
      }
      else if (s == "\"is%false(false%val) iff truth\"") {
        val p: IMPSMathExp = IMPSApply(IMPSMathSymbol("is%false"), List(IMPSMathSymbol("false%val")))
        Some(IMPSIff(p, IMPSTruth()))
      }
      else if (s == "\"falsehood implies truth\"") {
        Some(IMPSImplication(IMPSFalsehood(), IMPSTruth()))
      }
      None
    }
    else {
      // "lambda(v:[zz,sets[ind_1]],a:sets[ind_1],forsome(m:zz,forall(x:ind_1,x in a implies forsome(n:zz,-m<=n and n<=m and x in v(n)))))"

      var store: Option[IMPSMathExp] = None
      var input: String = s

      input = input.trim
      if (input.startsWith("\"")) {
        input = input.drop(1)
      }
      if (input.endsWith("\"")) {
        input = input.dropRight(1)
      }
      input = input.trim

      /* This takes as reference the IMPS Manual pg. 65 */
      val k = new IMPSMathParser()
      val j = k.parseAll(k.parseMath, input)
      if (j.isEmpty) {
        println("COULD NOT PARSE: " + s)
        None
      } else {
        println("PARSED:          " + j.get.toString )
        Some(j.get)
      }
    }
  }

  /* PackratParsers because those can be left-recursive */
  class IMPSMathParser extends RegexParsers with PackratParsers
  {
    lazy val parseMath  : PackratParser[IMPSMathExp] =
    {
      /* Binding hierarchies taken from IMPS manual pg. 150*/
      parseDisjunction | parseConjunction | parseLambda | parseForAll | parseForSome | parseEquals | parseIff | parseImplies | parseNot | parseTruth | parseFalse
    }

    lazy val parseTruth : PackratParser[IMPSTruth]     = { "truth"     ^^ {_ => IMPSTruth()     } }
    lazy val parseFalse : PackratParser[IMPSFalsehood] = { "falsehood" ^^ {_ => IMPSFalsehood() } }

    lazy val parseNot : PackratParser[IMPSNegation]  = {
      ("not(" ~ parseMath ~ ")") ^^ { case _ ~ m ~ _ => IMPSNegation(m) }
    }

    lazy val parseIff : PackratParser[IMPSIff] = {
      (parseMath ~ "iff" ~ parseMath) ^^ { case m1 ~ _ ~ m2 => IMPSIff(m1,m2)}
    }

    lazy val parseImplies : PackratParser[IMPSImplication] = {
      (parseMath ~ "implies" ~ parseMath) ^^ { case m1 ~ _ ~ m2 => IMPSImplication(m1,m2) }
    }

    lazy val parseEquals : PackratParser[IMPSEquals] = {
      (parseMath ~ "=" ~ parseMath) ^^ { case m1 ~ _ ~ m2 => IMPSEquals(m1,m2)}
    }

    lazy val parseUndefined : PackratParser[IMPSUndefined] = {
      ("?" ~ parseMath) ^^ {case _ ~ m => IMPSUndefined(m)}
    }

    lazy val parseConjunction : PackratParser[IMPSConjunction] = {
      parseMath ~ "and" ~ rep1sep(parseMath,"and") ^^ { case c ~ _ ~ cs => IMPSConjunction(List(c) ::: cs) }
    }

    lazy val parseDisjunction : PackratParser[IMPSDisjunction] = {
      parseMath ~ "or" ~ rep1sep(parseMath,"or") ^^ { case c ~ _ ~ cs => IMPSDisjunction(List(c) ::: cs) }
    }

    lazy val parseLambda : PackratParser[IMPSLambda] = {
      (("lambda(" ~> rep1sep(parseVal,",") <~ ",") ~ (parseMath <~ ")")) ^^ { case (vrs ~ p) => IMPSLambda(vrs,p) }
    }

    lazy val parseForAll : PackratParser[IMPSForAll] = {
      (("forall(" ~> rep1sep(parseVal,",") <~ ",") ~ (parseMath <~ ")")) ^^ { case (vrs ~ p) => IMPSForAll(vrs,p) }
    }

    lazy val parseForSome : PackratParser[IMPSForSome] = {
      (("forsome(" ~> rep1sep(parseVal,",") <~ ",") ~ (parseMath <~ ")")) ^^ { case (vrs ~ p) => IMPSForSome(vrs,p) }
    }

    lazy val parseVal : PackratParser[(IMPSVar, Option[IMPSSortRef])] = {
      (("[^,:]+".r) <~ ":") ~ ("[^,]+".r.?) ^^ { case (name ~ sort) => (IMPSVar(name),sort.map(x => IMPSSortRef(x)))}
    }

  }

}