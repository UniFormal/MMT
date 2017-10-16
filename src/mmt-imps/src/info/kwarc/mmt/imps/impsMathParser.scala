package info.kwarc.mmt.imps

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.PackratParsers

package object impsMathParser {
  def parseIMPSMath(s: String): Option[IMPSMathExp] =
  {
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
      println("PARSED:          " + j.get.toString)
      Some(j.get)
    }
  }

  /* PackratParsers because those can be left-recursive */
  class IMPSMathParser extends RegexParsers with PackratParsers
  {
    lazy val parseMath  : PackratParser[IMPSMathExp] =
    {
      /* Binding hierarchies taken from IMPS manual pg. 150*/
      parseDisjunction | parseConjunction |
        parseLambda | parseForAll | parseForSome |
        parseEquals | parseIff | parseImplies | parseIfForm |
        parseNot | parseTruth | parseFalse | parseApply | parseSymbol
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

    lazy val parseIfForm : PackratParser[IMPSIfForm] = {
      ("if_form(" ~> parseMath <~ ",") ~ (parseMath <~ ",") ~ (parseMath <~ ")") ^^ { case (p ~ q ~ f) => IMPSIfForm(p,q,f)}
    }

    lazy val parseVal : PackratParser[(IMPSVar, Option[IMPSSort])] = {
      ("[^,:\\s]+".r ~ parseSortDecl) ^^ { case (name ~ sort) => (IMPSVar(name), Some(sort))}
    }

    lazy val parseSortDecl : PackratParser[IMPSSort] = { (":" ~ parseSort) ^^ {case (semi ~ sort) => sort} }

    lazy val parseSort : PackratParser[IMPSSort] = { parseFunSort | parseFunSort2 | parseAtomicSort }

    lazy val parseAtomicSort : PackratParser[IMPSAtomSort] = {
      ("[^,\\]):\\s]+".r) ^^ {case (sort) => IMPSAtomSort(sort)}
    }

    lazy val parseFunSort : PackratParser[IMPSNaryFunSort] = {
      "[" ~> rep1sep(parseSort,",") <~ "]" ^^ {case (sorts) => IMPSNaryFunSort(sorts)}
    }

    lazy val parseFunSort2 : PackratParser[IMPSNaryFunSort] = {
      "(" ~> rep1(parseSort) <~ ")" ^^ {case (sorts) => IMPSNaryFunSort(sorts)}
    }

    lazy val parseApply : PackratParser[IMPSApply] = {
      (parseSymbol ~ ("(" ~> rep1sep(parseSymbol, ",") <~ ")")) ^^ {case (fun ~ args) => IMPSApply(fun, args)}
    }

    lazy val parseSymbol : PackratParser[IMPSMathSymbol] = {
      ("[^(),\\s]+".r)  ^^ {case (sym) => IMPSMathSymbol(sym)}
    }

  }

}