package info.kwarc.mmt.imps

import com.sun.xml.internal.bind.v2.runtime.output.StAXExStreamWriterOutput

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.PackratParsers

package object impsMathParser
{
  def parseSEXPFormula(sexp : SEXP) : IMPSMathExp =
  {
    sexp match {
      case s@SEXPAtom(name)   => IMPSVar(name)
      case s@SEXPNested(args) => args.head match
      {
        /* See imps manual, page 64 */

        case SEXPAtom("truth")              => IMPSTruth()
        case SEXPAtom("falsehood")          => IMPSFalsehood()
        case SEXPAtom("not")                => makeNot(s)
        case SEXPAtom("and")                => makeConjunction(s)
        case SEXPAtom("or")                 => makeDisjunction(s)
        case SEXPAtom("implies")            => makeImplication(s)
        case SEXPAtom("iff")                => makeIff(s)
        case SEXPAtom("if_form")            => ??? // probably defunct
        case SEXPAtom("forall")             => makeForall(s)
        case SEXPAtom("forsome")            => makeForSome(s)
        case SEXPAtom("=")                  => makeEquals(s)
        case SEXPAtom("apply-operator")     => makeApplication(s)
        case SEXPAtom("lambda")             => makeLambda(s)
        case SEXPAtom("iota")               => makeIota(s)
        case SEXPAtom("iota_p")             => ??? // probably defunct
        case SEXPAtom("if")                 => makeIf(s)
        case SEXPAtom("is-defined")         => makeIsDefined(s)
        case SEXPAtom("is-defined-in-sort") => makeIsDefinedInSort(s)
        case SEXPAtom("undefined")          => makeUndefined(s)

        /* Quasi-constructors */

        case SEXPAtom("total?")             => ???
      }
    }
  }

  def makeNot(sexp : SEXPNested) : IMPSNegation =
  {
    assert(sexp.args.head == SEXPAtom("not"))
    assert(sexp.args.length == 2)

    val recur : IMPSMathExp = parseSEXPFormula(sexp.args(1))
    return IMPSNegation(recur)
  }

  def makeConjunction(sexp : SEXPNested) : IMPSConjunction =
  {
    assert(sexp.args.head == SEXPAtom("and"))
    assert(sexp.args.length >= 3)

    val recurs : List[IMPSMathExp] = sexp.args.tail.map(parseSEXPFormula)

    return IMPSConjunction(recurs)
  }

  def makeDisjunction(sexp : SEXPNested) : IMPSDisjunction =
  {
    assert(sexp.args.head == SEXPAtom("or"))
    assert(sexp.args.length >= 3)

    val recurs : List[IMPSMathExp] = sexp.args.tail.map(parseSEXPFormula)

    return IMPSDisjunction(recurs)
  }

  def makeImplication(sexp : SEXPNested) : IMPSImplication =
  {
    assert(sexp.args.head == SEXPAtom("implies"))
    assert(sexp.args.length == 3)

    val recur1 : IMPSMathExp = parseSEXPFormula(sexp.args(1))
    val recur2 : IMPSMathExp = parseSEXPFormula(sexp.args(2))

    return IMPSImplication(recur1, recur2)
  }

  def makeEquals(sexp : SEXPNested) : IMPSEquals =
  {
    assert(sexp.args.head == SEXPAtom("="))
    assert(sexp.args.length == 3)

    val recur1 : IMPSMathExp = parseSEXPFormula(sexp.args(1))
    val recur2 : IMPSMathExp = parseSEXPFormula(sexp.args(2))

    return IMPSEquals(recur1, recur2)
  }

  def makeIff(sexp : SEXPNested) : IMPSIff =
  {
    assert(sexp.args.head == SEXPAtom("iff"))
    assert(sexp.args.length == 3)

    val recur1 : IMPSMathExp = parseSEXPFormula(sexp.args(1))
    val recur2 : IMPSMathExp = parseSEXPFormula(sexp.args(2))

    return IMPSIff(recur1, recur2)
  }

  def makeIf(sexp : SEXPNested) : IMPSIf =
  {
    assert(sexp.args.head == SEXPAtom("if"))
    assert(sexp.args.length == 4)

    val recur1 : IMPSMathExp = parseSEXPFormula(sexp.args(1))
    val recur2 : IMPSMathExp = parseSEXPFormula(sexp.args(2))
    val recur3 : IMPSMathExp = parseSEXPFormula(sexp.args(3))

    return IMPSIf(recur1, recur2, recur3)
  }

  /* Three possibilities: (ind_1 prop), [ind_1 prop] (?), ind_1 */
  def makeSort(sexp : SEXP) : IMPSSort =
  {
    sexp match
    {
      case SEXPAtom(name)   => return IMPSAtomSort(name)
      case SEXPNested(args) =>
      {
        assert(args.forall(a => a.isInstanceOf[SEXPAtom]))
        val sorts : List[IMPSSort] = args.map(makeSort)

        return IMPSNaryFunSort(sorts)
      }
    }
  }

  def makeSortDecls(sexp : SEXPNested) : List[(IMPSVar, Option[IMPSSort])] =
  {
    def oneSortDecl(sexp : SEXPNested) : List[(IMPSVar, Option[IMPSSort])] =
    {
      val theSort : IMPSSort = makeSort(sexp.args.head)
      assert(sexp.args.tail.forall(a => a.isInstanceOf[SEXPAtom]))

      return sexp.args.tail.map(a => a match { case SEXPAtom(n) => (IMPSVar(n), Some(theSort))})
    }

    assert(sexp.args.forall(a => a.isInstanceOf[SEXPNested]))
    return sexp.args.map(a => a match { case t@SEXPNested(_) => oneSortDecl(t) }).flatten
  }

  def makeForall(sexp : SEXPNested) : IMPSForAll =
  {
    assert(sexp.args.head == SEXPAtom("forall"))
    assert(sexp.args.length == 3)

    val sorts : List[(IMPSVar, Option[IMPSSort])] = sexp.args(1) match
    {
      case SEXPAtom(_)     => ???
      case s@SEXPNested(_) => makeSortDecls(s)
    }

    val target : IMPSMathExp = parseSEXPFormula(sexp.args.last)

    return IMPSForAll(sorts,target)
  }

  def makeForSome(sexp : SEXPNested) : IMPSForSome =
  {
    assert(sexp.args.head == SEXPAtom("forsome"))
    assert(sexp.args.length == 3)

    val sorts : List[(IMPSVar, Option[IMPSSort])] = sexp.args(1) match
    {
      case SEXPAtom(_)     => ???
      case s@SEXPNested(_) => makeSortDecls(s)
    }

    val target : IMPSMathExp = parseSEXPFormula(sexp.args.last)

    return IMPSForSome(sorts,target)
  }

  def makeLambda(sexp : SEXPNested) : IMPSLambda =
  {
    assert(sexp.args.head == SEXPAtom("lambda"))
    assert(sexp.args.length == 3)

    val sorts : List[(IMPSVar, Option[IMPSSort])] = sexp.args(1) match
    {
      case SEXPAtom(_)     => ???
      case s@SEXPNested(_) => makeSortDecls(s)
    }

    val target : IMPSMathExp = parseSEXPFormula(sexp.args.last)

    return IMPSLambda(sorts,target)
  }

  def makeApplication(sexp : SEXPNested) : IMPSApply =
  {
    assert(sexp.args.head == SEXPAtom("apply-operator"))
    assert(sexp.args.length >= 3)

    val recurs : List[IMPSMathExp] = sexp.args.tail.map(parseSEXPFormula)

    return  IMPSApply(recurs.head, recurs.tail)
  }

  def makeIota(sexp : SEXPNested) : IMPSIota =
  {
    assert(sexp.args.head == SEXPAtom("iota"))
    assert(sexp.args.length == 3)

    assert(sexp.args(1).isInstanceOf[SEXPNested])
    val vars  : List[(IMPSVar, IMPSSort)] = sexp.args(1) match { case t@SEXPNested(_) => makeSortDecls(t).map(p => (p._1,p._2.get)) }
    val recur : IMPSMathExp = parseSEXPFormula(sexp.args(2))

    assert(vars.length == 1)

    return IMPSIota(vars.head._1, vars.head._2, recur)
  }


  def makeIsDefined(sexp : SEXPNested) : IMPSIsDefined =
  {
    assert(sexp.args.head == SEXPAtom("is-defined"))
    assert(sexp.args.length == 2)

    val recur : IMPSMathExp = parseSEXPFormula(sexp.args(1))
    return IMPSIsDefined(recur)
  }

  def makeIsDefinedInSort(sexp : SEXPNested) : IMPSIsDefinedIn =
  {
    assert(sexp.args.head == SEXPAtom("is-defined-in-sort"))
    assert(sexp.args.length == 3)

    val recur : IMPSMathExp = parseSEXPFormula(sexp.args(1))
    val sort  : IMPSSort    = makeSort(sexp.args(2))
    return IMPSIsDefinedIn(recur,sort)
  }


  def makeUndefined(sexp : SEXPNested) : IMPSUndefined =
  {
    assert(sexp.args.head == SEXPAtom("undefined"))
    assert(sexp.args.length == 2)

    val sort : IMPSSort = makeSort(sexp.args(1))
    return IMPSUndefined(sort)
  }

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
      //println("COULD NOT PARSE: " + s)
      None
    } else {
      //println("PARSED:          " + j.get.toString)
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
      ("?" ~ parseSort) ^^ {case _ ~ m => IMPSUndefined(m)}
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
