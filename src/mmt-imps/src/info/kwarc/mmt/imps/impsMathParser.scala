package info.kwarc.mmt.imps

import com.sun.xml.internal.bind.v2.runtime.output.StAXExStreamWriterOutput
import info.kwarc.mmt.api.GlobalName

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.PackratParsers

package object impsMathParser
{
  def makeSEXPFormula(sexp : SEXP, mpc : MathParsingContext) : IMPSMathExp =
  {
    sexp match {
      case s@SEXPAtom(name)   => IMPSVar(name)
      case s@SEXPNested(args) => args.head match
      {
        /* See imps manual, page 64 */

        case SEXPAtom("truth")              => IMPSTruth()
        case SEXPAtom("falsehood")          => IMPSFalsehood()
        case SEXPAtom("not")                => makeNot(s, mpc)
        case SEXPAtom("and")                => makeConjunction(s, mpc)
        case SEXPAtom("or")                 => makeDisjunction(s, mpc)
        case SEXPAtom("implies")            => makeImplication(s, mpc)
        case SEXPAtom("iff")                => makeIff(s, mpc)
        case SEXPAtom("if-form")            => makeIfform(s,mpc)
        case SEXPAtom("forall")             => makeForall(s, mpc)
        case SEXPAtom("forsome")            => makeForSome(s, mpc)
        case SEXPAtom("=")                  => makeEquals(s, mpc)
        case SEXPAtom("apply-operator")     => makeApplication(s, mpc)
        case SEXPAtom("lambda")             => makeLambda(s, mpc)
        case SEXPAtom("iota")               => makeIota(s, mpc)
        case SEXPAtom("iota-p")             => ??? // probably defunct
        case SEXPAtom("if")                 => makeIf(s, mpc)
        case SEXPAtom("is-defined")         => makeIsDefined(s, mpc)
        case SEXPAtom("is-defined-in-sort") => makeIsDefinedInSort(s, mpc)
        case SEXPAtom("undefined")          => makeUndefined(s, mpc)

        /* IMPS-universal quasi-constructors */

        case SEXPAtom("total?")             => makeTotal(s, mpc)
        case SEXPAtom("nonvacuous?")        => makeNonvacuous(s, mpc)
        case SEXPAtom("==")                 => makeQuasiEquals(s, mpc)

        /* User-defined quasi-constructors */
        /* These needed to be hand-translated */

        case SEXPAtom("predicate-to-indicator") => {
          // "lambda(s:[uu,prop], lambda(x:uu, if(s(x), an%individual, ?unit%sort)))"
          val var_s = (IMPSVar("s"), Some(IMPSBinaryFunSort(IMPSAtomSort("uu"),IMPSAtomSort("prop"))))
          val var_x = (IMPSVar("x"), Some(IMPSAtomSort("uu")))
          val appl  = IMPSApply(IMPSMathSymbol("s"),List(IMPSMathSymbol("x")))
          val target = IMPSIf(appl,IMPSIndividual(),IMPSUndefined(IMPSAtomSort("unitsort")))
          IMPSLambda(List(var_s),target)
        }

        case SEXPAtom("sort-to-indicator") => {
          // "lambda(e:uu, lambda(x:uu, an%individual))"
          val e_var = (IMPSVar("e"), Some(IMPSAtomSort("uu")))
          val x_var = (IMPSVar("x"), Some(IMPSAtomSort("uu")))
          val inner = IMPSLambda(List(x_var),IMPSIndividual())
          IMPSLambda(List(e_var),inner)
        }

        case SEXPAtom("i-in") => {
          // "lambda(x:uu,a:sets[uu], #(a(x)))"
          val x_var = (IMPSVar("x"), Some(IMPSAtomSort("uu")))
          val a_var = (IMPSVar("a"), Some(IMPSSetSort(IMPSAtomSort("uu"))))
          val target = IMPSIsDefined(IMPSApply(IMPSMathSymbol("a"),List(IMPSMathSymbol("x"))))
          IMPSLambda(List(x_var,a_var),target)
        }

        case SEXPAtom("i-subseteq") => {
          // "lambda(a,b:sets[uu], forall(x:uu, (x in a) implies (x in b)))"
          val a_var = (IMPSVar("a"), Some(IMPSSetSort(IMPSAtomSort("uu"))))
          val b_var = (IMPSVar("b"), Some(IMPSSetSort(IMPSAtomSort("uu"))))
          val x_var = (IMPSVar("x"), Some(IMPSAtomSort("uu")))

          val sp : SymbolicExpressionParser = new SymbolicExpressionParser
          val i1 = sp.parseAll(sp.parseSEXP,"(i-in x a)")
          assert(i1.successful)
          val i2 = sp.parseAll(sp.parseSEXP,"(i-in x b)")
          assert(i2.successful)

          val recur1 = makeSEXPFormula(i1.get,mpc)
          val recur2 = makeSEXPFormula(i2.get,mpc)

          val impl = IMPSImplication(recur1,recur2)
          val fora = IMPSForAll(List(x_var), impl)
          IMPSLambda(List(a_var,b_var),fora)
        }

        case SEXPAtom("i-subset") => {
          // "lambda(a,b:sets[uu], (a subseteq b) and not(a = b))"
          val a_var = (IMPSVar("a"),Some(IMPSSetSort(IMPSAtomSort("uu"))))
          val b_var = (IMPSVar("b"),Some(IMPSSetSort(IMPSAtomSort("uu"))))

          val sp : SymbolicExpressionParser = new SymbolicExpressionParser
          val i1 = sp.parseAll(sp.parseSEXP,"(i-subseteq a b)")
          assert(i1.successful)
          val sbs = makeSEXPFormula(i1.get,mpc)

          val nt = IMPSNegation(IMPSEquals(IMPSVar("a"),IMPSVar("b")))
          IMPSLambda(List(a_var,b_var), IMPSConjunction(List(sbs,nt)))
        }

        case SEXPAtom("i-empty-indicator") => {
          // "lambda(e:uu, lambda(x:uu,?unit%sort))"
          val e_var = (IMPSVar("e"), Some(IMPSAtomSort("uu")))
          val x_var = (IMPSVar("x"), Some(IMPSAtomSort("uu")))
          val inner = IMPSLambda(List(x_var), IMPSUndefined(IMPSAtomSort("unitsort")))
          IMPSLambda(List(e_var), inner)
        }

        case SEXPAtom("i-nonempty-indicator?") => {
          // "lambda(a:sets[uu], forsome(x:uu, x in a))"
          val a_var = (IMPSVar("a"), Some(IMPSSetSort(IMPSAtomSort("uu"))))
          val x_var = (IMPSVar("x"), Some(IMPSAtomSort("uu")))

          // "lambda(x:uu,a:sets[uu], #(a(x)))"
          val ia_var = (IMPSVar("a"), Some(IMPSSetSort(IMPSAtomSort("uu"))))
          val ix_var = (IMPSVar("x"), Some(IMPSAtomSort("uu")))

          ???
        }

        case SEXPAtom(str) =>
        {
          val qc : Option[(String, IMPSMathExp)] = mpc.quasiCs.find(q => q._1 == str)

          if (qc.isDefined) { return makeQC(s, qc.get, mpc) }
          else
            { throw new IMPSDependencyException("Error: Unknown quasi-constructor(?): " + str) }
        }
      }
    }
  }

  def makeQC(sexp : SEXPNested, qc : (String, IMPSMathExp), mpc : MathParsingContext) : IMPSMathExp =
  {
    ???
  }


  def makeNot(sexp : SEXPNested, mpc : MathParsingContext) : IMPSNegation =
  {
    assert(sexp.args.head == SEXPAtom("not"))
    assert(sexp.args.length == 2)

    val recur : IMPSMathExp = makeSEXPFormula(sexp.args(1), mpc)
    return IMPSNegation(recur)
  }

  def makeConjunction(sexp : SEXPNested, mpc : MathParsingContext) : IMPSConjunction =
  {
    assert(sexp.args.head == SEXPAtom("and"))
    assert(sexp.args.length >= 3)

    val recurs : List[IMPSMathExp] = sexp.args.tail.map(r => makeSEXPFormula(r, mpc))

    return IMPSConjunction(recurs)
  }

  def makeDisjunction(sexp : SEXPNested, mpc : MathParsingContext) : IMPSDisjunction =
  {
    assert(sexp.args.head == SEXPAtom("or"))
    assert(sexp.args.length >= 3)

    val recurs : List[IMPSMathExp] = sexp.args.tail.map(r => makeSEXPFormula(r, mpc))

    return IMPSDisjunction(recurs)
  }

  def makeImplication(sexp : SEXPNested, mpc : MathParsingContext) : IMPSImplication =
  {
    assert(sexp.args.head == SEXPAtom("implies"))
    assert(sexp.args.length == 3)

    val recur1 : IMPSMathExp = makeSEXPFormula(sexp.args(1), mpc)
    val recur2 : IMPSMathExp = makeSEXPFormula(sexp.args(2), mpc)

    return IMPSImplication(recur1, recur2)
  }

  def makeEquals(sexp : SEXPNested, mpc : MathParsingContext) : IMPSEquals =
  {
    assert(sexp.args.head == SEXPAtom("="))
    assert(sexp.args.length == 3)

    val recur1 : IMPSMathExp = makeSEXPFormula(sexp.args(1), mpc)
    val recur2 : IMPSMathExp = makeSEXPFormula(sexp.args(2), mpc)

    return IMPSEquals(recur1, recur2)
  }

  def makeIff(sexp : SEXPNested, mpc : MathParsingContext) : IMPSIff =
  {
    assert(sexp.args.head == SEXPAtom("iff"))
    assert(sexp.args.length == 3)

    val recur1 : IMPSMathExp = makeSEXPFormula(sexp.args(1), mpc)
    val recur2 : IMPSMathExp = makeSEXPFormula(sexp.args(2), mpc)

    return IMPSIff(recur1, recur2)
  }

  def makeIfform(sexp : SEXPNested, mpc : MathParsingContext) : IMPSIfForm =
  {
    assert(sexp.args.head == SEXPAtom("if-form"))
    assert(sexp.args.length == 4)

    val recur1 : IMPSMathExp = makeSEXPFormula(sexp.args(1), mpc)
    val recur2 : IMPSMathExp = makeSEXPFormula(sexp.args(2), mpc)
    val recur3 : IMPSMathExp = makeSEXPFormula(sexp.args(3), mpc)

    return IMPSIfForm(recur1,recur2,recur3)
  }

  def makeIf(sexp : SEXPNested, mpc : MathParsingContext) : IMPSIf =
  {
    assert(sexp.args.head == SEXPAtom("if"))
    assert(sexp.args.length == 4)

    val recur1 : IMPSMathExp = makeSEXPFormula(sexp.args(1), mpc)
    val recur2 : IMPSMathExp = makeSEXPFormula(sexp.args(2), mpc)
    val recur3 : IMPSMathExp = makeSEXPFormula(sexp.args(3), mpc)

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
        //assert(args.forall(a => a.isInstanceOf[SEXPAtom]))
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

  def makeForall(sexp : SEXPNested, mpc : MathParsingContext) : IMPSForAll =
  {
    assert(sexp.args.head == SEXPAtom("forall"))
    assert(sexp.args.length == 3)

    val sorts : List[(IMPSVar, Option[IMPSSort])] = sexp.args(1) match
    {
      case SEXPAtom(_)     => ???
      case s@SEXPNested(_) => makeSortDecls(s)
    }

    val target : IMPSMathExp = makeSEXPFormula(sexp.args.last, mpc)

    return IMPSForAll(sorts,target)
  }

  def makeForSome(sexp : SEXPNested, mpc : MathParsingContext) : IMPSForSome =
  {
    assert(sexp.args.head == SEXPAtom("forsome"))
    assert(sexp.args.length == 3)

    val sorts : List[(IMPSVar, Option[IMPSSort])] = sexp.args(1) match
    {
      case SEXPAtom(_)     => ???
      case s@SEXPNested(_) => makeSortDecls(s)
    }

    val target : IMPSMathExp = makeSEXPFormula(sexp.args.last, mpc)

    return IMPSForSome(sorts,target)
  }

  def makeLambda(sexp : SEXPNested, mpc : MathParsingContext) : IMPSLambda =
  {
    assert(sexp.args.head == SEXPAtom("lambda"))
    assert(sexp.args.length == 3)

    val sorts : List[(IMPSVar, Option[IMPSSort])] = sexp.args(1) match
    {
      case SEXPAtom(_)     => ???
      case s@SEXPNested(_) => makeSortDecls(s)
    }

    val target : IMPSMathExp = makeSEXPFormula(sexp.args.last, mpc)

    return IMPSLambda(sorts,target)
  }

  def makeApplication(sexp : SEXPNested, mpc : MathParsingContext) : IMPSApply =
  {
    assert(sexp.args.head == SEXPAtom("apply-operator"))
    assert(sexp.args.length >= 3)

    val recurs : List[IMPSMathExp] = sexp.args.tail.map(r => makeSEXPFormula(r, mpc))

    return  IMPSApply(recurs.head, recurs.tail)
  }

  def makeIota(sexp : SEXPNested, mpc : MathParsingContext) : IMPSIota =
  {
    assert(sexp.args.head == SEXPAtom("iota"))
    assert(sexp.args.length == 3)

    assert(sexp.args(1).isInstanceOf[SEXPNested])
    val vars  : List[(IMPSVar, IMPSSort)] = sexp.args(1) match { case t@SEXPNested(_) => makeSortDecls(t).map(p => (p._1,p._2.get)) }
    val recur : IMPSMathExp = makeSEXPFormula(sexp.args(2), mpc)

    assert(vars.length == 1)

    return IMPSIota(vars.head._1, vars.head._2, recur)
  }


  def makeIsDefined(sexp : SEXPNested, mpc : MathParsingContext) : IMPSIsDefined =
  {
    assert(sexp.args.head == SEXPAtom("is-defined"))
    assert(sexp.args.length == 2)

    val recur : IMPSMathExp = makeSEXPFormula(sexp.args(1), mpc)
    return IMPSIsDefined(recur)
  }

  def makeIsDefinedInSort(sexp : SEXPNested, mpc : MathParsingContext) : IMPSIsDefinedIn =
  {
    assert(sexp.args.head == SEXPAtom("is-defined-in-sort"))
    assert(sexp.args.length == 3)

    val recur : IMPSMathExp = makeSEXPFormula(sexp.args(1), mpc)
    val sort  : IMPSSort    = makeSort(sexp.args(2))
    return IMPSIsDefinedIn(recur,sort)
  }


  def makeUndefined(sexp : SEXPNested, mpc : MathParsingContext) : IMPSUndefined =
  {
    assert(sexp.args.head == SEXPAtom("undefined"))
    assert(sexp.args.length == 2)

    val sort : IMPSSort = makeSort(sexp.args(1))
    return IMPSUndefined(sort)
  }

  /* Quasi-Constructprs */

  def makeNonvacuous(sexp : SEXPNested, mpc : MathParsingContext) : IMPSNonVacuous =
  {
    assert(sexp.args.head == SEXPAtom("nonvacuous?"))
    assert(sexp.args.length == 2)

    val recur : IMPSMathExp = makeSEXPFormula(sexp.args(1), mpc)
    return IMPSNonVacuous(recur)
  }

  def makeTotal(sexp : SEXPNested, mpc : MathParsingContext) : IMPSTotal =
  {
    assert(sexp.args.head == SEXPAtom("total?"))
    assert(sexp.args.length >= 3)

    val f     : IMPSMathExp    = makeSEXPFormula(sexp.args(1), mpc)
    val betas : List[IMPSSort] = sexp.args.tail.tail.map(b => b match {
      case t@SEXPAtom(_)   => makeSort(t)
      case t@SEXPNested(_) => if (t.args.head == SEXPAtom("undefined")) { makeUndefined(t, mpc).s } else { makeSort(t) }
    })

    return IMPSTotal(f, betas)
  }

  def makeQuasiEquals(sexp : SEXPNested, mpc : MathParsingContext) : IMPSQuasiEquals =
  {
    assert(sexp.args.head == SEXPAtom("=="))
    assert(sexp.args.length == 3)

    val p1 : IMPSMathExp = makeSEXPFormula(sexp.args(1),mpc)
    val p2 : IMPSMathExp = makeSEXPFormula(sexp.args(2),mpc)

    return IMPSQuasiEquals(p1,p2)

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

    lazy val parseSort : PackratParser[IMPSSort] = { parseSets | parseFunSort | parseFunSort2 | parseAtomicSort }

    lazy val parseSets : PackratParser[IMPSSetSort] = {
      ("sets[" ~> parseSort <~ "]") ^^ { case setsort => IMPSSetSort(setsort)}
    }

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

class MathParsingContext
{
  var quasiCs : List[(String, IMPSMathExp)] = Nil
}
