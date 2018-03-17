package info.kwarc.mmt.imps

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.PackratParsers
import scala.util.Random

package object impsMathParser
{
  def makeSEXPFormula(sexp : SEXP, mpc : MathParsingContext) : IMPSMathExp =
  {
    sexp match {
      case s@SEXPAtom(name)   => IMPSVar(name)    /* Parse all "only strings" as Vars for now,
                                                     during translation we switch to MathSymbols
                                                     for undefined vars. */
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

        case SEXPAtom("predicate-to-indicator") =>
        {
          assert(s.args.length == 2)
          val pred : IMPSMathExp = makeSEXPFormula(s.args(1),mpc)

          IMPSQCPred2Indicator(pred)
        }

        case SEXPAtom("sort-to-indicator") =>
        {
          assert(s.args.length == 2)
          val srt : IMPSMathExp = makeSEXPFormula(s.args(1),mpc)
          assert(srt.isInstanceOf[IMPSUndefined])

          IMPSQCSort2Indicator(srt)
        }

        case SEXPAtom("i-in") =>
        {
          assert(s.args.length == 3)
          val v1 : IMPSMathExp = makeSEXPFormula(s.args(1),mpc)
          val v2 : IMPSMathExp = makeSEXPFormula(s.args(2),mpc)

          IMPSQCIn(v1,v2)
        }

        case SEXPAtom("i-subseteq") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1),mpc)
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2),mpc)

          IMPSQCSubsetEQ(e1,e2)
        }

        case SEXPAtom("i-subset") =>
        {
          // not used?
          ???
        }

        case SEXPAtom("i-empty-indicator") => {
          // "lambda(e:uu, lambda(x:uu,?unit%sort))"
          val e_var = (IMPSVar("e"), IMPSAtomSort("uu"))
          val x_var = (IMPSVar("x"), IMPSAtomSort("uu"))
          val inner = IMPSLambda(List(x_var), IMPSUndefined(IMPSAtomSort("unitsort")))
          IMPSLambda(List(e_var), inner)
        }

        case SEXPAtom("i-nonempty-indicator?") => {
          // "lambda(a:sets[uu], forsome(x:uu, x in a))"
          val a_var = (IMPSVar("a"), IMPSSetSort(IMPSAtomSort("uu")))
          val x_var = (IMPSVar("x"), IMPSAtomSort("uu"))

          // "lambda(x:uu,a:sets[uu], #(a(x)))"
          val ia_var = (IMPSVar("a"), IMPSSetSort(IMPSAtomSort("uu")))
          val ix_var = (IMPSVar("x"), IMPSAtomSort("uu"))

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

        if (sorts.length == 2) { IMPSBinaryFunSort(sorts.head, sorts.last) } else { IMPSNaryFunSort(sorts) }
      }
    }
  }

  def makeSortDecls(sexp : SEXPNested, inp : List[IMPSMathExp] = Nil) : List[(IMPSVar, IMPSSort)] =
  {
    def oneSortDecl(sexp : SEXPNested, inp : List[IMPSMathExp] = Nil) : List[(IMPSVar, IMPSSort)] =
    {
      val theSort : IMPSSort = makeSort(sexp.args.head)
      assert(sexp.args.tail.forall(a => a.isInstanceOf[SEXPAtom]))

      println(" > TODO: make sortDecl use FreshVar")

      return sexp.args.tail.map { case SEXPAtom(n) => (IMPSVar(n), theSort) } // Needs FreshVar here
    }

    assert(sexp.args.forall(a => a.isInstanceOf[SEXPNested]))
    return sexp.args.flatMap(a => a match {
      case t@SEXPNested(_) => oneSortDecl(t)
    })
  }

  def makeForall(sexp : SEXPNested, mpc : MathParsingContext) : IMPSForAll =
  {
    assert(sexp.args.head == SEXPAtom("forall"))
    assert(sexp.args.length == 3)

    val sorts : List[(IMPSVar, IMPSSort)] = sexp.args(1) match
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

    val sorts : List[(IMPSVar, IMPSSort)] = sexp.args(1) match
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

    val sorts : List[(IMPSVar, IMPSSort)] = sexp.args(1) match
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
    val vars  : List[(IMPSVar, IMPSSort)] = sexp.args(1) match { case t@SEXPNested(_) => makeSortDecls(t).map(p => (p._1,p._2)) }
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

  //-----------

  def freshVar(preferred : String, input : List[IMPSMathExp] = Nil) : IMPSVar =
  {
    def boundVars(input : IMPSMathExp) : List[String] =
    {
      val list : List[String] = input match
      {
        case IMPSVar(v)             => List(v)
        case IMPSIndividual()       => Nil
        case IMPSMathSymbol(s)      => Nil
        case IMPSTruth()            => Nil
        case IMPSFalsehood()        => Nil
        case IMPSNegation(p)        => boundVars(p)
        case IMPSIf(p,t1,t2)        => boundVars(p) ::: boundVars(t1) ::: boundVars(t2)
        case IMPSIff(p, q)          => boundVars(p) ::: boundVars(q)
        case IMPSIfForm(p,q,r)      => boundVars(p) ::: boundVars(q) ::: boundVars(r)
        case IMPSEquals(a,b)        => boundVars(a) ::: boundVars(b)
        case IMPSDisjunction(ls)    => ls.flatMap(boundVars)
        case IMPSConjunction(ls)    => ls.flatMap(boundVars)
        case IMPSLambda(vs,t)       => vs.flatMap(v => boundVars(v._1)) ::: boundVars(t)
        case IMPSForAll(vs,p)       => vs.flatMap(v => boundVars(v._1)) ::: boundVars(p)
        case IMPSForSome(vs,r   )   => vs.flatMap(v => boundVars(v._1)) ::: boundVars(r)
        case IMPSImplication(p,q)   => boundVars(p) ::: boundVars(q)
        case IMPSApply(f,ts)        => boundVars(f) ::: ts.flatMap(boundVars)
        case IMPSIota(v1,s1,p)      => boundVars(v1) ::: boundVars(p)
        case IMPSIotaP(v1,s1,p)     => boundVars(v1) ::: boundVars(p)
        case IMPSIsDefined(r)       => boundVars(r)
        case IMPSIsDefinedIn(r,s)   => boundVars(r)
        case IMPSUndefined(s)       => Nil
        case IMPSTotal(f,bs)        => boundVars(f)
        case IMPSNonVacuous(p)      => boundVars(p)
        case IMPSQuasiEquals(p,q)   => boundVars(p) ::: boundVars(q)

        case IMPSQCIn(e1,e2)         => boundVars(e1) ::: boundVars(e2)
        case IMPSQCSort2Indicator(s) => boundVars(s)
        case IMPSQCPred2Indicator(p) => boundVars(p)
      }
      list.distinct
    }

    val alphabet : List[String] = "abcefghijkmopqrstuvwxyz".flatMap(c => List(c.toString, c.toString+c.toString)).toList
    val used     : List[String] = if (input.nonEmpty) { input.flatMap(m => boundVars(m)) } else { Nil }
    val valid    : List[String] = alphabet.filter(s => !used.contains(s))
    assert(valid.nonEmpty)

    if (valid.contains(preferred))
    { return IMPSVar(preferred) }
    else
    { val random = new Random() ; return IMPSVar(valid(random.nextInt(valid.length))) }
    /* Doesn't actually need to be random of course, could also be head. */
  }

  /* PackratParsers because those can be left-recursive */
  class IMPSMathParser extends RegexParsers with PackratParsers
  {
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
  }
}

class MathParsingContext
{
  var quasiCs : List[(String, IMPSMathExp)] = Nil
}
