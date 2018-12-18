package info.kwarc.mmt.imps

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.PackratParsers
import scala.util.Random

package object impsMathParser
{
  def makeSEXPFormula(sexp : SEXP) : IMPSMathExp =
  {
    sexp match {
      case SEXPAtom("truth")      => IMPSTruth()
      case SEXPAtom("falsehood")  => IMPSFalsehood()
      case s@SEXPAtom(name)       => IMPSVar(name)    /* Parse all "only strings" as Vars for now,
                                                         during translation we switch to MathSymbols
                                                         for undefined vars. */
      case s@SEXPNested(args) => args.head match
      {
        /* See imps manual, page 64 */

        case SEXPAtom("not")                => makeNot(s)
        case SEXPAtom("and")                => makeConjunction(s)
        case SEXPAtom("or")                 => makeDisjunction(s)
        case SEXPAtom("implies")            => makeImplication(s)
        case SEXPAtom("iff")                => makeIff(s)
        case SEXPAtom("if-form")            => makeIfform(s)
        case SEXPAtom("forall")             => makeForall(s)
        case SEXPAtom("forsome")            => makeForSome(s)
        case SEXPAtom("=")                  => makeEquals(s)
        case SEXPAtom("apply-operator")     => makeApplication(s)
        case SEXPAtom("lambda")             => makeLambda(s)
        case SEXPAtom("iota")               => makeIota(s)
        case SEXPAtom("iota-p")             => ??? // probably defunct
        case SEXPAtom("if")                 => makeIf(s)
        case SEXPAtom("is-defined")         => makeIsDefined(s)
        case SEXPAtom("is-defined-in-sort") => makeIsDefinedInSort(s)
        case SEXPAtom("undefined")          => makeUndefined(s)

        case SEXPAtom("WITH")
           | SEXPAtom("with")               =>
        {
          assert(args.head == SEXPAtom("with") | args.head == SEXPAtom("WITH"))
          assert(args.length == 3)

          val sorts : List[(IMPSVar, IMPSSort)] = args(1) match
          {
            case SEXPAtom(_)     => ???
            case s@SEXPNested(_) => makeSortDecls(s)
          }

          val target : IMPSMathExp = makeSEXPFormula(args.last)

          IMPSWith(sorts,target)
        }

        /* IMPS-universal quasi-constructors */

        case SEXPAtom("total?")             => makeTotal(s)
        case SEXPAtom("nonvacuous?")        => makeNonvacuous(s)
        case SEXPAtom("==")                 => makeQuasiEquals(s)

        /* User-defined quasi-constructors */
        /* These needed to be hand-translated */

        case SEXPAtom("predicate-to-indicator") =>
        {
          assert(s.args.length == 2)
          val pred : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCPred2Indicator(pred)
        }

        case SEXPAtom("sort-to-indicator") =>
        {
          assert(s.args.length == 2)
          val srt : IMPSMathExp = makeSEXPFormula(s.args(1))
          assert(srt.isInstanceOf[IMPSUndefined])
          IMPSQCSort2Indicator(srt)
        }

        case SEXPAtom("i-in") =>
        {
          assert(s.args.length == 3)
          val v1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val v2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCIn(v1,v2)
        }

        case SEXPAtom("i-subseteq") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCSubsetEQ(e1,e2)
        }

        case SEXPAtom("i-subset") =>
        {
          // not used?
          ???
        }

        case SEXPAtom("i-empty-indicator") =>
        {
          assert(s.args.length == 2)
          val srt : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCEmptyIndicator(srt)
        }

        case SEXPAtom("I-NONEMPTY-INDICATOR?")
           | SEXPAtom("i-nonempty-indicator?") =>
        {
          assert(s.args.length == 2)
          val srt : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCNonemptyIndicator(srt)
        }

        case SEXPAtom("i-empty-indicator?") =>
        {
          assert(s.args.length == 2)
          val srt : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCEmptyIndicatorQ(srt)
        }

        case SEXPAtom("i-complement") =>
        {
          assert(s.args.length == 2)
          val m : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCComplement(m)
        }

        case SEXPAtom("i-union") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCUnion(e1,e2)
        }

        case SEXPAtom("i-intersection") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCIntersection(e1,e2)
        }

        case SEXPAtom("i-difference") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCDifference(e1,e2)
        }

        case SEXPAtom("i-disjoint") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCDisjoint(e1,e2)
        }

        case SEXPAtom("i-singleton") =>
        {
          assert(s.args.length == 2)
          val m : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCSingleton(m)
        }

        case SEXPAtom("i-big-union") =>
        {
          assert(s.args.length == 2)
          val m : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCBigUnion(m)
        }

        case SEXPAtom("i-big-intersection") =>
        {
          assert(s.args.length == 2)
          val m : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCBigIntersection(m)
        }

        case SEXPAtom("i-partition?") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCPartitionQ(e1,e2)
        }

        case SEXPAtom("m-domain") =>
        {
          assert(s.args.length == 2)
          val m : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCMDomain(m)
        }

        case SEXPAtom("m-composition") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCMComposition(e1,e2)
        }

        case SEXPAtom("m-range") =>
        {
          assert(s.args.length == 2)
          val m : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCMRange(m)
        }

        case SEXPAtom("m-image") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCMImage(e1,e2)
        }

        case SEXPAtom("m-inverse-image") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCMInverseImage(e1,e2)
        }

        case SEXPAtom("m-inverse") =>
        {
          assert(s.args.length == 2)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCMInverse(e1)
        }

        case SEXPAtom("m-id") =>
        {
          assert(s.args.length == 2)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCMId(e1)
        }

        case SEXPAtom("m-restrict") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCMRestrict(e1,e2)
        }

        case SEXPAtom("m-restrict2") =>
        {
          assert(s.args.length == 4)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))
          val e3 : IMPSMathExp = makeSEXPFormula(s.args(3))

          IMPSQCMRestrict2(e1,e2,e3)
        }

        case SEXPAtom("m-surjective?") =>
        {
          assert(s.args.length == 2)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCMSurjective(e1)
        }

        case SEXPAtom("m-injective?") =>
        {
          assert(s.args.length == 2)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCMSurjective(e1)
        }

        case SEXPAtom("m-bijective?") =>
        {
          assert(s.args.length == 2)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCMBijective(e1)
        }

        case SEXPAtom("m-surjective-on?") =>
        {
          assert(s.args.length == 4)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))
          val e3 : IMPSMathExp = makeSEXPFormula(s.args(3))

          IMPSQCMSurjectiveOn(e1,e2,e3)
        }

        case SEXPAtom("m-injective-on?") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCMInjectiveOn(e1,e2)
        }

        case SEXPAtom("m-bijective-on?") =>
        {
          assert(s.args.length == 4)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))
          val e3 : IMPSMathExp = makeSEXPFormula(s.args(3))

          IMPSQCMBijectiveOn(e1,e2,e3)
        }

        case SEXPAtom("equinumerous") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCEquinumerous(e1,e2)
        }

        case SEXPAtom("embeds") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCEmbeds(e1,e2)
        }

        case SEXPAtom("finite%cover") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCFiniteCover(e1,e2)
        }

        case SEXPAtom("countable%cover") =>
        {
          assert(s.args.length == 3)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val e2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCCountableCover(e1,e2)
        }

        case SEXPAtom("group") =>
        {
          assert(s.args.length == 5)
          val m   : IMPSMathExp = makeSEXPFormula(s.args(1))
          val mul : IMPSMathExp = makeSEXPFormula(s.args(2))
          val e   : IMPSMathExp = makeSEXPFormula(s.args(3))
          val inv : IMPSMathExp = makeSEXPFormula(s.args(4))

          IMPSQCGroups(m,mul,e,inv)
        }

        case SEXPAtom("finite-cardinality") =>
        {
          assert(s.args.length == 2)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCFinCard(e1)
        }

        case SEXPAtom("finite-indicator?") =>
        {
          assert(s.args.length == 2)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCFinIndic(e1)
        }

        case SEXPAtom("finite-sort?") =>
        {
          assert(s.args.length == 2)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCFinSort(???)
        }

        case SEXPAtom("invariant") =>
        {
          assert(s.args.length == 3)
          val a : IMPSMathExp = makeSEXPFormula(s.args(1))
          val f : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCInvariant(a,f)
        }

        case SEXPAtom("length") =>
        {
          assert(s.args.length == 2)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCLength(e1)
        }

        case SEXPAtom("f-seq?") =>
        {
          assert(s.args.length == 2)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCFseqQ(e1)
        }

        case SEXPAtom("nil") =>
        {
          assert(s.args.length == 2)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCNil(e1)
        }

        case SEXPAtom("cons") =>
        {
          assert(s.args.length == 3)
          val a : IMPSMathExp = makeSEXPFormula(s.args(1))
          val f : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCCons(a,f)
        }

        case SEXPAtom("drop") =>
        {
          assert(s.args.length == 3)
          val a : IMPSMathExp = makeSEXPFormula(s.args(1))
          val f : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCDrop(a,f)
        }

        case SEXPAtom("takefirst") =>
        {
          assert(s.args.length == 3)
          val a : IMPSMathExp = makeSEXPFormula(s.args(1))
          val f : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCTakeFirst(a,f)
        }

        case SEXPAtom("append") =>
        {
          assert(s.args.length == 3)
          val a : IMPSMathExp = makeSEXPFormula(s.args(1))
          val f : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCAppend(a,f)
        }

        case SEXPAtom("in_seq") =>
        {
          assert(s.args.length == 3)
          val a : IMPSMathExp = makeSEXPFormula(s.args(1))
          val f : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCInSeq(a,f)
        }

        case SEXPAtom("constrict") =>
        {
          assert(s.args.length == 3)
          val a : IMPSMathExp = makeSEXPFormula(s.args(1))
          val f : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCConstrict(a,f)
        }

        case SEXPAtom("collapse") =>
        {
          assert(s.args.length == 2)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCCollapse(e1)
        }

        case SEXPAtom("pair") =>
        {
          assert(s.args.length == 3)
          val p1 : IMPSMathExp = makeSEXPFormula(s.args(1))
          val p2 : IMPSMathExp = makeSEXPFormula(s.args(2))

          IMPSQCPair(p1,p2)
        }

        case SEXPAtom("pair?") =>
        {
          assert(s.args.length == 2)
          val p : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCPairQ(p)
        }

        case SEXPAtom("first") =>
        {
          assert(s.args.length == 2)
          val e1 : IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCFirst(e1)
        }

        case SEXPAtom("second") => {
          assert(s.args.length == 2)
          val e1: IMPSMathExp = makeSEXPFormula(s.args(1))

          IMPSQCSecond(e1)
        }

        case SEXPAtom(str) =>
        {
          throw new IMPSDependencyException("Error: Unknown operators: " + str + "\n" + s)
        }
      }
    }
  }


  def makeNot(sexp : SEXPNested) : IMPSNegation =
  {
    assert(sexp.args.head == SEXPAtom("not"))
    assert(sexp.args.length == 2)

    val recur : IMPSMathExp = makeSEXPFormula(sexp.args(1))
    return IMPSNegation(recur)
  }

  def makeConjunction(sexp : SEXPNested) : IMPSConjunction =
  {
    assert(sexp.args.head == SEXPAtom("and"))
    assert(sexp.args.length >= 3)

    val recurs : List[IMPSMathExp] = sexp.args.tail.map(r => makeSEXPFormula(r))

    return IMPSConjunction(recurs)
  }

  def makeDisjunction(sexp : SEXPNested) : IMPSDisjunction =
  {
    assert(sexp.args.head == SEXPAtom("or"))
    assert(sexp.args.length >= 3)

    val recurs : List[IMPSMathExp] = sexp.args.tail.map(r => makeSEXPFormula(r))

    return IMPSDisjunction(recurs)
  }

  def makeImplication(sexp : SEXPNested) : IMPSImplication =
  {
    assert(sexp.args.head == SEXPAtom("implies"))
    assert(sexp.args.length == 3)

    val recur1 : IMPSMathExp = makeSEXPFormula(sexp.args(1))
    val recur2 : IMPSMathExp = makeSEXPFormula(sexp.args(2))

    return IMPSImplication(recur1, recur2)
  }

  def makeEquals(sexp : SEXPNested) : IMPSEquals =
  {
    assert(sexp.args.head == SEXPAtom("="))
    assert(sexp.args.length == 3)

    val recur1 : IMPSMathExp = makeSEXPFormula(sexp.args(1))
    val recur2 : IMPSMathExp = makeSEXPFormula(sexp.args(2))

    return IMPSEquals(recur1, recur2)
  }

  def makeIff(sexp : SEXPNested) : IMPSIff =
  {
    assert(sexp.args.head == SEXPAtom("iff"))
    assert(sexp.args.length == 3)

    val recur1 : IMPSMathExp = makeSEXPFormula(sexp.args(1))
    val recur2 : IMPSMathExp = makeSEXPFormula(sexp.args(2))

    return IMPSIff(recur1, recur2)
  }

  def makeIfform(sexp : SEXPNested) : IMPSIfForm =
  {
    assert(sexp.args.head == SEXPAtom("if-form"))
    assert(sexp.args.length == 4)

    val recur1 : IMPSMathExp = makeSEXPFormula(sexp.args(1))
    val recur2 : IMPSMathExp = makeSEXPFormula(sexp.args(2))
    val recur3 : IMPSMathExp = makeSEXPFormula(sexp.args(3))

    return IMPSIfForm(recur1,recur2,recur3)
  }

  def makeIf(sexp : SEXPNested) : IMPSIf =
  {
    assert(sexp.args.head == SEXPAtom("if"))
    assert(sexp.args.length == 4)

    val recur1 : IMPSMathExp = makeSEXPFormula(sexp.args(1))
    val recur2 : IMPSMathExp = makeSEXPFormula(sexp.args(2))
    val recur3 : IMPSMathExp = makeSEXPFormula(sexp.args(3))

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

      return sexp.args.tail.map { case SEXPAtom(n) => (IMPSVar(n), theSort) } // Needs FreshVar here
    }

    assert(sexp.args.forall(a => a.isInstanceOf[SEXPNested]))
    return sexp.args.flatMap(a => a match {
      case t@SEXPNested(_) => oneSortDecl(t)
    })
  }

  def makeForall(sexp : SEXPNested) : IMPSForAll =
  {
    assert(sexp.args.head == SEXPAtom("forall"))
    assert(sexp.args.length == 3)

    val sorts : List[(IMPSVar, IMPSSort)] = sexp.args(1) match
    {
      case SEXPAtom(_)     => ???
      case s@SEXPNested(_) => makeSortDecls(s)
    }

    val target : IMPSMathExp = makeSEXPFormula(sexp.args.last)

    return IMPSForAll(sorts,target)
  }

  def makeForSome(sexp : SEXPNested) : IMPSForSome =
  {
    assert(sexp.args.head == SEXPAtom("forsome"))
    assert(sexp.args.length == 3)

    val sorts : List[(IMPSVar, IMPSSort)] = sexp.args(1) match
    {
      case SEXPAtom(_)     => ???
      case s@SEXPNested(_) => makeSortDecls(s)
    }

    val target : IMPSMathExp = makeSEXPFormula(sexp.args.last)

    return IMPSForSome(sorts,target)
  }

  def makeLambda(sexp : SEXPNested) : IMPSLambda =
  {
    assert(sexp.args.head == SEXPAtom("lambda"))
    assert(sexp.args.length == 3)

    val sorts : List[(IMPSVar, IMPSSort)] = sexp.args(1) match
    {
      case SEXPAtom(_)     => ???
      case s@SEXPNested(_) => makeSortDecls(s)
    }

    val target : IMPSMathExp = makeSEXPFormula(sexp.args.last)

    return IMPSLambda(sorts,target)
  }

  def makeApplication(sexp : SEXPNested) : IMPSApply =
  {
    assert(sexp.args.head == SEXPAtom("apply-operator"))
    assert(sexp.args.length >= 3)

    val recurs : List[IMPSMathExp] = sexp.args.tail.map(r => makeSEXPFormula(r))

    return  IMPSApply(recurs.head, recurs.tail)
  }

  def makeIota(sexp : SEXPNested) : IMPSIota =
  {
    assert(sexp.args.head == SEXPAtom("iota"))
    assert(sexp.args.length == 3)

    assert(sexp.args(1).isInstanceOf[SEXPNested])
    val vars  : List[(IMPSVar, IMPSSort)] = sexp.args(1) match { case t@SEXPNested(_) => makeSortDecls(t).map(p => (p._1,p._2)) }
    val recur : IMPSMathExp = makeSEXPFormula(sexp.args(2))

    assert(vars.length == 1)

    return IMPSIota(vars.head._1, vars.head._2, recur)
  }


  def makeIsDefined(sexp : SEXPNested) : IMPSIsDefined =
  {
    assert(sexp.args.head == SEXPAtom("is-defined"))
    assert(sexp.args.length == 2)

    val recur : IMPSMathExp = makeSEXPFormula(sexp.args(1))
    return IMPSIsDefined(recur)
  }

  def makeIsDefinedInSort(sexp : SEXPNested) : IMPSIsDefinedIn =
  {
    assert(sexp.args.head == SEXPAtom("is-defined-in-sort"))
    assert(sexp.args.length == 3)

    val recur : IMPSMathExp = makeSEXPFormula(sexp.args(1))
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

  /* Quasi-Constructprs */

  def makeNonvacuous(sexp : SEXPNested) : IMPSNonVacuous =
  {
    assert(sexp.args.head == SEXPAtom("nonvacuous?"))
    assert(sexp.args.length == 2)

    val recur : IMPSMathExp = makeSEXPFormula(sexp.args(1))
    return IMPSNonVacuous(recur)
  }

  def makeTotal(sexp : SEXPNested) : IMPSTotal =
  {
    assert(sexp.args.head == SEXPAtom("total?"))
    assert(sexp.args.length >= 3)

    val f     : IMPSMathExp    = makeSEXPFormula(sexp.args(1))
    val betas : List[IMPSSort] = sexp.args.tail.tail.map(b => b match {
      case t@SEXPAtom(_)   => makeSort(t)
      case t@SEXPNested(_) => if (t.args.head == SEXPAtom("undefined")) { makeUndefined(t).s } else { makeSort(t) }
    })

    assert(betas.length == 1)

    return IMPSTotal(f, betas.head)
  }

  def makeQuasiEquals(sexp : SEXPNested) : IMPSQuasiEquals =
  {
    assert(sexp.args.head == SEXPAtom("=="))
    assert(sexp.args.length == 3)

    val p1 : IMPSMathExp = makeSEXPFormula(sexp.args(1))
    val p2 : IMPSMathExp = makeSEXPFormula(sexp.args(2))

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

        case IMPSQCIn(e1,e2)              => boundVars(e1) ::: boundVars(e2)
        case IMPSQCSort2Indicator(s)      => boundVars(s)
        case IMPSQCPred2Indicator(p)      => boundVars(p)
        case IMPSQCDifference(d1,d2)      => boundVars(d1) ::: boundVars(d2)
        case IMPSQCSymDifference(sd1,sd2) => boundVars(sd1) ::: boundVars(sd2)
        case IMPSQCDisjoint(dj1,dj2)      => boundVars(dj1) ::: boundVars(dj2)
        case IMPSQCNonemptyIndicator(srt) => boundVars(srt)
        case IMPSQCEmptyIndicator(srt)    => boundVars(srt)
        case IMPSQCEmptyIndicatorQ(srt)   => boundVars(srt)
        case IMPSQCPartitionQ(p,s)        => boundVars(p) ::: boundVars(s)
        case IMPSQCSingleton(f)           => boundVars(f)
        case IMPSQCBigIntersection(f)     => boundVars(f)
        case IMPSQCBigUnion(f)            => boundVars(f)
        case IMPSQCIntersection(i1,i2)    => boundVars(i1) ::: boundVars(i2)
        case IMPSQCUnion(u1,u2)           => boundVars(u1) ::: boundVars(u2)
        case IMPSQCSubsetEQ(e1,e2)        => boundVars(e1) ::: boundVars(e2)
        case IMPSQCSubset(e1,e2)          => boundVars(e1) ::: boundVars(e2)
        case IMPSQCComplement(c)          => boundVars(c)
      }
      list.distinct
    }

    val alphabet  : List[String] = "abcefghijkmopqrstuvwxyz".flatMap(c => List(c.toString, c.toString+c.toString)).toList
    val canidates : List[String] = List(preferred) ::: alphabet
    val used      : List[String] = if (input.nonEmpty) { input.flatMap(m => boundVars(m)) } else { Nil }
    val valid     : List[String] = canidates.filter(s => !used.contains(s))
    assert(valid.nonEmpty)

    if (valid.contains(preferred))
    { return IMPSVar(preferred) }
    else
    { IMPSVar(valid.head) }
  }

  class SortParser extends RegexParsers with PackratParsers
  {
    lazy val parseAtomicSort : PackratParser[IMPSAtomSort] = {
      ("[^,\\]):\\s]+".r) ^^ {case sort => IMPSAtomSort(sort)}
    }

    lazy val parseSort : PackratParser[IMPSSort] = { parseSets | parseFunSort | parseFunSort2 | parseAtomicSort }

    lazy val parseSets : PackratParser[IMPSSetSort] = {
      ("sets[" ~> parseSort <~ "]") ^^ { case setsort => IMPSSetSort(setsort)}
    }

    lazy val parseFunSort : PackratParser[IMPSNaryFunSort] = {
      "[" ~> rep1sep(parseSort,",") <~ "]" ^^ {case (sorts) => IMPSNaryFunSort(sorts)}
    }

    lazy val parseFunSort2 : PackratParser[IMPSNaryFunSort] = {
      "(" ~> rep1(parseSort) <~ ")" ^^ {case (sorts) => IMPSNaryFunSort(sorts)}
    }
  }

  class SymbolicExpressionParser extends RegexParsers with PackratParsers
  {
    lazy val parseSEXP : PackratParser[SEXP] = { parseNestedSEXP | parseAtom }

    lazy val parseAtom : PackratParser[SEXPAtom] =
    {
      """[^()\t\r\n ]+""".r ^^ { case name => SEXPAtom(name)}
    }

    lazy val parseNestedSEXP : PackratParser[SEXPNested] =
    {
      ("(" ~> rep1(parseSEXP) <~ ")") ^^ { case sexps => SEXPNested(sexps) }
    }
  }
}
