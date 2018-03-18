package info.kwarc.mmt.imps

import info.kwarc.mmt.api.utils.JSONObject
import info.kwarc.mmt.imps.Usage.Usage
import info.kwarc.mmt.imps.NumericalType.NumericalType
import info.kwarc.mmt.imps.impsDefFormParsers.{handpick, removeWhitespace}
import info.kwarc.mmt.imps.impsMathParser.IMPSMathParser

package object impsArgumentParsers
{
  /* Parser for IMPS axiom specifications argument
   * used def-theory */
  def parseAxiomSpecification(e : Exp, js : List[JSONObject], thy : String) : Option[AxiomSpecification] =
  {
    assert(e.children.nonEmpty)

    var name       : Option[String]       = None
    var formula    : Option[IMPSMathExp]  = None
    var usgs_prime : List[Usage]          = Nil

    var foobarList : List[LispExp] = Nil

    var frml  : String = ""
    var index : Int = 0

    e.children(0) match
    {
      case Exp(List(Str(h)),_) => {
        if (h.startsWith("\"")) {
          frml = h
          foobarList = e.children
          index = 0
        } else {
          name = Some(h)
          index = 1
          e.children(1) match {
            case Exp(List(Str(f)),_) => {
              frml = f
              foobarList = e.children.tail
            }
            case _ => ???
          }
        }
      }
      case _ => ???
    }

    // Parsing all usages
    for (c <- foobarList)
    {
      c match
      {
        case Exp(List(Str("elementary-macete")),_)        => usgs_prime = usgs_prime :+ Usage.ELEMENTARYMACETE
        case Exp(List(Str("transportable-macete")),_)     => usgs_prime = usgs_prime :+ Usage.TRANSPORTABLEMACETE
        case Exp(List(Str("rewrite")),_)                  => usgs_prime = usgs_prime :+ Usage.REWRITE
        case Exp(List(Str("transportable-rewrite")),_)    => usgs_prime = usgs_prime :+ Usage.TRANSPORTABLEREWRITE
        case Exp(List(Str("simplify-logically-first")),_) => usgs_prime = usgs_prime :+ Usage.SIMPLIFYLOGICALLYFIRST
        case Exp(List(Str("d-r-convergence")),_)          => usgs_prime = usgs_prime :+ Usage.DRCONVERGENCE
        case Exp(List(Str("d-r-value")),_)                => usgs_prime = usgs_prime :+ Usage.DRVALUE
        case _ => ()
      }
    }

    val json_theory : Option[JSONObject] = js.find(j => j.getAsString("name").toLowerCase == thy.toLowerCase)
    println(" > Looking for " + thy.toLowerCase)
    assert(json_theory.isDefined)
    assert(json_theory.get.getAsString("type") == "imps-theory")
    val axioms : List[JSONObject] = json_theory.get.getAsList(classOf[JSONObject],"axioms")
    assert(axioms.nonEmpty)

    var s : String = ""

    val theaxiom : Option[JSONObject] = if (name.isDefined)
    {
      println(" > looking for axiom " + name.get + " in theory " + thy + " ...")
      axioms.find(j => j.getAsString("name").toLowerCase == name.get.toLowerCase)
    } else {
      println(" > looking for nameless axiom in theory " + thy + " ...")
      e.children(index) match {
        case Exp(List(Str(x)), _) => {
          s = x.tail.init
          println("     > scala axiom: " + removeWhitespace(x.tail.init))
          var tempt = axioms.find(j => removeWhitespace(j.getAsString("formula-string")) == removeWhitespace(x.tail.init))

          if (!(tempt.isDefined)) {
            val handpicked = handpick(removeWhitespace(x.tail.init))
            println("     > handpicked: " +handpicked)
            tempt = axioms.find(j => removeWhitespace(j.getAsString("formula-string")) == handpicked)
          }

          tempt
        }
        case _                    => ???
      }
    }

    if (!(theaxiom.isDefined))
    {
      assert(s != "")
      val bar = removeWhitespace(s)
      println(" > s = " + s)

      for (t <- axioms)
      {
        val foo = removeWhitespace(t.getAsString("formula-string"))
        val n = 5
        if (foo.take(n) == bar.take(n)) {
          println("     > json axiom: " + foo)
        }
      }
    }

    assert(theaxiom.get.getAsString("type") == "imps-theory-axiom")
    assert(theaxiom.isDefined)

    val thesexp : String = theaxiom.get.getAsString("formula-sexp")
    assert(thesexp.nonEmpty)

    val sp : SymbolicExpressionParser = new SymbolicExpressionParser
    val lsp = sp.parseAll(sp.parseSEXP,thesexp)
    assert(lsp.successful)

    formula = Some(impsMathParser.makeSEXPFormula(lsp.get))
    println("     > Formula generation successful: " + formula.get.toString)

    val usgs : Option[List[Usage]] = if (usgs_prime.isEmpty) { None } else { Some(usgs_prime) }

    assert(formula.isDefined)
    Some(AxiomSpecification(formula.get, name, usgs, e.src))
  }

  /* Parser for IMPS theory axioms argument
   * used def-theory */
  def parseTheoryAxioms(e : Exp, js : List[JSONObject], thy : String) : Option[TheoryAxioms] =
  {
    assert(e.children.tail.nonEmpty)
    var axms : List[AxiomSpecification] = Nil

    for (aspec <- e.children.tail)
    {
        aspec match {
          case Exp(ds,src) => parseAxiomSpecification(Exp(ds,src), js, thy) match {
            case Some(as)  => axms = axms :+ as
            case None      => ()
          }
        }
    }

    assert(axms.nonEmpty)
    Some(TheoryAxioms(axms, e.src))
  }

  /* Parser for IMPS usages objects
   * used in: def-atomic-sort */
  def parseArgumentUsages(e : Exp) : Option[ArgumentUsages] =
  {
    /* Can contain one or multiple usages */
    var usgs : List[Usage] = List.empty

    assert(e.children.length >= 2)
    if (e.children.length >= 2)
    {
      var i : Int = 1
      while (i < e.children.length)
      {
        e.children(i) match
        {
          case Exp(List(Str("elementary-macete")),_)        => usgs = usgs :+ Usage.ELEMENTARYMACETE
          case Exp(List(Str("transportable-macete")),_)     => usgs = usgs :+ Usage.TRANSPORTABLEMACETE
          case Exp(List(Str("rewrite")),_)                  => usgs = usgs :+ Usage.REWRITE
          case Exp(List(Str("transportable-rewrite")),_)    => usgs = usgs :+ Usage.TRANSPORTABLEREWRITE
          case Exp(List(Str("simplify-logically-first")),_) => usgs = usgs :+ Usage.SIMPLIFYLOGICALLYFIRST
          case Exp(List(Str("d-r-convergence")),_)          => usgs = usgs :+ Usage.DRCONVERGENCE
          case Exp(List(Str("d-r-value")),_)                => usgs = usgs :+ Usage.DRVALUE
          case _ => ()
        }
        i += 1
      }

      assert(usgs.nonEmpty)
      Some(ArgumentUsages(usgs, e.src))

    } else { None }
  }

  /* Parser for IMPS component-theories arguments
   * used in: def-theory */
  def parseComponentTheories(e : Exp) : Option[ComponentTheories] =
  {
    // One component theory minimum
    assert(e.children.tail.nonEmpty)

    var lst : List[String] = Nil

    for (ct <- e.children.tail)
    {
      ct match {
        case Exp(List(Str(theory)),_) => lst = lst :+ theory
        case _ => ()
      }
    }

    assert(lst.nonEmpty)
    Some(ComponentTheories(lst, e.src))
  }

  /* Parser for IMPS constructor argument objects
   * used in: def-cartesian-product */
  def parseConstructor(e : Exp) : Option[Constructor] =
  {
    assert(e.children.length == 2)
    if (e.children.length == 2)
    {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(Constructor(x, e.src))
        case _                   => ???
      }
    } else { None }
  }

  /* Parser for IMPS distinct constants argument
   * used in: def-theory */
  def parseDistinctConstants(e : Exp) : Option[DistinctConstants] =
  {
    assert(e.children.tail.nonEmpty)

    var outerList : List[List[String]] = Nil

    for (d <- e.children.tail)
    {
      d match {
        case Exp(ds,_) =>
        {
          assert(ds.length >= 2)
          var innerList : List[String] = Nil

          for (c <- ds)
          {
              c match {
                case Exp(List(Str(constant)),_) => innerList = innerList :+ constant
                case _ => ()
              }
          }

          if (innerList.nonEmpty) { outerList = outerList :+ innerList }
        }
        case _ => ()
      }
    }

    assert(outerList.nonEmpty)
    Some(DistinctConstants(outerList,e.src))
  }

  /* Parser for IMPS theory argument objects
   * used in: def-atomic-sort... */
  def parseArgumentTheory(e : Exp) : Option[ArgumentTheory] =
  {
    assert(e.children.length == 2)
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(ArgumentTheory(x, e.src))
        case _                   => ???
      }
    } else { None }
  }

  /* Parser for IMPS source-theory argument objects
   * used in: def-imported-rewrite-rules... */
  def parseSourceTheory (e : Exp) : Option[SourceTheory] =
  {
    assert(e.children.length == 2)
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(SourceTheory(x, e.src))
        case _                   => ???
      }
    } else { None }
  }

  /* Parser for IMPS home-theory argument objects
   * used in: def-theorem... */
  def parseHomeTheory (e : Exp) : Option[HomeTheory] =
  {
    assert(e.children.length == 2)
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(HomeTheory(x, e.src))
        case _                   => ???
      }
    } else { None }
  }

  /* Parser for IMPS source argument objects
   * used in: def-translation... */
  def parseSource (e : Exp) : Option[TranslationSource] =
  {
    assert(e.children.length == 2)
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(TranslationSource(x, e.src))
        case _                   => ???
      }
    } else { None }
  }

  /* Parser for IMPS target argument objects
   * used in: def-translation... */
  def parseTarget (e : Exp) : Option[TranslationTarget] =
  {
    assert(e.children.length == 2)
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(TranslationTarget(x, e.src))
        case _                   => ???
      }
    } else { None }
  }

  /* Parser for IMPS macete argument (!) objects
  * used in: def-theorem... */
  def parseMacete (e : Exp) : Option[Macete] =
  {
    assert(e.children.length == 2)
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(Macete(x, e.src))
        case _                   => ???
      }
    } else { None }
  }

  /* Parser for IMPS translation argument (!) objects
  * used in: def-theorems... */
  def parseTranslation (e : Exp) : Option[ArgumentTranslation] =
  {
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(ArgumentTranslation(x, e.src))
        case _                   => None
      }
    } else { None }
  }

  /* Parser for IMPS witness argument objects
   * used in: def-atomic-sort, ... */
  def parseWitness (e : Exp) : Option[Witness] =
  {
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(Witness(x, e.src))
        case _                   => None
      }
    } else { None }
  }

  /* Parser for IMPS language argument objects
   * used in: def-quasi-constructor, ... */
  def parseArgumentLanguage(e : Exp) : Option[ArgumentLanguage] =
  {
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => Some(ArgumentLanguage(x, e.src))
        case _                   => None
      }
    } else { None }
  }

  /* Parser for IMPS sort argument objects
   * used in: def-constant, ... */
  def parseSort (e : Exp) : Option[Sort] =
  {
    assert(e.children.length == 2)
    if (e.children.length == 2) {
      e.children(1) match {
        case Exp(List(Str(x)),_) => {
          var toBeParsed : String = if (x.startsWith("\"") && x.endsWith("\""))
            { x.tail.init } else { x }

          val mp = new IMPSMathParser()
          val j  = mp.parseAll(mp.parseSort, toBeParsed)

          assert(!(j.isEmpty))
          return Some(Sort(j.get, e.src))
        }
        case _                   => ???
      }
    } else { None }
  }

  /* Parser for IMPS fixed theories objects
   * used in: def-quasi-constructor */
  def parseFixedTheories (e : Exp) : Option[FixedTheories] =
  {
    /* Can contain one or multiple usages */
    var fixed : List[String] = List.empty

    assert(e.children.length >= 2)
    if (e.children.length >= 2)
    {
      var i : Int = 1
      while (i < e.children.length)
      {
        e.children(i) match
        {
          case Exp(List(Str(x)),_) => fixed = fixed :+ x
          case _                   => None
        }
        i += 1
      }

      assert(fixed.nonEmpty)
      Some(FixedTheories(fixed, e.src))

    } else { None }
  }

  /* Parser for IMPS source theories objects
   * used in: def-imported-rewrite-rules */
  def parseSourceTheories (e : Exp) : Option[SourceTheories] =
  {
    /* Can contain one or multiple usages */
    var source : List[String] = List.empty

    assert(e.children.length >= 2)
    if (e.children.length >= 2)
    {
      var i : Int = 1
      while (i < e.children.length)
      {
        e.children(i) match
        {
          case Exp(List(Str(x)),_) => source = source :+ x
          case _                   => ???
        }
        i += 1
      }

      assert(source.nonEmpty)
      Some(SourceTheories(source, e.src))

    } else { None }
  }

  /* Parser for IMPS embedded-lang arguments
   * used in: def-language */
  def parseEmbeddedLang(e : Exp) : Option[EmbeddedLanguage] =
  {
    assert(e.children.length == 2)

    e.children(1) match {
      case Exp(List(Str(x)),_) => Some(EmbeddedLanguage(x, e.src))
      case _                   => ???
    }
  }

  /* Parser for IMPS embedded-langs arguments
   * used in: def-language */
  def parseEmbeddedLangs(e : Exp) : Option[EmbeddedLanguages] =
  {
    var lst : List[String] = Nil

    for (k <- e.children.tail)
    {
      k match {
        case Exp(List(Str(x)),_) => lst = lst :+ x
        case _                   => ???
      }
    }

    assert(lst.nonEmpty)
    Some(EmbeddedLanguages(lst, e.src))
  }


  /* Parser for IMPS base-types argument
   * used in: def-language */
  def parseLanguageBaseTypes(e : Exp) : Option[LangBaseTypes] =
  {
    var lst : List[String] = Nil

    for (k <- e.children.tail)
    {
      k match {
        case Exp(List(Str(x)),_) => lst = lst :+ x
        case _                   => ???
      }
    }

    assert(lst.nonEmpty)
    Some(LangBaseTypes(lst, e.src))
  }

  /* Parser for IMPS extensible argument
   * used in: def-language */
  def parseExtensible(e : Exp) : Option[Extensible] =
  {
    var list : List[TypeSortAList] = Nil

    for (l <- e.children.tail)
    {
      val prs : Option[TypeSortAList] = l match {
        case Exp(x,y) => parseTypeSortAList(Exp(x,y))
        case _        => ???
      }

      if (prs.isDefined) { list = list :+ prs.get }
    }

    assert(list.nonEmpty)
    Some(Extensible(list, e.src))

  }

  /* Parser for IMPS typeSortALists
   * used in: def-language (extensible, see parseExtensible) */
  def parseTypeSortAList(e : Exp) : Option[TypeSortAList] =
  {
    val ntp : Option[NumericalType] = e.children(0) match {
      case Exp(List(Str("*integer-type*")),_)  => Some(NumericalType.INTEGERTYPE)
      case Exp(List(Str("*rational-type*")),_) => Some(NumericalType.RATIONALTYPE)
      case Exp(List(Str("*octet-type*")),_)    => Some(NumericalType.OCTETTYPE)
      case _                                   => println(" > e = " +e.children(0).toString) ; ???
    }

    var srt : Option[String] = None
    if (ntp.isDefined)
    {
      srt = e.children(1) match {
        case Exp(List(Str(y)),_) => Some(y)
        case _                   => ???
      }
    }

    if (ntp.isDefined && srt.isDefined) {
      Some(TypeSortAList(ntp.get,srt.get))
    } else {
      ???
    }

  }

  /* Parser for IMPS constantsList argument
   * used in: def-language */
  def parseConstants(e : Exp) : Option[ConstantSpecifications] =
  {
    var lst : List[(String, IMPSSort)] = Nil

    // One constant notification minimum
    assert(e.children.tail.nonEmpty)

    // each c is one constant specification
    for (c <- e.children.tail)
    {
      c match
      {
        case Exp(ss,_) =>
        {
          // Constant specification has two elements
          if(!(ss.length == 2)) {
            println(" > DROPPING tail of constant specs: " + ss.toString())
          }

          ss(0) match {
            case Exp(List(Str(name)),sourceR) => ss(1) match {
              case Exp(js,_) =>
              {
                var str : String = ""
                if (js.length >= 2) {str = "["}
                for (j <- js)
                {
                  j match {
                    case Exp(List(Str(sort)),_) => {
                      if (str.length > 1) {
                        str = str + "," + sort
                      } else {
                        str = str + sort
                      }
                    }
                    case Str(snm) => str = str + snm
                    case _ => ()
                  }
                }
                str = str.trim
                if (js.length >= 2) {str = str + "]"}

                if (str.startsWith("\"") && str.endsWith("\"")) {
                  str = str.tail.init
                }

                val mp     = new IMPSMathParser()
                val parsed = mp.parseAll(mp.parseSort, str)

                assert(parsed.successful)

                lst = lst :+ (name, parsed.get)
              }
            }
            case _ => ()
          }
        }
        case _ => ()
      }
    }

    assert(lst.nonEmpty)
    Some(ConstantSpecifications(lst, e.src))
  }

  /* Parser for IMPS sorts argument
   * used in: def-language */
  def parseSortsArgument(e : Exp) : Option[SortSpecifications] =
  {
    var lst : List[(String, String)] = Nil

    // One sort notification minimum
    assert(e.children.tail.nonEmpty)

    // each c is one sort specification
    for (c <- e.children.tail)
    {
      c match
      {
        case Exp(ss,_) =>
        {
          // each sort specification has two elements
          assert(ss.length == 2)

          ss(0) match {
            case Exp(List(Str(name)),_) => ss(1) match {
              case Exp(js,_) =>
              {
                var str : String = ""
                if (js.length >= 2) {str = "("}
                for (j <- js)
                {
                  j match {
                    case Str(srt) => str = str + srt + " "
                    case _ => ()
                  }
                }
                str = str.trim
                if (js.length >= 2) {str = str + ")"}
                lst = lst :+ (name, str)
              }
            }
            case _ => ()
          }
        }
        case _ => ()
      }
    }

    assert(lst.nonEmpty)
    if (lst.isEmpty) { None } else
    {
      val mp : IMPSMathParser = new IMPSMathParser()
      var srtlst : List[(IMPSSort, IMPSSort)] = Nil
      assert(srtlst.length == 0)

      for (pr <- lst)
      {
        val sub = mp.parseAll(mp.parseSort,pr._1)
        val sup = mp.parseAll(mp.parseSort,pr._2)
        assert(!(sub.isEmpty))
        assert(!(sup.isEmpty))
        srtlst = srtlst ::: List((sub.get,sup.get))
        assert(srtlst.length >= 1)
      }

      Some(SortSpecifications(srtlst, e.src))
    }
  }

  /* Parser for IMPS fixed theories objects
   * used in: def-quasi-constructor */
  def parseAccessors(e : Exp) : Option[Accessors] =
  {
    /* Can contain one or multiple usages */
    var accs : List[String] = List.empty

    assert(e.children.length >= 2)
    if (e.children.length >= 2)
    {
      var i : Int = 1
      while (i < e.children.length)
      {
        e.children(i) match
        {
          case Exp(List(Str(x)),_) => accs = accs :+ x
          case _                   => None
        }
        i += 1
      }

      assert(accs.nonEmpty)
      Some(Accessors(accs, e.src))

    } else { None }
  }

  /**
    * Parser for IMPS proof argument objects, used in def-theorem
    * @param e The parsed LispExp (Exp, to be precise) from which to read the script.
    * @return A Proof object, which is just a wrapper for String at this time.
    */
  def parseProofScript(e : Exp) : Option[Proof] =
  {
    // TODO: This shit is whack, yo!

    var proof : String = "(proof "
    for (c <- e.children.tail) {
      proof = proof + "\n  " + c.toString
    }
    proof = proof + ")"
    Some(Proof(proof, e.src))
  }
}
