package info.kwarc.mmt.imps

import info.kwarc.mmt.api.utils.JSONObject
import info.kwarc.mmt.imps.impsMathParser.IMPSMathParser

package object impsDefFormParsers
{
  def removeWhitespace(str : String) : String = {
    return str.replaceAll(" ", "").replaceAll("\t","").replaceAll("\n","").toLowerCase
  }

  /* Parser for IMPS special form def-atomic sort
   * Documentation: IMPS manual pgs. 158, 159 */
  def parseAtomicSort (e : Exp, js : List[JSONObject]) : Option[AtomicSort] =
  {
    // Required arguments
    var name : Option[String]         = None
    var qss  : Option[IMPSMathExp]    = None
    var thy  : Option[ArgumentTheory] = None

    // Optional arguments
    var usages  : Option[ArgumentUsages]  = None
    var witness : Option[Witness] = None

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    if (cs >= 3)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name = Some(x) }
      e.children(2) match { case Exp(List(Str(y)), _) => qss  = impsMathParser.parseIMPSMath(y) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 3
      while (cs - i > 0)
      {
        e.children(i) match {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("theory")),_)  => thy     = impsArgumentParsers.parseArgumentTheory(Exp(ds,src))
            case Exp(List(Str("usages")),_)  => usages  = impsArgumentParsers.parseArgumentUsages(Exp(ds,src))
            case Exp(List(Str("witness")),_) => witness = impsArgumentParsers.parseWitness(Exp(ds,src))
            case _                           => ()
          }
          case _ => ()
        }
        i += 1
      }

      val json_theory : Option[JSONObject] = js.find(j => j.getAsString("name") == thy.get.thy)
      assert(json_theory.isDefined)
      assert(json_theory.get.getAsString("type") == "imps-theory")
      val defsorts : List[JSONObject] = json_theory.get.getAsList(classOf[JSONObject],"defsorts")
      assert(defsorts.nonEmpty)
      val thesort : Option[JSONObject] = defsorts.find(j => j.getAsString("name").toLowerCase == name.get.toLowerCase)
      assert(thesort.isDefined)
      val supersort : String = thesort.get.getAsString("sort")
      assert(supersort.endsWith(" prop)"))

      val finalsort : IMPSSort = IMPSAtomSort(supersort.takeWhile(c => c != ' ').tail)

      /* check for required arguments */
      if (name.isEmpty || qss.isEmpty || thy.isEmpty) None
      else { Some(AtomicSort(name.get, qss.get, thy.get, usages, witness, e.src, finalsort)) }

    } else { None }
  }

  /* Parser for IMPS special form def-constants
   * Documentation: IMPS manual pgs. 168, 169 */
  def parseConstant (e : Exp) : Option[Constant] =
  {
    // Required arguments
    var name   : Option[String]         = None
    var defexp : Option[IMPSMathExp]    = None
    var thy    : Option[ArgumentTheory] = None

    // Optional arguments
    var usages : Option[ArgumentUsages] = None
    var sort   : Option[Sort]   = None

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    if (cs >= 3)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }
      e.children(2) match { case Exp(List(Str(y)), _) => defexp = impsMathParser.parseIMPSMath(y) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 3
      while (cs - i > 0)
      {
        e.children(i) match {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("theory")),_) => thy    = impsArgumentParsers.parseArgumentTheory(Exp(ds,src))
            case Exp(List(Str("usages")),_) => usages = impsArgumentParsers.parseArgumentUsages(Exp(ds,src))
            case Exp(List(Str("sort")),_)   => sort   = impsArgumentParsers.parseSort(Exp(ds,src))
            case _                          => ()
          }
          case _ => ()
        }
        i += 1
      }

      /* check for required arguments */
      if (name.isEmpty || defexp.isEmpty || thy.isEmpty) None
      else { println(" >> Success while trying to parse constant " + e.children(1)) ; Some(Constant(name.get, defexp.get, thy.get, sort, usages, e.src)) }
    } else { println(" >> Failure while trying to parse constant " + e.children(1)) ; None }
  }

  /* Parser for IMPS special form def-quasi-constructor
   * Documentation: IMPS manual pgs. 177, 178 */
  def parseQuasiConstructor (e : Exp) : Option[QuasiConstructor] =
  {
    // Required arguments
    var name   : Option[String]           = None
    var expstr : Option[String]           = None
    var lang   : Option[ArgumentLanguage] = None

    // Optional arguments
    var fixed  : Option[FixedTheories] = None

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    if (cs >= 3)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }
      e.children(2) match { case Exp(List(Str(y)), _) => expstr = Some(y) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 3
      while (cs - i > 0)
      {
        e.children(i) match {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("language")),_)       => lang  = impsArgumentParsers.parseArgumentLanguage(Exp(ds,src)) ; assert(lang.isDefined)
            case Exp(List(Str("fixed-theories")),_) => fixed = impsArgumentParsers.parseFixedTheories(Exp(ds,src))    ; assert(fixed.isDefined)
            case _                           => ()
          }
          case _ => ()
        }
        i += 1
      }

      /* check for required arguments */
      if (name.isEmpty || expstr.isEmpty || lang.isEmpty) None
      else { Some(QuasiConstructor(name.get, expstr.get, lang.get, fixed, e.src)) }

    } else { None }
  }

  /* Parser for IMPS special form def-imported-rewrite-rules
   * Documentation: IMPS manual pg. 169 */
  def parseImportedRewriteRules (e : Exp) : Option[ImportedRewriteRules] =
  {
    // Required arguments
    var name   : Option[String]   = None

    // Optional arguments
    var srcTheory   : Option[SourceTheory]   = None
    var srcTheories : Option[SourceTheories] = None

    val cs : Int = e.children.length

    if (cs >= 2)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 2
      while (cs - i > 0)
      {
        e.children(i) match {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("src-theory")),_)   => srcTheory   = impsArgumentParsers.parseSourceTheory(Exp(ds,src))
            case Exp(List(Str("src-theories")),_) => srcTheories = impsArgumentParsers.parseSourceTheories(Exp(ds,src))
            case _                                => ()
          }
          case _ => ()
        }
        i += 1
      }

      /* check for required arguments */
      if (name.isEmpty || (srcTheory.isEmpty && srcTheories.isEmpty)) None
      else { Some(ImportedRewriteRules(name.get, srcTheory, srcTheories, e.src)) }

    } else { None }
  }

  /* Parser for IMPS special form def-schematic-macete
   * Documentation: IMPS manual pgs. 180, 181 */
  def parseSchematicMacete (e : Exp) : Option[SchematicMacete] =
  {
    // Required arguments
    var name    : Option[String] = None
    var formula : Option[String] = None
    var thy     : Option[ArgumentTheory] = None

    // Optional arguments
    var nullarg  : Boolean = false
    var transarg : Boolean = false

    val cs : Int = e.children.length

    /* Three arguments minimum because three req. arguments */
    if (cs >= 3)
    {
      /* Parse positional arguments */
      e.children(1) match { case Exp(List(Str(x)), _) => name    = Some(x) }
      e.children(2) match { case Exp(List(Str(y)), _) => formula = Some(y) }

      /* Parse keyword arguments, these can come in any order */
      var i : Int = 3
      while (cs - i > 0)
      {
        e.children(i) match
        {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("theory")),_) => thy = impsArgumentParsers.parseArgumentTheory(Exp(ds,src))
            case (Str("null"))              => nullarg  = true
            case (Str("transportable"))     => transarg = true
            case _                          => ()
          }
          case _ => ()
        }
        i += 1
      }

      /* check for required arguments */
      if (name.isEmpty || formula.isEmpty || thy.isEmpty) None
      else { Some(SchematicMacete(name.get, formula.get, thy.get, nullarg, transarg, e.src)) }

    } else { None }
  }

  def parseTranslation (e : Exp, js : List[JSONObject]) : Translation =
  {
    /* Required arguments */
    var name    : Option[String] = None
    var source  : Option[TranslationSource] = None
    var target  : Option[TranslationTarget] = None

    /* Optional arguments */
    var force      : Boolean = false
    var forceQL    : Boolean = false
    var dontEnrich : Boolean = false

    var fixed : Option[List[String]] = None
    var assumptions : Option[List[IMPSMathExp]] = None
    var sortpairs : Option[List[(IMPSSort,String)]] = None
    var constpairs : Option[List[(IMPSMathExp,IMPSMathExp)]] = None
    var coretrans : Option[String] = None
    var theintcheck : Option[String] = None

    /* Parse positional arguments */
    e.children(1) match { case Exp(List(Str(x)), _) => name   = Some(x) }

    /* Parse modifier and keyword arguments, these can come in any order */
    val csl : Int = e.children.length
    var i : Int = 2
    while (csl - i > 0)
    {
      e.children(i) match
      {
        case Exp(ds,src) => ds.head match
        {
          case Exp(List(Str("source")),_) => {
            source = impsArgumentParsers.parseSource(Exp(ds,src))
            assert(source.isDefined)
          }
          case Exp(List(Str("target")),_) => {
            target = impsArgumentParsers.parseTarget(Exp(ds,src))
            assert(target.isDefined)
          }
          case Exp(List(Str("assumptions")),_) => {
            ???
            assert(assumptions.isDefined)
          }
          case Exp(List(Str("fixed-theories")),_) => {
            ???
            assert(assumptions.isDefined)
          }
          case Exp(List(Str("sort-pairs")),_) => {
            ???
            assert(assumptions.isDefined)
          }
          case Exp(List(Str("constant-pairs")),_) => {
            ???
            assert(assumptions.isDefined)
          }
          case Exp(List(Str("core-translation")),_) => {
            ???
            assert(assumptions.isDefined)
          }
          case Exp(List(Str("theory-interpretation-check")),_) => {
            ???
            assert(assumptions.isDefined)
          }
          case (Str("force"))                  => force      = true
          case (Str("force-under-quick-load")) => forceQL    = true
          case (Str("dont-enrich"))            => dontEnrich = true
          case zzz                             => println(" ~~~ argument to translation not parsed: " + zzz.toString); ()
        }
        case _ => ()
      }
      i += 1
    }
    assert(name.nonEmpty)
    Translation(name.get,force,forceQL,dontEnrich,source.get,target.get,fixed,assumptions,sortpairs,constpairs,coretrans,theintcheck,e.src)
  }

  /* Parser for IMPS form def-theorem
   * Documentation: IMPS manual pgs. 184 ff. */
  def parseTheorem (e : Exp, js : List[JSONObject]) : Option[Theorem] =
  {
    /* Required arguments */
    var name    : Option[String]         = None
    var formula : Option[IMPSMathExp]    = None
    var theory  : Option[ArgumentTheory] = None

    /* Optional Arguments */
    var lemma   : Boolean = false
    var reverse : Boolean = false

    var usages : Option[ArgumentUsages]      = None
    var hmthy  : Option[HomeTheory]          = None
    var trans  : Option[ArgumentTranslation] = None
    var macete : Option[Macete]              = None
    var prf    : Option[Proof]               = None

    /* Three arguments minimum */
    val csl : Int = e.children.length
    assert(csl >= 3)

    /* Parse positional arguments, these must be in this order */
    e.children(1) match
    {
      case Exp(List(Str(x)), _) => name = Some(x)
      case Exp(List(),_)        => name = Some("()") // Theorems can have this name, according to documentation.
                                                     // Turns out parsers get easily confused here.
    }
    assert(name.isDefined)

    /* Formula will be parsed later */

    /* Parse modifier and keyword arguments, these can come in any order */
    var i : Int = 3
    while (csl - i > 0)
    {
      e.children(i) match
      {
        case Exp(ds,src) => ds.head match
        {
          case Exp(List(Str("theory")),_)      => { theory  = impsArgumentParsers.parseArgumentTheory(Exp(ds,src)) ; assert(theory.isDefined) }
          case Exp(List(Str("home-theory")),_) => { hmthy   = impsArgumentParsers.parseHomeTheory(Exp(ds,src))     ; assert(hmthy.isDefined)  }
          case Exp(List(Str("usages")),_)      => { usages  = impsArgumentParsers.parseArgumentUsages(Exp(ds,src)) ; assert(usages.isDefined) }
          case Exp(List(Str("macete")),_)      => { macete  = impsArgumentParsers.parseMacete(Exp(ds,src))         ; assert(macete.isDefined) }
          case Exp(List(Str("translation")),_) => { trans   = impsArgumentParsers.parseTranslation(Exp(ds,src))    ; assert(trans.isDefined)  }
          case Exp(List(Str("proof")),_)       => prf     = impsArgumentParsers.parseProofScript(Exp(ds,src))
          case (Str("lemma"))                  => reverse = true
          case (Str("reverse"))                => lemma   = true
          case zzz                             => println(" ~~~ argument to theorem not parsed: " + zzz.toString); ()
        }
        case _ => ()
      }
      i += 1
    }
    assert(name.nonEmpty)
    assert(theory.nonEmpty)

    val json_theory : Option[JSONObject] = js.find(j => j.getAsString("name") == theory.get.thy.toLowerCase)
    assert(json_theory.isDefined)
    assert(json_theory.get.getAsString("type") == "imps-theory")
    val theorems : List[JSONObject] = json_theory.get.getAsList(classOf[JSONObject],"theorems")
    assert(theorems.nonEmpty)
    var s : String = ""
    val thetheorem : Option[JSONObject] = if (name.get != "()")
    {
      println(" > looking for theorem " + name.get + " in theory " + theory.get.thy + " ...")
      theorems.find(j => j.getAsString("name") == name.get.toLowerCase)
    } else {
      println(" > looking for nameless theorem () in theory " + theory.get.thy + " ...")
      e.children(2) match {
        case Exp(List(Str(x)), _) => {
          s = x.tail.init
          println(" > scala theorem: " + removeWhitespace(x.tail.init))
          var tempt = theorems.find(j => removeWhitespace(j.getAsString("formula-string")) == removeWhitespace(x.tail.init))

          if (!(tempt.isDefined)) {
            val handpicked = handpick(removeWhitespace(x.tail.init))
            println(" > handpicked: " +handpicked)
            tempt = theorems.find(j => removeWhitespace(j.getAsString("formula-string")) == handpicked)
          }

          tempt
        }
        case _                    => ???
      }
    }

    if (!(thetheorem.isDefined))
    {
      assert(s != "")
      val bar = removeWhitespace(s)
      println(" > s = " + s)

      for (t <- theorems)
      {
        val foo = removeWhitespace(t.getAsString("formula-string"))
        val n = 5
        if (foo.take(n) == bar.take(n)) {
          println(" > json theorem: " + foo)
        }
      }
    }

    assert(thetheorem.isDefined)
    val thesexp : String = thetheorem.get.getAsString("formula-sexp")
    assert(thesexp.nonEmpty)

    val sp : SymbolicExpressionParser = new SymbolicExpressionParser
    val lsp = sp.parseAll(sp.parseSEXP,thesexp)
    assert(lsp.successful)

    var mpc : MathParsingContext = new MathParsingContext
    formula = Some(impsMathParser.makeSEXPFormula(lsp.get, mpc))
    println(" > Formula generation successful: " + formula.get.toString)

    assert (!(name.isEmpty || formula.isEmpty || theory.isEmpty))
    Some(Theorem(name.get, formula.get, lemma, reverse, theory.get, usages, trans, macete, hmthy, prf, e.src))
  }

  /* Parser for IMPS special form def-language
   * Documentation: IMPS manual pgs. 172 - 174 */
  def parseLanguage (e : Exp) : Option[Language] =
  {
    /* Required Arguments */
    var name : Option[String] = None

    /* Optional Arguments */
    var embedlang  : Option[EmbeddedLanguage]       = None
    var embedlangs : Option[EmbeddedLanguages]      = None
    var bstps      : Option[LangBaseTypes]          = None
    var extens     : Option[Extensible]             = None
    var srts       : Option[SortSpecifications]     = None
    var cnstnts    : Option[ConstantSpecifications] = None

    if (e.children.nonEmpty)
    {
      /* Parse positional arguments, these must be in this order */
      e.children(1) match {
        case Exp(List(Str(x)), _) => name = Some(x)
      }

      /* Parse keyword arguments. These can be in any order */
      for (c <- e.children.tail)
      {
        c match
        {
          case Exp(ds,src) => ds.head match
          {
            case Exp(List(Str("embedded-languages")),_) => embedlangs = impsArgumentParsers.parseEmbeddedLangs(Exp(ds,src))
            case Exp(List(Str("embedded-language")),_)  => embedlang  = impsArgumentParsers.parseEmbeddedLang(Exp(ds,src))
            case Exp(List(Str("base-types")),_)         => bstps      = impsArgumentParsers.parseLanguageBaseTypes(Exp(ds,src))
            case Exp(List(Str("sorts")),_)              => srts       = impsArgumentParsers.parseSortsArgument(Exp(ds,src))
            case Exp(List(Str("extensible")),_)         => extens     = impsArgumentParsers.parseExtensible(Exp(ds,src))
            case Exp(List(Str("constants")),_)          => cnstnts    = impsArgumentParsers.parseConstants(Exp(ds,src))
            case _                                      => ()
          }
          case _ => ()
        }

      }

      if (name.isEmpty) { None }
      else { Some(Language(name.get, embedlang, embedlangs, bstps, extens, srts, cnstnts, e.src)) }
    } else { None }
  }

  /* Parser for IMPS special form def-language
   * Documentation: IMPS manual pgs. 186-188 */
  def parseTheory(e : Exp) : Option[Theory] =
  {
    /* Required Arguments */
    var name : Option[String] = None

    /* Optional Arguments */
    var lang      : Option[ArgumentLanguage]  = None
    var cmpntthrs : Option[ComponentTheories] = None
    var axioms    : Option[TheoryAxioms]      = None
    var dstnct    : Option[DistinctConstants] = None

    if (e.children.nonEmpty)
    {
      /* Parse positional arguments, these must be in this order */
      e.children(1) match {
        case Exp(List(Str(x)), _) => name = Some(x)
      }

      /* Parse keyword arguments. These can be in any order */
      for (c <- e.children.tail)
      {
        c match {
          case Exp(ds, src) => ds.head match {
            case Exp(List(Str("language")), _) => lang = impsArgumentParsers.parseArgumentLanguage(Exp(ds, src))
            case Exp(List(Str("component-theories")), _) => cmpntthrs = impsArgumentParsers.parseComponentTheories(Exp(ds, src))
            case Exp(List(Str("distinct-constants")), _) => dstnct = impsArgumentParsers.parseDistinctConstants(Exp(ds, src))
            case Exp(List(Str("axioms")), _) => axioms = impsArgumentParsers.parseTheoryAxioms(Exp(ds,src))
            case _ => ()
          }
          case _ => ()
        }
      }

      if (name.isEmpty) { None }
      else { Some(Theory(name.get, lang, cmpntthrs, axioms, dstnct, e.src)) }
    } else { None }
  }

  /* Welcome to the most shameful part of the codebase...
   */
  def handpick(str: String) : String =
  {
    str match {
      case "forall(x:rr,#(abs(x)))" => "total_q{abs,[rr,rr]}"
      case "forall(x,y:rr,#(max(x,y)))" => "total_q{max,[rr,rr,rr]}"
      case "forall(x,y:pp,#(dist(x,y)))" => "total_q{dist,[pp,pp,rr]}"
      case "total_q(+_kk,[kk,kk,kk])" => "total_q{+_kk,[kk,kk,kk]}"
      case "total_q(*_kk,[kk,kk,kk])" => "total_q{*_kk,[kk,kk,kk]}"
      case "total_q(-_kk,[kk,kk])" => "total_q{-_kk,[kk,kk]}"
      case _ => str
    }

  }
}
