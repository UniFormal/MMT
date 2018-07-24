package info.kwarc.mmt.imps

import info.kwarc.mmt.api.utils.JSONObject
import info.kwarc.mmt.imps.NumericalType.{NumericalType}
import info.kwarc.mmt.imps.ParseMethod.{ParseMethod}
import info.kwarc.mmt.imps.ParserWithSourcePosition._
import info.kwarc.mmt.imps.Usage.Usage

class DefFormParsers(js : List[JSONObject])
{
  // ######### Sort Parsers

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

  // ######### Argument Parsers

  lazy val number     : Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }

  lazy val parseName  : Parser[String] = "[^()\"\t\r\n ]+".r
  lazy val parseTName : Parser[Name]   = fullParser(
    (parseName ^^ { case (nm) => Name(nm,None,None)}) | ("()" ^^ {case (_) => Name("()",None,None)})
  )

  // ToDo: nested strings could be a problem. Do those occur?
  lazy val parseDefString : Parser[DefString] = fullParser("\"[^\"]+\"".r ^^ {case (s) => DefString(s,None,None)})

  lazy val parseArgTheory : Parser[ArgTheory] =
    fullParser(("(theory" ~> parseTName <~ ")") ^^ { case (n) => ArgTheory(n,None,None) })

  lazy val parseArgTranslation : Parser[ArgTranslation] =
    fullParser(("(translation" ~> parseTName <~ ")") ^^ { case (n) => ArgTranslation(n,None,None) })

  lazy val parseArgLanguage : Parser[ArgLanguage] = {
    fullParser(("(language" ~> parseTName <~ ")") ^^ { case (n) => ArgLanguage(n, None, None) })
  }

  lazy val parseArgWitness : Parser[ArgWitness] =
    fullParser(("(witness" ~> parseDefString <~ ")") ^^ {case (s) => ArgWitness(s,None,None)})

  lazy val parseUsage : Parser[Usage] = parseName ^^ {
    case "elementary-macete"        => Usage.ELEMENTARYMACETE
    case "transportable-macete"     => Usage.TRANSPORTABLEMACETE
    case "rewrite"                  => Usage.REWRITE
    case "transportable-rewrite"    => Usage.TRANSPORTABLEREWRITE
    case "simplify-logically-first" => Usage.SIMPLIFYLOGICALLYFIRST
    case "d-r-convergence"          => Usage.DRCONVERGENCE
    case "d-r-value"                => Usage.DRVALUE
  }

  lazy val parseArgUsages  : Parser[ArgUsages] =
    fullParser(("(usages" ~> rep1(parseUsage) <~ ")") ^^ { case (us) => ArgUsages(us,None,None) })

  lazy val parseArgSort    : Parser[ArgSort] =
    fullParser(("(sort" ~> (("\""?) ~> parseSort <~ ("\""?)) <~ ")") ^^ {case (n) => ArgSort(n,None,None)})

  lazy val parseArgFixedTheories  : Parser[ArgFixedTheories] =
    fullParser(("(fixed-theories" ~> rep1(parseTName) <~ ")") ^^ { case (ts) => ArgFixedTheories(ts,None,None) })

  lazy val parseModTransportable  : Parser[ModTransportable] =
    fullParser("transportable" ^^ {case (_) => ModTransportable(None,None)} )

  lazy val parseModNull : Parser[ModNull] = fullParser("null" ^^ {case (_) => ModNull(None,None)} )

  lazy val parseSpecName : Parser[MSpecName] = fullParser(parseTName ^^ {case (n) => MSpecName(n,None,None)})

  lazy val parseSpecSeries : Parser[MSpecSeries] = fullParser(
    "(series" ~> rep1(parseSpec) <~ ")" ^^ { case (s) => MSpecSeries(s,None,None)}
  )

  lazy val parseSpecRepeat : Parser[MSpecRepeat] = fullParser(
    "(repeat" ~> rep1(parseSpec) <~ ")" ^^ { case (s) => MSpecRepeat(s,None,None)}
  )

  lazy val parseSpecSequential : Parser[MSpecSequential] = fullParser(
    "(sequential" ~> rep1(parseSpec) <~ ")" ^^ { case (s) => MSpecSequential(s,None,None)}
  )

  lazy val parseSpecSound : Parser[MSpecSound] = fullParser(
    "(sound" ~> (parseSpec ~ parseSpec ~ parseSpec) <~ ")" ^^ { case x ~ y ~ z => MSpecSound(x,y,z,None,None)}
  )

  lazy val parseSpecParallel : Parser[MSpecParallel] = fullParser(
    "(parallel" ~> rep1(parseSpec) <~ ")" ^^ { case (s) => MSpecParallel(s,None,None)}
  )

  lazy val parseSpecWithoutMinorPremises : Parser[MSpecWithoutMinorPremises] = fullParser(
    "(without-minor-premises" ~> parseSpec <~ ")" ^^ { case (s) => MSpecWithoutMinorPremises(s,None,None)}
  )

  val allMSpecs : List[Parser[MaceteSpec]] = List(
    parseSpecSeries, parseSpecRepeat, parseSpecSequential, parseSpecSound,
    parseSpecParallel, parseSpecWithoutMinorPremises, parseSpecName
  )

  lazy val parseSpec : Parser[MaceteSpec] = anyOf(allMSpecs)

  lazy val parseArgSourceTheory : Parser[ArgSourceTheory] = fullParser(
    "(source-theory" ~> parseTName <~ ")" ^^ { case (n) => ArgSourceTheory(n,None,None) }
  )

  lazy val parseArgSourceTheories : Parser[ArgSourceTheories] = fullParser(
    "(source-theories" ~> rep1(parseTName) <~ ")" ^^ { case (ns) => ArgSourceTheories(ns,None,None) }
  )

  lazy val parseInductionPrinciple : Parser[ArgInductionPrinciple] = fullParser(
    (parseDefString | parseTName) ^^ {
      case n@Name(_,_,_)      => ArgInductionPrinciple(Left(n),None,None)
      case d@DefString(_,_,_) => ArgInductionPrinciple(Right(d),None,None)
      case p@_                => ??!(p)
    }
  )

  lazy val parseArgBaseCaseHook : Parser[ArgBaseCaseHook] = fullParser(
    "(base-case-hook" ~> parseTName <~ ")" ^^ { case (n) => ArgBaseCaseHook(n,None,None) }
  )

  lazy val parseArgInductionStepHook : Parser[ArgInductionStepHook] = fullParser(
    "(induction-step-hook" ~> parseTName <~ ")" ^^ { case (n) => ArgInductionStepHook(n,None,None) }
  )

  lazy val parseArgDontUnfold : Parser[ArgDontUnfold] = fullParser(
    "(dont-unfold" ~> rep1(parseTName) <~ ")" ^^ { case (ns) => ArgDontUnfold(ns,None,None) }
  )

  lazy val parseArgEmbeddedLang : Parser[ArgEmbeddedLang] = fullParser(
    "(embedded-language" ~> parseTName <~ ")" ^^ { case (n) => ArgEmbeddedLang(n,None,None) }
  )

  lazy val parseArgEmbeddedLangs : Parser[ArgEmbeddedLangs] = fullParser(
    "(embedded-language" ~> rep1(parseTName) <~ ")" ^^ { case (ns) => ArgEmbeddedLangs(ns,None,None) }
  )

  lazy val parseArgBaseTypes : Parser[ArgBaseTypes] = fullParser(
    "(base-types" ~> rep1(parseAtomicSort) <~ ")" ^^ { case (ns) => ArgBaseTypes(ns,None,None) }
  )

  lazy val parseArgSortSpec : Parser[ArgSortSpec] = fullParser(
    "(" ~> (parseSort ~ parseSort) <~ ")" ^^ { case (sub ~ sup) => ArgSortSpec(sub,sup,None,None) }
  )

  lazy val parseArgSorts : Parser[ArgSorts] = fullParser(
    "(sorts" ~> rep1(parseArgSortSpec) <~ ")" ^^ { case (ns) => ArgSorts(ns,None,None) }
  )

  lazy val parseNumericalType : Parser[NumericalType] = parseName ^^ {
    case "*integer-type*"  => NumericalType.INTEGERTYPE
    case "*rational-type*" => NumericalType.RATIONALTYPE
    case "*octet-type*"    => NumericalType.OCTETTYPE
  }

  lazy val parseArgTypeSortAList : Parser[ArgTypeSortAList] = fullParser(
    "(" ~> (parseNumericalType ~ parseSort) <~ ")" ^^ { case (num ~ srt) => ArgTypeSortAList(num,srt,None,None) }
  )

  lazy val parseArgExtensible : Parser[ArgExtensible] = fullParser(
    "(extensible" ~> rep1(parseArgTypeSortAList) <~ ")" ^^ { case (als) => ArgExtensible(als,None,None) }
  )

  lazy val parseArgConstantSpec : Parser[ArgConstantSpec] = fullParser(
    "(" ~> (parseTName ~ parseSort) <~ ")" ^^ { case (nm ~ srt) => ArgConstantSpec(nm,srt,None,None) }
  )

  lazy val parseArgConstants : Parser[ArgConstants] = fullParser(
    "(constants" ~> rep1(parseArgConstantSpec) <~ ")" ^^ { case (ns) => ArgConstants(ns,None,None) }
  )

  lazy val parseArgRenamerPair : Parser[ArgRenamerPair] = fullParser(
    "(" ~> parseTName ~ parseTName <~ ")" ^^ { case (old ~ nu) => ArgRenamerPair(old,nu,None,None) }
  )

  lazy val parseArgRenamerPairs : Parser[ArgPairs] = fullParser(
    "(pairs" ~> rep1(parseArgRenamerPair) <~ ")" ^^ { case (ps) => ArgPairs(ps,None,None) }
  )

  lazy val parseArgToken : Parser[ArgToken] = fullParser(
    "(token" ~> (("\""?) ~> rep1(parseName) <~ ("\""?)) <~ ")" ^^ { case (muls) => ArgToken(muls,None,None) }
  )

  lazy val parseArgBinding : Parser[ArgBinding] = fullParser(
    "(binding" ~> number <~ ")" ^^ { case (n) => ArgBinding(n,None,None) }
  )

  lazy val parseArgTable : Parser[ArgTable] = fullParser(
    "(table" ~> parseTName <~ ")" ^^ { case (t) => ArgTable(t,None,None) }
  )

  lazy val parseArgMethod : Parser[ArgMethod] = fullParser(
    "(method" ~> parseTName <~ ")" ^^ { case (m) => ArgMethod(m,None,None) }
  )

  lazy val parseParseMethod : Parser[ParseMethod] = parseName ^^ {
      case "prefix-operator-method"                    => ParseMethod.PREFIXMETHOD
      case "infix-operator-method"                     => ParseMethod.INFIXMETHOD
      case "postfix-operator-method"                   => ParseMethod.POSTFIXMETHOD
      case "negation-operator-method"                  => ParseMethod.NEGATIONMETHOD
      case "table-operator-method"                     => ParseMethod.TABLEMETHOD
      case "parse-indicator-constructor-both-syntaxes" => ParseMethod.INDBOTHSYN
      case "prefix-sort-dependent-operator-method"     => ParseMethod.PREFSORTDEPOM
      case "null-call-method-terminator"               => ParseMethod.NULLCALL
    }

  lazy val parseArgLeftMethod : Parser[ArgLeftMethod] = fullParser(
    "(left-method" ~> parseParseMethod <~ ")" ^^ { case (m) => ArgLeftMethod(m,None,None) }
  )

  lazy val parseArgNullMethod : Parser[ArgNullMethod] = fullParser(
    "(null-method" ~> parseParseMethod <~ ")" ^^ { case (m) => ArgNullMethod(m,None,None) }
  )

  lazy val parseModTex : Parser[ModTex] = fullParser("tex" ^^ {case (_) => ModTex(None,None)} )

  lazy val parseModReverse : Parser[ModReverse] = fullParser("reverse" ^^ {case (_) => ModReverse(None,None)} )

  lazy val parseModLemma : Parser[ModLemma] = fullParser("lemma" ^^ {case (_) => ModLemma(None,None)} )

  lazy val parseArgMacete : Parser[ArgMacete] = fullParser(
    "(macete" ~> parseTName <~ ")" ^^ { case (m) => ArgMacete(m,None,None) }
  )

  lazy val parseArgHomeTheory : Parser[ArgHomeTheory] = fullParser(
    "(home-theory" ~> parseTName <~ ")" ^^ { case (m) => ArgHomeTheory(m,None,None) }
  )

  def balance(chars:List[Char], level:Int=0): Boolean = {
    if (level < 0) return false;

    val nextParen = chars.dropWhile(char => char != ')' && char != '(')
    if (nextParen.isEmpty) {
      level == 0
    } else if (nextParen.head == '(') {
      balance(nextParen.tail, level + 1)
    } else {
      assert(nextParen.head == ')')
      balance(nextParen.tail, level - 1)
    }
  }

  lazy val stuff            : Parser[String] = "[^()]+".r
  lazy val bracketed        : Parser[String] = "(" ~> parseProofScript <~ ")" ^^ {case (s) => "(" + s + ")"}
  lazy val parseProofScript : Parser[String] = rep1(bracketed|stuff) ^^ {_.mkString(" ")}

  lazy val parseArgProof : Parser[ArgProof] = fullParser(
    "(proof" ~> parseProofScript <~ ")" ^^ { case (m) => ArgProof(m,None,None) }
  )

  lazy val parseDefStringOrName : Parser[ODefString] = fullParser((parseDefString | parseTName) ^^ {
      case (d : DefString) => ODefString(Left(d),None,None)
      case (n : Name)      => ODefString(Right(n),None,None)
  })

  lazy val parseArgComponentTheories : Parser[ArgComponentTheories] = fullParser(
    "(component-theories" ~> rep1(parseTName) <~ ")" ^^ { case (ns) => ArgComponentTheories(ns,None,None) }
  )

  lazy val parseArgDistinctConstants : Parser[ArgDistinctConstants] = fullParser(
    "(distinct-constants" ~> rep1("(" ~> rep1(parseTName) <~ ")") <~ ")" ^^ { case (ll) => ArgDistinctConstants(ll,None,None)}
  )

  lazy val parseAxiomSpec : Parser[AxiomSpec] = fullParser(
    "(" ~> (parseName?) ~ parseDefString ~ (rep1(parseUsage)?) <~ ")" ^^ {case n ~ d ~ us => AxiomSpec(n,d,us,None,None)}
  )

  lazy val parseArgAxioms : Parser[ArgAxioms] = fullParser(
    "(axioms" ~> rep1(parseAxiomSpec) <~ ")" ^^ { case (as) => ArgAxioms(as,None,None) }
  )

  lazy val parseArgOverloadingPairs : Parser[ArgOverloadingPair] = fullParser(
    "(" ~> parseTName ~ parseTName <~ ")" ^^ {case p1 ~ p2 => ArgOverloadingPair(p1,p2,None,None)}
  )

  lazy val parseModForce : Parser[ModForce] = fullParser("force" ^^ {case (_) => ModForce(None,None)})

  lazy val parseModForceU : Parser[ModForceUnderQuickLoad] = fullParser("force-under-quick-load" ^^ {case (_) => ModForceUnderQuickLoad(None,None)})

  lazy val parseModDontEnrich : Parser[ModDontEnrich] = fullParser("dont-enrich" ^^ {case (_) => ModDontEnrich(None,None)})

  lazy val parseArgSource : Parser[ArgSource] = fullParser(
    "(source" ~> parseTName <~ ")" ^^ { case (n) => ArgSource(n, None, None) }
  )

  lazy val parseArgTarget : Parser[ArgTarget] = fullParser(
    "(target" ~> parseTName <~ ")" ^^ { case (n) => ArgTarget(n, None, None) }
  )

  lazy val parseArgAssumptions : Parser[ArgAssumptions] = fullParser(
    "(assumptions" ~> rep1(parseDefString) <~ ")" ^^ { case (as) => ArgAssumptions(as, None, None) }
  )

  lazy val pv0 = parseTName ^^ { case (n) => (n,0) }
  lazy val pv1 = parseDefString ^^ { case (d) => (d,1) }
  lazy val pv2 = "(pred" ~> parseDefString <~ ")" ^^ { case (d) => (d,2) }
  lazy val pv3 = "(indic" ~> parseDefString <~ ")" ^^ { case (d) => (d,3) }

  lazy val parseSortPairSpec : Parser[ArgSortPairSpec] = fullParser(
    "(" ~> parseTName ~ (pv0 | pv1 | pv2 | pv3) <~ ")" ^^ { case (nm : Name) ~ t => t match {
      case (n : Name, 0)      => ArgSortPairSpec(nm,Left(Left(n)),None,None)
      case (d : DefString, 1) => ArgSortPairSpec(nm,Left(Right(d)),None,None)
      case (d : DefString, 2) => ArgSortPairSpec(nm,Right(Left(d)),None,None)
      case (d : DefString, 3) => ArgSortPairSpec(nm,Right(Right(d)),None,None)
      case _ => ??!(t)
    }
    }
  )

  lazy val parseArgSortPairs : Parser[ArgSortPairs] = fullParser(
    "(sort-pairs" ~> rep1(parseSortPairSpec) <~ ")" ^^ { case (sps) => ArgSortPairs(sps, None, None) }
  )

  lazy val parseArgConstPairSpec : Parser[ArgConstPairSpec] = fullParser(
    "(" ~> parseTName ~ parseDefStringOrName <~ ")" ^^ { case (f ~ b) => ArgConstPairSpec(f,b,None,None) }
  )

  lazy val parseArgConstPairs : Parser[ArgConstPairs] = fullParser(
    ("(constant-pairs" ~> rep1(parseArgConstPairSpec) <~ ")") ^^ { case (as) => ArgConstPairs(as, None, None) }
  )

  lazy val parseArgCoreTranslation : Parser[ArgCoreTranslation] = fullParser(
    "(core-translation" ~> parseTName <~ ")" ^^ { case (n) => ArgCoreTranslation(n, None, None) }
  )

  lazy val parseArgTheoryInterpretationCheck : Parser[ArgTheoryInterpretationCheck] = fullParser(
    "(theory-interpretation-check" ~> parseTName <~ ")" ^^ { case (n) => ArgTheoryInterpretationCheck(n, None, None) }
  )

  lazy val parseArgNameList : Parser[ArgNameList] = fullParser(
    singleOrList(parseTName) ^^ { case (ns) => ArgNameList(ns,None,None) }
  )

  lazy val parseArgDefStringList : Parser[ArgDefStringList] = fullParser(
    singleOrList(parseDefString) ^^ { case (ds) => ArgDefStringList(ds,None,None) }
  )

  lazy val parseArgDefinitionName : Parser[ArgDefinitionName] = fullParser(
    "(definition-name" ~> parseTName <~ ")" ^^ { case (nm) => ArgDefinitionName(nm,None,None) }
  )

  // ######### Full Def-Form Parsers

  val pHeralding  : Parser[Heralding] = composeParser(
    "herald",
    List(parseTName),
    Nil,
    Nil,
    Heralding
  )

  val pAtomicSort : Parser[DFAtomicSort] = composeParser(
    "def-atomic-sort",
    List(parseTName, parseDefString),
    Nil,
    List((parseArgTheory, R), (parseArgUsages, O), (parseArgWitness, O)),
    DFAtomicSort
  )

  val pCompoundMacete : Parser[DFCompoundMacete] = composeParser(
    "def-compound-macete",
    List(parseTName, parseSpec),
    Nil,
    Nil,
    DFCompoundMacete
  )

  val pConstant : Parser[DFConstant] = composeParser(
    "def-constant",
    List(parseTName, parseDefString),
    Nil,
    List((parseArgTheory,R),(parseArgSort,O),(parseArgUsages,O)),
    DFConstant
  )

  val pImportedRewriteRules : Parser[DFImportedRewriteRules] = composeParser(
    "def-imported-rewrite-rules",
    List(parseTName),
    Nil,
    List((parseArgSourceTheory,O), (parseArgSourceTheories,O)),
    DFImportedRewriteRules
  )

  val pInductor : Parser[DFInductor] = composeParser(
    "def-inductor",
    List(parseTName, parseInductionPrinciple),
    Nil,
    List((parseArgTheory,R), (parseArgTranslation,O), (parseArgBaseCaseHook,O),
      (parseArgInductionStepHook,O), (parseArgDontUnfold,O)),
    DFInductor
  )

  val pLanguage : Parser[DFLanguage] = composeParser(
    "def-language",
    List(parseTName),
    Nil,
    List(parseArgEmbeddedLang, parseArgEmbeddedLangs, parseArgBaseTypes, parseArgSorts,
      parseArgExtensible, parseArgConstants).map(p => (p,O)), // all optional
    DFLanguage
  )

  val pQuasiConstructor : Parser[DFQuasiConstructor] = composeParser(
    "def-quasi-constructor",
    List(parseTName, parseDefString),
    Nil,
    List((parseArgLanguage,R),(parseArgFixedTheories,O)),
    DFQuasiConstructor
  )

  val pRenamer : Parser[DFRenamer] = composeParser(
    "def-renamer",
    List(parseTName),
    Nil,
    List((parseArgRenamerPairs,O)),
    DFRenamer
  )

  val pSchematicMacete : Parser[DFSchematicMacete] = composeParser(
    "def-schematic-macete",
    List(parseTName, parseDefString),
    List(parseModNull, parseModTransportable),
    List((parseArgTheory,R)),
    DFSchematicMacete
  )

  val pParseSyntax : Parser[DFParseSyntax] = composeParser(
    "def-parse-syntax",
    List(parseTName),
    Nil,
    List((parseArgToken,O),(parseArgLeftMethod,O),(parseArgNullMethod,O),(parseArgTable,O),(parseArgBinding,R)),
    DFParseSyntax
  )

  val pPrintSyntax : Parser[DFPrintSyntax] = composeParser(
    "def-print-syntax",
    List(parseTName),
    List(parseModTex),
    List((parseArgToken,O),(parseArgMethod,O),(parseArgTable,O),(parseArgBinding,R)),
    DFPrintSyntax
  )

  val pTheorem : Parser[DFTheorem] = {
    DFTheorem.js = this.js
    composeParser(
      "def-theorem",
      List(parseTName, parseDefStringOrName),
      List(parseModReverse, parseModLemma),
      List((parseArgTheory,R)) :::
        List(parseArgUsages, parseArgTranslation, parseArgMacete,parseArgHomeTheory,parseArgProof).map(p => (p,O)),
      DFTheorem
    )
  }

  val pTheory : Parser[DFTheory] = {
    DFTheorem.js = this.js
    composeParser(
      "def-theory",
      List(parseTName),
      Nil,
      List(parseArgLanguage,parseArgAxioms,parseArgComponentTheories,parseArgDistinctConstants).map(p => (p,O)),
      DFTheory
    )
  }

  lazy val pOverloading : Parser[DFOverloading] = fullParser(
    "(def-overloading" ~> parseTName ~ rep1(parseArgOverloadingPairs) <~ ")" ^^ { case t ~ ps =>
      DFOverloading.build(List(t,ps))
    }
  )

  lazy val pTranslation : Parser[DFTranslation] = {
    DFTheorem.js = this.js
    composeParser(
      "def-translation",
      List(parseTName),
      List(parseModForce,parseModForceU,parseModDontEnrich),
      List(parseArgSource, parseArgTarget).map(p => (p,R)) :::
        List(parseArgAssumptions, parseArgFixedTheories, parseArgSortPairs, parseArgConstPairs,
          parseArgCoreTranslation, parseArgTheoryInterpretationCheck).map(p => (p,O)),
      DFTranslation
    )
  }

  lazy val pRecursiveConstant : Parser[DFRecursiveConstant] = {
    DFTheorem.js = this.js
    composeParser(
      "def-recursive-constant",
      List(parseArgNameList, parseArgDefStringList),
      Nil,
      List((parseArgTheory,R),(parseArgUsages,O),(parseArgDefinitionName,O)),
      DFRecursiveConstant
    )
  }

  // ######### Complete Parsers

  val allDefFormParsers : List[Parser[DefForm]] = List(
    parseLineComment, pHeralding, pAtomicSort, pConstant, pQuasiConstructor, pSchematicMacete, pCompoundMacete,
    pInductor, pImportedRewriteRules, pLanguage, pRenamer, pPrintSyntax, pParseSyntax, pTheorem, pTheory, pOverloading,
    pTranslation, pRecursiveConstant
  )

  lazy val parseImpsSource : PackratParser[List[DefForm]] = { rep1(anyOf(allDefFormParsers)) }

}
