package info.kwarc.mmt.imps

import info.kwarc.mmt.api.utils.JSONObject
import info.kwarc.mmt.imps.NumericalType.NumericalType
import info.kwarc.mmt.imps.OperationType.OperationType
import info.kwarc.mmt.imps.ParseMethod.ParseMethod
import info.kwarc.mmt.imps.ParserWithSourcePosition._
import info.kwarc.mmt.imps.Usage.Usage

class DefFormParsers(js : List[JSONObject])
{
  // ######### Sort Parsers

  lazy val parseAtomicSort : PackratParser[IMPSAtomSort] = {
    ("[^,\\]):\\s\"]+".r) ^^ {case sort => IMPSAtomSort(sort)}
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

  lazy val number       : Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  lazy val parseTNumber : Parser[Number] = fullParser(
    number ^^ { case (n) => Number(n,None,None) }
  )

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
    case "transportable"            => Usage.TRANSPORTABLE
  }

  lazy val parseArgUsages  : Parser[ArgUsages] =
    fullParser(("(usages" ~> rep(parseUsage) <~ ")") ^^ { case (us) => ArgUsages(us,None,None) })

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
    "(embedded-languages" ~> rep1(parseTName) <~ ")" ^^ { case (ns) => ArgEmbeddedLangs(ns,None,None) }
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
    "(" ~> (parseTName ~ (("\""?) ~> parseSort <~ ("\""?))) <~ ")" ^^ { case (nm ~ srt) => ArgConstantSpec(nm,srt,None,None) }
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
    "(token" ~> parseScript <~ ")" ^^ { case (muls) => ArgToken(muls,None,None) }
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

  lazy val parseModTex : Parser[ModTex] = fullParser(("tex" | "TeX" | "TEX") ^^ {case (_) => ModTex(None,None)} )

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


  lazy val stuff       : Parser[String] = "[^()]+".r
  lazy val bracketed   : Parser[String] = "(" ~> (parseScript | "") <~ ")" ^^ {case (s) => "(" + s.toString + ")"}
  lazy val parseScript : Parser[Script] = fullParser(
    rep1(bracketed|stuff) ^^ {case (ss) => Script(ss.mkString(" "),None,None) }
  )

  lazy val parseArgProof : Parser[ArgProof] = fullParser(
    "(proof" ~> parseScript <~ ")" ^^ { case (m) => ArgProof(m,None,None) }
  )

  lazy val parseArgComponentTheories : Parser[ArgComponentTheories] = fullParser(
    "(component-theories" ~> rep1(parseTName) <~ ")" ^^ { case (ns) => ArgComponentTheories(ns,None,None) }
  )

  lazy val parseArgDistinctConstants : Parser[ArgDistinctConstants] = fullParser(
    "(distinct-constants" ~> rep1("(" ~> rep1(parseTName) <~ ")") <~ ")" ^^ { case (ll) => ArgDistinctConstants(ll,None,None)}
  )

  lazy val parseAxiomSpec : Parser[AxiomSpec] = fullParser(
    "(" ~> (parseName?) ~ parseDefString ~ (rep1(parseUsage)?) <~ ")" ^^ {case n ~ d ~ us => AxiomSpec(n,d,None,us,None,None)}
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
    "(assumptions" ~> rep1(parseDefString) <~ ")" ^^ { case (as) => ArgAssumptions(as, Nil, None, None) }
  )

  lazy val pv0 = parseTName ^^ { case (n) => (n,0) }
  lazy val pv1 = parseDefString ^^ { case (d) => (d,1) }
  lazy val pv2 = "(pred" ~> parseDefString <~ ")" ^^ { case (d) => (d,2) }
  lazy val pv3 = "(indic" ~> parseDefString <~ ")" ^^ { case (d) => (d,3) }

  lazy val parseSortPairSpec : Parser[ArgSortPairSpec] = fullParser(
    "(" ~> parseTName ~ (pv0 | pv1 | pv2 | pv3) <~ ")" ^^ { case (nm : Name) ~ t => t match {
      case (n : Name, 0)      => ArgSortPairSpec(nm,Left(Left(n)),None,None,None)
      case (d : DefString, 1) => ArgSortPairSpec(nm,Left(Right(d)),None,None,None)
      case (d : DefString, 2) => ArgSortPairSpec(nm,Right(Left(d)),None,None,None)
      case (d : DefString, 3) => ArgSortPairSpec(nm,Right(Right(d)),None,None,None)
      case _ => ??!(t)
    }
    }
  )

  lazy val parseArgSortPairs : Parser[ArgSortPairs] = fullParser(
    "(sort-pairs" ~> rep1(parseSortPairSpec) <~ ")" ^^ { case (sps) => ArgSortPairs(sps, None, None) }
  )

  lazy val parseDefStringOrName : Parser[ODefString] = fullParser(
    (parseDefString ^^ { case (d) => ODefString(Left((d,None)),None,None) }) | (parseTName ^^ { case (n) => ODefString(Right(n),None,None) })
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

  lazy val parseArgRenamer : Parser[ArgRenamer] = fullParser(
    "(renamer" ~> parseTName <~ ")" ^^ { case (r) => ArgRenamer(r,None,None) }
  )

  lazy val parseOperationType : Parser[OperationType] = parseName ^^ {
    case "+"    => OperationType.PLUS
    case "-"    => OperationType.MINUS
    case "*"    => OperationType.TIMES
    case "/"    => OperationType.DIV
    case "^"    => OperationType.EXP
    case "sub"  => OperationType.SUB
    case "zero" => OperationType.ZERO
    case "unit" => OperationType.UNIT
  }

  lazy val parseOperationAlist : Parser[ArgOperationAlist] = fullParser(
    "(" ~> parseOperationType ~ parseTName <~ ")" ^^ {case ot ~ n => ArgOperationAlist(ot,n,None,None) }
  )

  lazy val parseArgOperations : Parser[ArgOperations] = fullParser(
    "(operations" ~> rep1(parseOperationAlist) <~ ")" ^^ { case (oas) => ArgOperations(oas,None,None) }
  )

  lazy val parseArgScalars : Parser[ArgScalars] = fullParser(
    "(scalars" ~> parseNumericalType <~ ")" ^^ { case (nt) => ArgScalars(nt,None,None) }
  )

  lazy val parseModUseNumerals : Parser[ModUseNumerals] = fullParser(
    "use-numerals-for-ground-terms" ^^ { case _ => ModUseNumerals(None,None) }
  )

  lazy val parseModCommutes : Parser[ModCommutes] = fullParser(
    "commutes" ^^ { case _ => ModCommutes(None,None) }
  )

  lazy val parseQDFSpecForm : Parser[QDFSpecForm] = composeParser(
    "",
    Nil,
    List(parseModCommutes,parseModUseNumerals),
    List(parseArgScalars,parseArgOperations).map(p => (p,O)),
    QDFSpecForm
  )

  lazy val parseArgSpecForms : Parser[ArgSpecForms] = fullParser(
    (parseQDFSpecForm ^^ { case (sf) => ArgSpecForms(Right(sf),None,None) }) | (parseTName ^^ { case (n) => ArgSpecForms(Left(n),None,None) })
  )

  lazy val parseArgBase : Parser[ArgBase] = fullParser(
    "(base" ~> parseArgSpecForms <~ ")" ^^ { case (b) => ArgBase(b,None,None) }
  )

  lazy val parseArgExponent : Parser[ArgExponent] = fullParser(
    "(exponent" ~> parseArgSpecForms <~ ")" ^^ { case (b) => ArgExponent(b,None,None) }
  )

  lazy val parseArgCoefficient : Parser[ArgCoefficient] = fullParser(
    "(coefficient" ~> parseArgSpecForms <~ ")" ^^ { case (b) => ArgCoefficient(b,None,None) }
  )

  lazy val parseModCancellative : Parser[ModCancellative] = fullParser(
    "cancellative" ^^ { case _ => ModCancellative(None,None) }
  )

  lazy val parseArgRetrievalProtocol : Parser[ArgRetrievalProtocol] = fullParser(
    "(retrieval-protocol" ~> parseTName <~ ")" ^^ { case (b) => ArgRetrievalProtocol(b,None,None) }
  )

  lazy val parseArgApplicabilityRecognizer : Parser[ArgApplicabilityRecognizer] = fullParser(
    "(applicability-recognizer" ~> parseTName <~ ")" ^^ { case (b) => ArgApplicabilityRecognizer(b,None,None) }
  )

  lazy val parseArgFileSpec : Parser[ArgFileSpec] = fullParser(
    (";;"?) ~> "(" ~> parseTName ~ parseTName <~ ")" ^^ { case (p ~ q) => ArgFileSpec(p,q,None,None) }
  )

  lazy val parseArgFiles : Parser[ArgFiles] = fullParser(
    "(files" ~> rep1(parseArgFileSpec) <~ ")" ^^ { case (fss) => ArgFiles(fss,None,None) }
  )

  lazy val parseArgComponentSections : Parser[ArgComponentSections] = fullParser(
    "(component-sections" ~> rep1(parseTName) <~ ")" ^^ { case (ns) => ArgComponentSections(ns,None,None) }
  )

  lazy val parseArgBaseTheory : Parser[ArgBaseTheory] = fullParser(
    "(base-theory" ~> parseTName <~ ")" ^^ { case (b) => ArgBaseTheory(b,None,None) }
  )

  lazy val parseArgReplicaRenamer : Parser[ArgReplicaRenamer] = fullParser(
    "(replica-renamer" ~> parseTName <~ ")" ^^ { case (b) => ArgReplicaRenamer(b,None,None) }
  )

  lazy val parseArgNumbers : Parser[ArgNumbers] = fullParser(
    "(" ~> rep1(parseTNumber) <~ ")" ^^ { case (ns) => ArgNumbers(ns,None,None) }
  )

  lazy val parseArgTargetTheories : Parser[ArgTargetTheories] = fullParser(
    "(target-theories" ~> rep1(parseTName) <~ ")" ^^ { case (ns) => ArgTargetTheories(ns,None,None) }
  )

  lazy val parseArgTargetMultiple : Parser[ArgTargetMultiple] = fullParser(
    "(target-multiple" ~> parseTNumber <~ ")" ^^ { case (n) => ArgTargetMultiple(n,None,None) }
  )

  lazy val parseArgSortAssoc : Parser[ArgSortAssoc] = fullParser(
    "(" ~> parseTName ~ rep1(parseDefStringOrName) <~ ")" ^^ { case (n ~ ns) => ArgSortAssoc(n,ns,None,None) }
  )

  lazy val parseArgEnsembleSorts : Parser[ArgEnsembleSorts] = fullParser(
    "(sorts" ~> rep1(parseArgSortAssoc) <~ ")" ^^ { case (sas) => ArgEnsembleSorts(sas,None,None) }
  )

  lazy val parseArgConstAssoc : Parser[ArgConstAssoc] = fullParser(
    "(" ~> parseTName ~ rep1(parseDefStringOrName) <~ ")" ^^ { case (n ~ ns) => ArgConstAssoc(n,ns,None,None) }
  )

  lazy val parseArgEnsembleConsts : Parser[ArgEnsembleConsts] = fullParser(
    "(constants" ~> rep1(parseArgConstAssoc) <~ ")" ^^ { case (cas) => ArgEnsembleConsts(cas,None,None) }
  )

  lazy val parseArgMultiples : Parser[ArgMultiples] = fullParser(
    "(multiples" ~> rep1(parseTNumber) <~ ")" ^^ { case (ns) => ArgMultiples(ns,None,None) }
  )

  lazy val parseArgPermutations : Parser[ArgPermutations] = fullParser(
    "(permutations" ~> rep1("(" ~> rep1(parseTNumber) <~ ")") <~ ")" ^^ { case (ns) => ArgPermutations(ns,None,None) }
  )

  lazy val parseArgSpecialRenamings : Parser[ArgSpecialRenamings] = fullParser(
    "(special-renamings" ~> rep1(parseArgRenamerPair) <~ ")" ^^ { case (ns) => ArgSpecialRenamings(ns,None,None) }
  )

  lazy val parseArgNewTranslationName : Parser[ArgNewTranslationName] = fullParser(
    "(new-translation-name" ~> parseTName <~ ")" ^^ { case (n) => ArgNewTranslationName(n,None,None) }
  )

  lazy val parseAlgebraicSimplifierSpec : Parser[ArgAlgebraicSimplifierSpec] = fullParser(
    "(" ~> rep1(parseTName) <~ ")" ^^ { case (ns) => ArgAlgebraicSimplifierSpec(ns,None,None) }
  )

  lazy val parseAlgebraicSimplifier : Parser[ArgAlgebraicSimplifier] = fullParser(
    "(algebraic-simplifier" ~> rep1(parseAlgebraicSimplifierSpec) <~ ")" ^^ { case (specs) => ArgAlgebraicSimplifier(specs,None,None) }
  )

  lazy val parseAlgebraicOrderSimplifierSpec : Parser[ArgAlgebraicOrderSimplifierSpec] = fullParser(
    "(" ~> rep1(parseTName) <~ ")" ^^ { case (ns) => ArgAlgebraicOrderSimplifierSpec(ns,None,None) }
  )

  lazy val parseAlgebraicOrderSimplifier : Parser[ArgAlgebraicOrderSimplifier] = fullParser(
    "(algebraic-order-simplifier" ~> rep1(parseAlgebraicOrderSimplifierSpec) <~ ")" ^^ { case (specs) => ArgAlgebraicOrderSimplifier(specs,None,None) }
  )

  lazy val parseAlgebraicTermComparator : Parser[ArgAlgebraicTermComparator] = fullParser(
    "(algebraic-term-comparator" ~> rep1(parseTName) <~ ")" ^^ { case (specs) => ArgAlgebraicTermComparator(specs,None,None) }
  )

  lazy val parseArgAlgebraicProcessor : Parser[ArgAlgebraicProcessor] = fullParser(
    "(algebraic-processor" ~> parseTName <~ ")" ^^ { case (n) => ArgAlgebraicProcessor(n,None,None) }
  )

  lazy val parseArgDiscreteSorts : Parser[ArgDiscreteSorts] = fullParser(
    "(discrete-sorts" ~> rep1(parseSort) <~ ")" ^^ { case (srt) => ArgDiscreteSorts(srt,None,None) }
  )

  lazy val parseArgOperationsAlist : Parser[ArgOperationsAlist] = fullParser(
    "(" ~> parseTName ~ parseTName <~ ")" ^^ { case (o ~ p) => ArgOperationsAlist(o,p,None,None) }
  )

  lazy val parseArgProcOperations : Parser[ArgProcOperations] = fullParser(
    "(operations" ~> rep1(parseArgOperationsAlist) <~ ")" ^^ { case (ls) => ArgProcOperations(ls,None,None) }
  )

  lazy val parseModReload : Parser[ModReload] = fullParser(
    "reload" ^^ { case _ => ModReload(None,None) }
  )

  lazy val parseModQuickLoad : Parser[ModQuickLoad] = fullParser(
    "quick-load" ^^ { case _ => ModQuickLoad(None,None) }
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
    new DFAtomicSortC(js)
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
    new DFConstantC(js)
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
    List(parseArgEmbeddedLangs, parseArgEmbeddedLang, parseArgBaseTypes, parseArgSorts,
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

  val pTheorem : Parser[DFTheorem] =
    composeParser(
      "def-theorem",
      List(parseTName, parseDefStringOrName),
      List(parseModReverse, parseModLemma),
      List((parseArgTheory,R)) :::
        List(parseArgUsages, parseArgTranslation, parseArgMacete,parseArgHomeTheory,parseArgProof).map(p => (p,O)),
      new DFTheoremC(js)
    )

  val pTheory : Parser[DFTheory] = composeParser(
      "def-theory ", // space is necessary because otherwise this conflicts with theory-ensemble etc.
      List(parseTName),
      Nil,
      List(parseArgLanguage,parseArgComponentTheories,parseArgAxioms,parseArgDistinctConstants).map(p => (p,O)),
      new DFTheoryC(js)
  )

  lazy val pOverloading : Parser[DFOverloading] = fullParser(
    "(def-overloading" ~> parseTName ~ rep1(parseArgOverloadingPairs) <~ ")" ^^ { case t ~ ps =>
      DFOverloading.build(List(t,ps))
    }
  )

  lazy val pTranslation : Parser[DFTranslation] = composeParser(
      "def-translation",
      List(parseTName),
      List(parseModForceU,parseModForce,parseModDontEnrich),
      List(parseArgSource, parseArgTarget).map(p => (p,R)) :::
        List(parseArgAssumptions, parseArgFixedTheories, parseArgSortPairs, parseArgConstPairs,
          parseArgCoreTranslation, parseArgTheoryInterpretationCheck).map(p => (p,O)),
      new DFTranslationC(js)
    )

  lazy val pRecursiveConstant : Parser[DFRecursiveConstant] = {
    composeParser(
      "def-recursive-constant",
      List(parseArgNameList, parseArgDefStringList),
      Nil,
      List((parseArgTheory,R),(parseArgUsages,O),(parseArgDefinitionName,O)),
      new DFRecursiveConstantC(js)
    )
  }

  lazy val pTransportedSymbols : Parser[DFTransportedSymbols] = composeParser(
    "def-transported-symbols",
    List(parseArgNameList),
    Nil,
    List((parseArgTranslation,R),(parseArgRenamer,O)),
    DFTransportedSymbols
  )

  lazy val pAlgebraicProcessor : Parser[DFAlgebraicProcessor] = composeParser(
    "def-algebraic-processor",
    List(parseTName),
    List(parseModCancellative),
    List((parseArgLanguage,R),(parseArgBase,R),(parseArgExponent,O),(parseArgCoefficient,O)),
    DFAlgebraicProcessor
  )

  lazy val pScript : Parser[DFScript] = composeParser(
    "def-script",
    List(parseTName,parseTNumber,parseScript),
    Nil,
    List(parseArgRetrievalProtocol, parseArgApplicabilityRecognizer).map(p => (p,O)),
    DFScript
  )

  lazy val pSection : Parser[DFSection] = composeParser(
    "def-section",
    List(parseTName),
    Nil,
    List(parseArgComponentSections, parseArgFiles).map(p => (p,O)),
    DFSection
  )

  lazy val pTheoryEnsemble : Parser[DFTheoryEnsemble] = composeParser(
    "def-theory-ensemble ",
    List(parseTName),
    Nil,
    List(parseArgBaseTheory,parseArgFixedTheories,parseArgReplicaRenamer).map(p => (p,O)),
    DFTheoryEnsemble
  )

  lazy val pTheoryEnsembleMultiple : Parser[DFTheoryEnsembleMultiple] = composeParser(
    "def-theory-ensemble-multiple",
    List(parseTName,parseTNumber),
    Nil,
    Nil,
    DFTheoryEnsembleMultiple
  )

  lazy val pTheoryEnsembleOverloadings : Parser[DFTheoryEnsembleOverloadings] = composeParser(
    "def-theory-ensemble-overloadings",
    List(parseTName,parseArgNumbers),
    Nil,
    Nil,
    DFTheoryEnsembleOverloadings
  )

  lazy val pTheoryEnsembleInstances : Parser[DFTheoryEnsembleInstances] = composeParser(
    "def-theory-ensemble-instances",
    List(parseTName),
    List(parseModForceU),
    List(parseArgTargetTheories, parseArgTargetMultiple, parseArgEnsembleSorts, parseArgEnsembleConsts,
      parseArgMultiples, parseArgTheoryInterpretationCheck, parseArgPermutations,parseArgSpecialRenamings).map(p => (p,O)),
    DFTheoryEnsembleInstances
  )

  lazy val pTheoryInstance : Parser[DFTheoryInstance] = composeParser(
    "def-theory-instance",
    List(parseTName),
    Nil,
    List((parseArgSource,R),(parseArgTarget,R),(parseArgTranslation,R),(parseArgFixedTheories,O),
      (parseArgRenamer,O),(parseArgNewTranslationName,O)),
    DFTheoryInstance
  )

  lazy val pTheoryProcessors : Parser[DFTheoryProcessors] = composeParser(
    "def-theory-processors",
    List(parseTName),
    Nil,
    List(parseAlgebraicSimplifier,parseAlgebraicOrderSimplifier,parseAlgebraicTermComparator).map(p => (p,O)),
    DFTheoryProcessors
  )

  lazy val pOrderProcessors : Parser[DFOrderProcessor] = composeParser(
    "def-order-processor",
    List(parseTName),
    Nil,
    List(parseArgAlgebraicProcessor,parseArgProcOperations,parseArgDiscreteSorts).map(p => (p,O)),
    DFOrderProcessor
  )

  lazy val pIncludeFiles : Parser[DFIncludeFiles] = composeParser(
    "include-files",
    Nil,
    List(parseModReload,parseModQuickLoad),
    List((parseArgFiles,O)),
    DFIncludeFiles
  )

  lazy val pLoadSection : Parser[DFLoadSection] = composeParser(
    "load-section",
    List(parseTName),
    Nil,
    Nil,
    DFLoadSection
  )

  lazy val pComment : Parser[DFComment] = composeParser(
    "comment",
    List(parseScript),
    Nil,
    Nil,
    DFComment
  )

  lazy val pSet : Parser[Set] = fullParser(
    "(set" ~> parseScript <~ ")" ^^ { case (c) => Set(c,None,None) }
  )

  lazy val pDefine : Parser[Define] = fullParser(
    "(define" ~> parseScript <~ ")" ^^ { case (c) => Define(c,None,None) }
  )

  lazy val pTeXCorrespondence : Parser[TeXCorrespondence] = fullParser(
    "(make-tex-correspondence" ~> parseScript <~ ")" ^^ { case (s) => TeXCorrespondence(s,None,None) }
  )

  lazy val pQCConstantlike : Parser[QCConstantlike] = fullParser(
    "(make-quasi-constructor-constantlike" ~> parseTName <~ ")" ^^ { case (s) => QCConstantlike(s,None,None) }
  )

  lazy val pEnsembleDontTranslateConst : Parser[EnsembleDontTranslateConst] = fullParser(
    "(ensemble-dont-translate-constant" ~> parseScript <~ ")" ^^ { case (s) => EnsembleDontTranslateConst(s,None,None) }
  )

  def parseArbitrary(str : String) : Parser[ArbitraryScript] = fullParser(
    "(" ~> str.r ~ parseScript <~ ")" ^^ { case (n ~ s) => println(n + ";" + s) ; ArbitraryScript(n,s,None,None) }
  )

  lazy val pSimplog1stWrapper : Parser[Simplog1stWrapper] = fullParser(
    "(transportable-rewrite-usage-simplog1st" ~> pTheorem <~ ")" ^^ { case (d) => Simplog1stWrapper(d,None,None) }
  )

  // ######### Complete Parsers

  val allDefFormParsers : List[Parser[DefForm]] = List(
    parseLineComment,pComment,pHeralding, pAtomicSort, pConstant, pQuasiConstructor, pSchematicMacete, pCompoundMacete,
    pInductor, pImportedRewriteRules, pLanguage, pRenamer, pPrintSyntax, pParseSyntax, pTheorem, pTheory, pOverloading,
    pTranslation, pTransportedSymbols, pRecursiveConstant, pAlgebraicProcessor, pScript, pSection, pTheoryEnsemble,
    pTheoryEnsembleMultiple, pTheoryEnsembleOverloadings, pTheoryEnsembleInstances, pTheoryInstance, pTheoryProcessors,
    pOrderProcessors,pIncludeFiles,pLoadSection,pSet,pDefine,pTeXCorrespondence,pQCConstantlike, pEnsembleDontTranslateConst,
    pSimplog1stWrapper
  )

  lazy val parseImpsSource : PackratParser[List[DefForm]] = { rep1(anyOf(allDefFormParsers)) }

}
