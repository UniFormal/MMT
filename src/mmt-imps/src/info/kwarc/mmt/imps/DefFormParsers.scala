package info.kwarc.mmt.imps

import info.kwarc.mmt.imps.ParserWithSourcePosition._
import info.kwarc.mmt.imps.Usage.Usage

class DefFormParsers
{
  lazy val parseImpsSource : PackratParser[List[DefForm]] = { rep1(parseDefForm) }

  lazy val parseDefForm : PackratParser[DefForm] = {
    parseLineComment | pHeralding | pAtomicSort | pConstant | pQuasiConstructor | pSchematicMacete
  }

  // ######### Argument Parsers

  lazy val parseName  : Parser[String] = regex("""[^()\t\r\n ]+""".r)
  lazy val parseTName : Parser[Name]   = fullParser(parseName ^^ { case nm => Name(nm,None,None)})

  // ToDo: nested strings could be a problem. Do those occur?
  lazy val parseDefString : Parser[DefString] = fullParser(regex("""\"[^\"]+\"""".r) ^^ {case s => DefString(s,None,None)})

  lazy val parseArgTheory : Parser[ArgTheory] =
    fullParser(("(theory" ~> parseTName <~ ")") ^^ { case n => ArgTheory(n,None,None) })

  lazy val parseArgLanguage : Parser[ArgLanguage] =
    fullParser(("(language" ~> parseTName <~ ")") ^^ { case n => ArgLanguage(n,None,None) })

  lazy val parseArgWitness : Parser[ArgWitness] =
    fullParser(("(witness" ~> parseDefString <~ ")") ^^ {case s => ArgWitness(s,None,None)})

  lazy val parseUsage : Parser[Usage] = parseName ^^ {
    case "elementary-macete"        => Usage.ELEMENTARYMACETE
    case "transportable-macete"     => Usage.TRANSPORTABLEMACETE
    case "rewrite"                  => Usage.REWRITE
    case "transportable-rewrite"    => Usage.TRANSPORTABLEREWRITE
    case "simplify-logically-first" => Usage.SIMPLIFYLOGICALLYFIRST
    case "d-r-convergence"          => Usage.DRCONVERGENCE
    case "d-r-value"                => Usage.DRVALUE
    case _                          => ???
  }

  lazy val parseArgUsages  : Parser[ArgUsages] =
    fullParser(("(usages" ~> rep1(parseUsage) <~ ")") ^^ { case us => ArgUsages(us,None,None) })

  lazy val parseArgSort    : Parser[ArgSort] =
    fullParser(("(sort" ~> parseTName <~ ")") ^^ {case n => ArgSort(n,None,None)})

  lazy val parseArgFixedTheories  : Parser[ArgFixedTheories] =
    fullParser(("(fixed-theories" ~> rep1(parseTName) <~ ")") ^^ { case ts => ArgFixedTheories(ts,None,None) })

  lazy val parseModTransportable  : Parser[ModTransportable] =
    fullParser("transportable" ^^ {case _ => ModTransportable(None,None)} )

  lazy val parseModNull : Parser[ModNull] = fullParser("null" ^^ {case _ => ModNull(None,None)} )

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

  val pConstant : Parser[DFConstant] = composeParser(
    "def-constant",
    List(parseTName, parseDefString),
    Nil,
    List((parseArgTheory,R),(parseArgSort,O),(parseArgUsages,O)),
    DFConstant
  )

  val pQuasiConstructor : Parser[DFQuasiConstructor] = composeParser(
    "def-quasi-constructor",
    List(parseTName, parseDefString),
    Nil,
    List((parseArgLanguage,R),(parseArgFixedTheories,O)),
    DFQuasiConstructor
  )

  val pSchematicMacete : Parser[DFSchematicMacete] = composeParser(
    "def-schematic-macete",
    List(parseTName, parseDefString),
    List(parseModNull, parseModTransportable),
    List((parseArgTheory,R)),
    DFSchematicMacete
  )

}
