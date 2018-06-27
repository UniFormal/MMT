package info.kwarc.mmt.imps

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

class SymbolicExpressionParser extends RegexParsers
                                  with PackratParsers
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