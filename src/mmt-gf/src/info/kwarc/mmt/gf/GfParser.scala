package info.kwarc.mmt.gf

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

// follow http://enear.github.io/2016/03/31/parser-combinators/

sealed trait GfToken

case class IDENTIFIER(str : String) extends GfToken

// operators
case object COLON extends GfToken
case object ARROW extends GfToken
case object SEMICOLON extends GfToken
case object COMMA extends GfToken
case object DOUBLE_ASTERISK extends GfToken

// keywords
case object ABSTRACT extends GfToken
case object OF extends GfToken
case object CAT extends GfToken
case object FUN extends GfToken

class GfLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace: Regex = "[ \t\r\f\n]+".r

  def identifier: Parser[IDENTIFIER] =    // TODO: Can identifiers contain unicode?
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }

  // operators
  def colon : Parser[COLON.type] = ":" ^^ (_ => COLON)
  def arrow : Parser[ARROW.type] = "->" ^^ (_ => ARROW)
  def semicolon : Parser[SEMICOLON.type] = ";" ^^ (_ => SEMICOLON)
  def comma : Parser[COMMA.type] = ";" ^^ (_ => COMMA)
  def double_asterisk : Parser[DOUBLE_ASTERISK.type] = "**" ^^ (_ => DOUBLE_ASTERISK)

  // keywords
  def abstract_ : Parser[ABSTRACT.type] = "abstract" ^^ (_ => ABSTRACT)
  def of_ : Parser[OF.type] = "of" ^^ (_ => OF)
  def cat_ : Parser[CAT.type] = "cat" ^^ (_ => CAT)
  def fun_ : Parser[FUN.type] = "fun" ^^ (_ => FUN)
}

