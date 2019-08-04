package info.kwarc.mmt.gf

/**
  * Code for parsing GF (Grammatical Framework) abstract syntax and extracting the relevant data.
  * This code likely only covers a subset of GF abstract syntax.
  */

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
case class OTHER_OPERATOR(str : String) extends GfToken   // operators we don't care about

// keywords
case object ABSTRACT extends GfToken
case object OF extends GfToken
case object CAT extends GfToken
case object FUN extends GfToken
case class OTHER_SEGMENT(str : String) extends GfToken   // segments we don't care about (like flags)

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
  def other_operator : Parser[OTHER_OPERATOR] = "=" ^^ (str => OTHER_OPERATOR(str))

  // keywords
  def abstract_ : Parser[ABSTRACT.type] = "abstract" ^^ (_ => ABSTRACT)
  def of_ : Parser[OF.type] = "of" ^^ (_ => OF)
  def cat_ : Parser[CAT.type] = "cat" ^^ (_ => CAT)
  def fun_ : Parser[FUN.type] = "fun" ^^ (_ => FUN)
  def other_segment : Parser[OTHER_SEGMENT] = "flags" ^^ {str => OTHER_SEGMENT(str)}

  def tokens: Parser[List[GfToken]] = {
    phrase(rep1(abstract_ | of_ | cat_ | fun_ | other_segment |
                colon | arrow | semicolon | comma | double_asterisk | other_operator |
                identifier))
  }

  def apply(str : String) : List[GfToken] = {
    parse(tokens, str) match {
      case Success(result, next) => result
      case NoSuccess(message, next) => throw GfLexerException(message)
    }
  }
}

class GfAbstractSyntax {
  /**
    * Contains all the data read from an abstract syntax
    */
}

class GfParserContext {
  val gfAbstractSyntax = new GfAbstractSyntax
}

class GfParser {
  /**
    * GF abstract syntaxes are rather linear in their structure.
    * Also, there are segments that we can simply skip.
    * Therefore, this parser is just a bunch of handwritten rules.
    * Maybe this should be improved in the future...
    */
  def parse(str : String) : GfAbstractSyntax = {
    val context = new GfParserContext
    val lexer = new GfLexer
    val tokens = lexer(str)
    if (tokens.isEmpty) throw GfEmptySyntaxException()

    // ... - TODO: do the actual work

    context.gfAbstractSyntax
  }
}
