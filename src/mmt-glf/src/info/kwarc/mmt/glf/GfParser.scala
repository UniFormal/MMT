package info.kwarc.mmt.glf

/**
  * Code for parsing GF (Grammatical Framework) abstract syntax and extracting the relevant data.
  * This code likely only covers a subset of GF abstract syntax.
  */

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

// follow http://enear.github.io/2016/03/31/parser-combinators/

sealed trait GfToken

case class IDENTIFIER(str : String) extends GfToken

// operators
case object EQUAL extends GfToken
case object COLON extends GfToken
case object ARROW extends GfToken
case object SEMICOLON extends GfToken
case object COMMA extends GfToken
case object DOUBLE_ASTERISK extends GfToken
case object LEFT_BRACE extends GfToken
case object RIGHT_BRACE extends GfToken
// case class OTHER_OPERATOR(str : String) extends GfToken   // operators we don't care about

// keywords
case object ABSTRACT extends GfToken
case object OF extends GfToken
case object CAT extends GfToken
case object FUN extends GfToken
case class OTHER_SEGMENT(str : String) extends GfToken   // segments we don't care about (like flags)

// TODO: Comments!
class GfLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace: Regex = "([ \t\r\f\n]|--.*|\\{-(.|\n)*-\\})+".r

  def identifier: Parser[IDENTIFIER] =    // TODO: Can identifiers contain unicode?
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }

  // operators
  def equal : Parser[EQUAL.type] = "=" ^^ (_ => EQUAL)
  def colon : Parser[COLON.type] = ":" ^^ (_ => COLON)
  def arrow : Parser[ARROW.type] = "->" ^^ (_ => ARROW)
  def semicolon : Parser[SEMICOLON.type] = ";" ^^ (_ => SEMICOLON)
  def comma : Parser[COMMA.type] = "," ^^ (_ => COMMA)
  def double_asterisk : Parser[DOUBLE_ASTERISK.type] = "**" ^^ (_ => DOUBLE_ASTERISK)
  def l_brace : Parser[LEFT_BRACE.type] = "{" ^^ (_ => LEFT_BRACE)
  def r_brace : Parser[RIGHT_BRACE.type] = "}" ^^ (_ => RIGHT_BRACE)
  // def other_operator : Parser[OTHER_OPERATOR] = "=" ^^ (str => OTHER_OPERATOR(str))

  // keywords
  def abstract_ : Parser[ABSTRACT.type] = "abstract[ \t\r\f\n]".r ^^ (_ => ABSTRACT)
  def of_ : Parser[OF.type] = "of[ \t\r\f\n]".r ^^ (_ => OF)
  def cat_ : Parser[CAT.type] = "cat[ \t\r\f\n]".r ^^ (_ => CAT)
  def fun_ : Parser[FUN.type] = "(fun|data)[ \t\r\f\n]".r ^^ (_ => FUN)
  def other_segment : Parser[OTHER_SEGMENT] = "flags[ \t\r\f\n]".r ^^ {str => OTHER_SEGMENT(str)}

  def tokens: Parser[List[GfToken]] = {
    phrase(rep1(abstract_ | of_ | cat_ | fun_ | other_segment |
                equal | colon | arrow | semicolon | comma | double_asterisk /* | other_operator */ |
                l_brace | r_brace |
                identifier))
  }

  def apply(str : String) : List[GfToken] = {
    parse(tokens, str) match {
      case Success(result, _) => result
      case NoSuccess(message, _) => throw GfLexerException(message)
    }
  }
}

class GfAbstractSyntax {
  /**
    * Contains all the data read from an abstract syntax
    */
  var syntaxName : Option[String] = None
  var includes : Seq[String] = Seq()
  val types : ArrayBuffer[String] = ArrayBuffer()
  val functions : ArrayBuffer[(String, Seq[String])] = ArrayBuffer()
}

class GfParserContext(tokens : List[GfToken]) {
  val gfAbstractSyntax = new GfAbstractSyntax
  var pos = 0

  def popToken() : GfToken = {
    if (reachedEOF()) throw GfUnexpectedEOF("Reached end of file unexpectedly")
    val token = tokens(pos)
    pos = pos + 1
    token
  }

  def unpopToken(): Unit = {
    assert(pos > 0)
    pos -= 1
  }

  def peekToken() : GfToken = {
    val token = popToken()
    unpopToken()
    token
  }

  def nextTokenIsIdentifier() : Boolean = {
    peekToken() match {
      case IDENTIFIER(_) => true
      case _ => false
    }
  }

  def reachedEOF() : Boolean = pos >= tokens.size
}

class GfParser {
  /**
    * GF abstract syntaxes are rather linear in their structure.
    * Also, there are segments that we can simply skip.
    * Therefore, this parser is just a bunch of handwritten rules.
    * Maybe this should be improved in the future...
    */
  def parse(str : String) : GfAbstractSyntax = {
    val lexer = new GfLexer
    val tokens = lexer(str)
    if (tokens.isEmpty) throw GfEmptySyntaxException()
    val ctx = new GfParserContext(tokens)

    parseHeader(ctx)
    parseBody(ctx)

    if (!ctx.reachedEOF()) {
      throw GfUnexpectedTokenException("Finished parsing before reaching EOF")
    }

    ctx.gfAbstractSyntax
  }

  def parseHeader(ctx : GfParserContext): Unit = {
    // parses the "first line", like e.g. "abstract Combined = Lexicon, Grammar **"
    expectToken(ABSTRACT, ctx)
    ctx.gfAbstractSyntax.syntaxName = Some(expectIdentifier(ctx))
    expectToken(EQUAL, ctx)

    // if the next token is an identifier, we should expect a list of includes
    ctx.peekToken() match {
      case IDENTIFIER(_) =>
        ctx.gfAbstractSyntax.includes = parseIdList(COMMA, ctx)
        expectToken(DOUBLE_ASTERISK, ctx)
      case DOUBLE_ASTERISK => ctx.popToken() // remove it
      case _ => // continue normally
    }
  }

  def parseBody(ctx : GfParserContext): Unit = {
    expectToken(LEFT_BRACE, ctx)
    var ignoremode = false
    var possiblydone = false  // if we are in ignoremode and we've found a '}', we could be done
    while (true) {
      if (possiblydone) {
        if (ctx.reachedEOF()) return
        possiblydone = false
      }
      ctx.popToken() match {
        case CAT =>
          parseCat(ctx)
          ignoremode = false
        case FUN =>
          parseFun(ctx)
          ignoremode= false
        case OTHER_SEGMENT(_) =>
          ignoremode = true
        case RIGHT_BRACE =>
          if (! ignoremode) return
          possiblydone = true
        case token =>
          if (!ignoremode) throw GfUnexpectedTokenException("Expected segment type (like cat or fun), found: '" + token.toString + "'")
      }
    }
    throw GfUnexpectedTokenException("Unexpectedly reached EOF")
  }

  def parseCat(ctx : GfParserContext): Unit = {
    // parses segment of categories
    while (ctx.nextTokenIsIdentifier()) {
      ctx.gfAbstractSyntax.types += expectIdentifier(ctx)
      expectToken(SEMICOLON, ctx)
    }
  }

  def parseFun(ctx : GfParserContext): Unit = {
    // parses segment of function constants
    while (true) {
      ctx.peekToken() match {
        case IDENTIFIER(_) =>
          val names = parseIdList(COMMA, ctx)
          expectToken(COLON, ctx)
          val type_ = parseIdList(ARROW, ctx)
          expectToken(SEMICOLON, ctx)
          for (name <- names) {
            // todo: improve this
            ctx.gfAbstractSyntax.functions ++= List((name, type_))
          }
        case _ => return
      }
    }
  }

  def parseIdList(separator : GfToken, ctx : GfParserContext) : Seq[String] = {
    /**
      * parses a list of identifiers separated by the separator (could e.g. be a COMMA)
      * Expects at least one identifier
      */
    val identifiers : ArrayBuffer[String] = new ArrayBuffer[String]()
    do {
      identifiers += expectIdentifier(ctx)
    } while (ctx.popToken() == separator)
    ctx.unpopToken()
    identifiers.toSeq
  }

  def expectIdentifier(ctx : GfParserContext) : String = {
    val token = ctx.popToken()
    token match {
      case IDENTIFIER(str) => str
      case other => throw GfUnexpectedTokenException("Expected identifier - found '" + other.toString + "'")
    }
  }

  def expectToken(expectedToken : GfToken, ctx : GfParserContext): Unit = {
    val token = ctx.popToken()
    if (token != expectedToken) {
      throw GfUnexpectedTokenException("Unexpected token '" + token.toString + "' - expected '" + expectedToken.toString + "'")
    }
  }
}
