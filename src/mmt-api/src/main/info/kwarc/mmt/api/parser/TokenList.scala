package info.kwarc.mmt.api.parser

import java.lang.Character._

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.Term

/** helper object */
object TokenList {
  /** @return true if c is considered to be a letter */
  def isLetter(c: Char): Boolean = c.isLetter ||
    List(COMBINING_SPACING_MARK, ENCLOSING_MARK, NON_SPACING_MARK).contains(c.getType)

  /** @return true if c is considered to be whitespace */
  def isWhitespace(c: Char): Boolean = c.isWhitespace

  /** @return true if c is considered to be a number */
  // OTHER_NUMBER is not treated as number in order to allow using e.g., superscripted numbers as operators without whitespace
  def isNumber(c: Char): Boolean =
    List(DECIMAL_DIGIT_NUMBER, LETTER_NUMBER) contains c.getType

  def isLetterOrNumber(c: Char) = isLetter(c) || isNumber(c)
    
  /** @return true if c is considered a connector, i.e., a character that never breaks a word */
  def isConnector(c: Char): Boolean = c.getType == CONNECTOR_PUNCTUATION

  /** @return true if there is no word break between before and after */
  def canFollow(before: Char, after: Char): Boolean =
    !isWhitespace(after) &&
      (isConnector(before) || isConnector(after) || (isLetterOrNumber(before) == isLetterOrNumber(after)))

  /** the Tokenizer
    *
    * tokens are sequences of letters or individual other Unicode characters
    *
    * whitespace separates Tokens and is never part of a Token
    *
    * connectors connect the preceding and the succeeding Tokens into a single Token;
    * the connector is part of the Token
    *
    * The SourcePositions in the Tokens are only correct if all line endings contain a '\n'.
    * (The '\n' counts when counting the offset.)
    *
    * @param s string to tokenize
    * @param em the escape manager containing lexing rules
    * @param first the position of the first character (defaults to 0)
    * @return the resulting TokenList
    */
  def apply(s: String, em: EscapeManager, first: SourcePosition = SourcePosition(0, 0, 0)): TokenList = {
    val l = first.offset + s.length
    // lexing state
    var i = first // position of next Char in s
    var current = "" // previously read prefix of the current Token
    var connect = false // current.last.getType == CONNECTOR_PUNCTUATION
    var skipEscaped = 0 //number of characters to skip, normally 0
    var whitespace = true //there was a whitespace before the current Token
    var tokens: List[TokenListElem] = Nil // Token's found so far in reverse order
    //end the current Token
    def endToken() {
      if (current != "") {
        tokens ::= Token(current, i - current.length, whitespace)
        current = ""
        whitespace = false
      }
    }
    // returns the character after the current one; caller must check for existence first
    def nextChar: Char = s(i.offset - first.offset + 1)
    // the lexing loop
    while (i.offset < l) {
      val c = s(i.offset - first.offset) // current Char
      val tp = c.getType // current Char's type
      if (skipEscaped > 0) {
        skipEscaped -= 1
      } else em(s, i.offset - first.offset, i) match {
        case Some(escaped) =>
          endToken()
          tokens ::= escaped
          skipEscaped = escaped.length - 1
        case None => if (isWhitespace(c)) {
          // whitespace always starts a new Token,
          endToken()
          whitespace = true
        } else {
          // we are in a Token
          tp match {
            // after a connector, everything continues the Token
            case _ if connect =>
              current += c
              connect = false
            // letters, marks, and numbers continue the Token
            case _ if isLetter(c) || isNumber(c) =>
              current += c
            // the special MMT delimiters continue a multi-character Token if other characters follow
            // TODO does not allow lexing URIs ?name
            case _ if "?/".contains(c) && current != "" && i.offset < l - 1 && !isWhitespace(nextChar) =>
              current += c
            // connectors are remembered
            case _ if isConnector(c) =>
              current += c
              connect = true
            case _ if '\uD800' < c && c < '\uD8FF' =>
              current += c
              connect = true
            // everything else:
            case _ =>
              // end previous Token, if any
              endToken()
              // look ahead: if a connector follows, start a multi-character Token
              // otherwise, create a single-character Token
              if (i.offset < l - 1 && isConnector(nextChar)) {
                current += c
              } else {
                tokens ::= Token(c.toString, i, whitespace)
              }
          }
          whitespace = false
        }
      }
      if (c == '\n') i = i.nl else i += 1
    }
    //add the last Token, if any
    endToken()
    new TokenList(tokens.reverse)
  }
}

/** A TokenList is a wrapper for a mutable list of TokenListElem with a few special methods for parsing
  *
  * TokenListElem's are always indexed starting from 0.
  *
  * @param tokens the underlying list
  *
  */
class TokenList(private var tokens: List[TokenListElem]) {
  /** supposed to be guaranteed by invariants, but checked redundantly to detect subtle otherwise hard-to-find implementation errors */
  private def checkIndex(n: Int, msg: String = "") {
    if (n >= tokens.length || n < 0)
      throw ImplementationError("token index " + n + " out of range in token list of length " + tokens.length + " (message: " + msg + "): " + tokens.toString)
  }
  /** returns a Token in a given position */
  def apply(n: Int): TokenListElem = {
    checkIndex(n)
    tokens(n)
  }

  /** returns a sublist of elements */
  def apply(from: Int, to: Int): List[TokenListElem] = tokens.slice(from, to)

  /** returns all tokens */
  def getTokens: List[TokenListElem] = tokens

  /**
   * applies a notation and transforms this token list accordingly (this is the only place where [[MatchedList]]s are created)
   * @param an the notation to reduce
   * @return the slice reduced into 1 Token
   */
  def reduce(an: ActiveNotation, rt: ParsingRuleTable, rep: frontend.Report): (Int, Int) = {
    val found = an.getFound
    def doFoundArg(fa: FoundArg): UnmatchedList = {
      fa match {
        case fa: FoundArg =>
          // fa.slice might be a single token, but that is harmless
          val ul = new UnmatchedList(new TokenList(fa.slice.toList))
          ul.scanner = new Scanner(ul.tl, None, rt, rep)
          ul
      }
    }
    var newTokens: List[(FoundContent, List[UnmatchedList])] = Nil
    found foreach {
      case _: FoundDelim =>
      case fa: FoundArg =>
        newTokens ::= (fa, List(doFoundArg(fa)))
      case fsa : FoundSeqArg =>
        newTokens ::= (fsa, fsa.args map doFoundArg)
      case fv: FoundVar =>
        val toks = fv.getVars flatMap {
          case SingleFoundVar(_, _, tpOpt) =>
            tpOpt match {
              case Some(fa) => List(doFoundArg(fa))
              case None => Nil
            }
        }
        newTokens ::= (fv, toks)
    }
    val (from, to) = an.fromTo
    checkIndex(from, "active notation is " + an.toString)
    checkIndex(to-1)
    val matched = new MatchedList(newTokens.reverse, an, tokens(from).firstPosition, tokens(to - 1).lastPosition)
    tokens = tokens.take(from) ::: matched :: tokens.drop(to)
    (from, to)
  }

  /** the length of the list */
  def length: Int = tokens.length

  override def toString: String = tokens.mkString("", " ", "")
}

/** A reference to a sublist of a TokenList that does not copy the element
  * @param tokens the underlying TokenList
  * @param start the first Token (inclusive)
  * @param next the last Token (exclusive)
  */
case class TokenSlice(tokens: TokenList, start: Int, next: Int) {
  override def toString: String = tokens(start, next).mkString("", " ", "")

  /** derefences the TokenSlice and returns the list */
  def toList: List[TokenListElem] = tokens(start, next)

  /** the length of the slice */
  def length: Int = next - start
}

/** the type of objects that may occur in a [[info.kwarc.mmt.api.parser.TokenList]] */
trait TokenListElem {
  /** the first position included in this token */
  def firstPosition: SourcePosition

  /** the last position included in this token */
  def lastPosition: SourcePosition

  /** the region covered by this token */
  def region: SourceRegion = SourceRegion(firstPosition, lastPosition)
}

/** subtype of TokenListElem that defines some methods generally */
abstract class PrimitiveTokenListElem(text: String) extends TokenListElem {
  override def toString: String = text

  //+ "@" + first.toString
  val length = text.length
  val lastPosition = {
    SourcePosition(firstPosition.offset + length - 1, firstPosition.line, firstPosition.column + length - 1)
  }
}

/** A Token is the basic TokenListElem
  * @param word the characters making up the Token (excluding whitespace)
  * @param firstPosition the SourcePosition of the first character
  * @param whitespaceBefore true iff the Token was preceded by whitespace (true for the first Token, too)
  * @param text the text lexed into this token, if different from word
  */
case class Token(word: String, firstPosition: SourcePosition, whitespaceBefore: Boolean, text: Option[String] = None)
  extends PrimitiveTokenListElem(text.getOrElse(word))

/** An ExternalToken can be anything produced by a [[LexerExtension]]
  * @param text the characters making up the token
  */
abstract class ExternalToken(text: String) extends PrimitiveTokenListElem(text) {
  /** a continuation function called by the parser when parsing this Token
    *
    * @param outer the ParsingUnit during which this ExternalToken was encountered
    * @param BoundName the context
    * @param parser the parser calling this function
    */
  def parse(outer: ParsingUnit, boundVars: List[BoundName], parser: ObjectParser): Term
}

/** A convenience class for an ExternalToken whose parsing is context-free so that it can be parsed immediately
  * @param term the result of parsing
  */
case class CFExternalToken(text: String, firstPosition: SourcePosition, term: Term) extends ExternalToken(text) {
  /** returns simply term */
  def parse(outer: ParsingUnit, boundVars: List[BoundName], parser: ObjectParser): Term = term
}

/** A MatchedList is a SubTermToken resulting by reducing a sublist using a notation
  *
  * invariant: every FoundArg (including nested ones) found when traversing an.getFound
  * corresponds to one TokenListElem in tokens
  *
  * TokenSlice's in an.getFound are invalid.
  *
  * @param tokens the TokenListElem's that were reduced (in concrete syntax order, excluding delimiters)
  * @param an the notation used for reduction (which stores the information about how the notation matched the tokens)
  */
class MatchedList(val tokens: List[(FoundContent,List[UnmatchedList])], val an: ActiveNotation,
                  val firstPosition: SourcePosition, val lastPosition: SourcePosition) extends TokenListElem {
  override def toString: String = if (tokens.isEmpty)
    "{" + an.toShortString + "}"
  else
    tokens.map(_._2.toString).mkString("{" + an.toShortString + " ", " ", " " + an.toShortString + "}")

  private[parser] def addRules(rules : ParsingRuleTable) : Unit = tokens foreach {
    case (_,ul) => ul.foreach(_.addRules(rules))
  }
}

/**
 * An UnmatchedList is a SubTermToken resulting by reducing a sublist without using a notation
 *
 * Other notations have determined that this sublist must be parsed into a subtree, but it is
 * not known (yet) which notation should be used.
 *
 * @param tl the TokenList that is to be reduced
 * @param localNotations notations that should additionally be used for this subterm
 */
class UnmatchedList(val tl: TokenList) extends TokenListElem {
  val firstPosition = tl(0).firstPosition
  val lastPosition = tl(tl.length - 1).lastPosition
  private[parser] var scanner: Scanner = null
  private[parser] var localNotations: Option[ParsingRuleTable] = None
  def addRules(rules : ParsingRuleTable) : Unit = {
    scanner.addRules(rules)
    tl.getTokens.foreach {
      case ul : UnmatchedList => ul.addRules(rules)
      case ml : MatchedList => ml.addRules(rules)
      case _ =>
    }
  }

  override def toString: String = if (tl.length == 1) tl(0).toString else "{unmatched " + tl.toString + " unmatched}"
}
