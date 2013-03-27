package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import objects.Term
import utils.MyList._


/** helper object */
object TokenList {
   import java.lang.Character._
   /** the Tokenizer
    * @param the string to tokenize
    * @param first the position of the first character (defaults to 0)
    * @return the resulting TokenList
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
    */
   def apply(s: String, first: SourcePosition = SourcePosition(0,0,0), em: EscapeManager) : TokenList = {
      val l = first.offset + s.length
      // lexing state
      var i = first  // position of net Char in s
      var current = "" // previously read prefix of the current Token
      var connect = false // current.last.getType == CONNECTOR_PUNCTUATION
      var skipEscaped = 0 //number of characters to skip, normally 0 
      var whitespace = true //there was a whitespace before the current Token
      var tokens : List[TokenListElem] = Nil // Token's found so far in reverse order
      //end the current Token
      def endToken {
         if (current != "") {
            tokens ::= Token(current, i-current.length, whitespace)
            current = ""
            whitespace = false
         }
      }
      // the lexing loop
      while (i.offset < l) {
         val c = s(i.offset-first.offset) // current Char
         val tp = c.getType // current Char's type
         if (skipEscaped > 0) {
            skipEscaped -= 1
         } else em(s,i.offset-first.offset, i) match {
           case Some(escaped) =>
               endToken
               tokens ::= escaped
               skipEscaped = escaped.length-1
           case None => if (c.isWhitespace) {
            // whitespace always starts a new Token, 
            endToken
            whitespace = true
         } else {
            // we are in a Token
            tp match {
               // after a connector, everything continues the Token
               case _ if connect =>
                  current += c
                  connect = false
               // letters, marks, and numbers continue the Token
               case _ if c.isLetter =>
                  current += c
               // the special MMT delimiters continue a multi-character Token
               case _ if (c == '?' || c == '/') && current != "" =>
                  current += c
               case COMBINING_SPACING_MARK | ENCLOSING_MARK | NON_SPACING_MARK =>
                  current += c
               case DECIMAL_DIGIT_NUMBER | LETTER_NUMBER | OTHER_NUMBER =>
                  current += c               
               // connectors are remembered
               case CONNECTOR_PUNCTUATION =>
                  current += c
                  connect = true
               // everything else:
               case _ =>
                  // end previous Token, if any
                  endToken
                  // look ahead: if a connector follows, start a multi-character Token
                  // otherwise, create a single-character Token
                  if (i.offset < l-1 && s(i.offset-first.offset+1).getType == CONNECTOR_PUNCTUATION) {
                     current += c
                  } else {
                     tokens ::= Token(c.toString, i, whitespace)
                  }
            }
            whitespace = false
         }}
         if (c == '\n') i = i.nl else i += 1
      }
      //add the last Token, if any
      endToken
      new TokenList(tokens.reverse)
   }
}

/** A TokenList is a wrapper for a mutable list of TokenListElem with a few special methods for parsing
 * 
 * @param tokens the underlying list
 * 
 * TokenListElem's are always indexed starting from 0.
 */
class TokenList(private var tokens: List[TokenListElem]) {
   /** returns a Token in a given position */
   def apply(n: Int) = tokens(n)
   /** returns a sublist of elements */
   def apply(from: Int, to: Int) = tokens.slice(from, to)
   /** returns all tokens */
   def getTokens = tokens
   /**
    * @param an the notation to reduce
    * @return the slice reduced into 1 Token 
    */
   def reduce(an: ActiveNotation): (Int,Int) = {
      val found = an.getFound
      var newTokens : List[TokenListElem] = Nil
      def doFoundArg(fa: FoundArg) {
         fa match {case FoundArg(sl, n) =>
            if (sl.length == 1 && tokens(sl.start).isInstanceOf[MatchedList])
               newTokens ::= tokens(sl.start)
            else
               newTokens ::= new UnmatchedList(new TokenList(sl.toList))
         }
      }
      found foreach {
         case _:FoundDelim =>
         case fa: FoundArg =>
            doFoundArg(fa)
         case FoundSeqArg(_, args) =>
            args foreach doFoundArg
         case fv : FoundVar =>
            fv.getVars foreach {
               case SingleFoundVar(_,_,tpOpt) =>
                  tpOpt foreach doFoundArg
            }
      }
      val (from,to) = an.fromTo
      val matched = new MatchedList(newTokens.reverse, an, tokens(from).firstPosition, tokens(to-1).lastPosition)
      tokens = tokens.take(from) ::: matched :: tokens.drop(to)
      (from,to)
   }
   /** the length of the list */
   def length = tokens.length
   override def toString = tokens.mkString("", " ", "") 
}

/** A reference to a sublist of a TokenList that does not copy the element
 * @param tokens the underlying TokenList
 * @param start the first Token (inclusive)
 * @param next the last Token (exclusive)
 */
case class TokenSlice(tokens: TokenList, start: Int, next: Int) {
   override def toString = tokens(start, next).mkString(""," ","")
   /** derefences the TokenSlice and returns the list */
   def toList = tokens(start,next)
   /** the length of the slice */
   def length = next - start
}

/** the type of objects that may occur in a [[info.kwarc.mmt.api.parser.TokenList]] */
trait TokenListElem {
   def firstPosition: SourcePosition
   def lastPosition: SourcePosition
   def region = SourceRegion(firstPosition,lastPosition)
}

/** subtype of TokenListElem that defines some methods generally */
abstract class PrimitiveTokenListElem(text: String) extends TokenListElem {
   override def toString = text //+ "@" + first.toString
   val length = text.length
   /** the region spanned by this Token, from first to last character */
   val lastPosition = {
      SourcePosition(firstPosition.offset+length, firstPosition.line, firstPosition.column+length-1)
   }
}

/** A Token is the basic TokenListElem
 * @param word the characters making up the Token (excluding whitespace)
 * @param first the index of the first character
 * @param whitespaceBefore true iff the Token was preceded by whitespace (true for the first Token, too)
 * @param firstPosition the SourcePosition of the first character
*  */
case class Token(word: String, firstPosition: SourcePosition, whitespaceBefore: Boolean) extends PrimitiveTokenListElem(word)

/** An ExternalToken is anything produced by an EscapeHandler
 * @param text the characters making up the literal
 * @param firstPosition the SourcePosition of the first character
 */
abstract class ExternalToken(text:String) extends PrimitiveTokenListElem(text) {
   /** a continuation function called by the parser when parsing this Token
    * 
    * @param outer the ParsingUnit during which this ExternalToken was encountered
    * @param boundVars the context
    * @param parser the parser calling this function
    */
   def parse(outer: ParsingUnit, boundVars: List[LocalName], parser: AbstractObjectParser): Term
}

/** A convenience class for an ExternalToken whose parsing is context-free so that it can be parsed immediately
 * @param term the result of parsing
 */
case class CFExternalToken(text:String, firstPosition: SourcePosition, term: Term) extends ExternalToken(text) {
   /** returns simply term */
   def parse(outer: ParsingUnit, boundVars: List[LocalName], parser: AbstractObjectParser) = term
}

/**
 * A MatchedList is a TokenListElem resulting by reducing a sublist using a notation
 * @param tokens the TokenListElem's that were reduced, excluding delimiters
 * @param an the notation used for reduction (which stores the information about how the notation matched the tokens)
 *
 * invariant: every FoundArg (including nested ones) found when traversing an.getFound
 * corresponds to one TokenListElem in tokens
 * 
 * TokenSlice's in an.getFound are invalid. 
 */
class MatchedList(var tokens: List[TokenListElem], val an: ActiveNotation,
                  val firstPosition: SourcePosition, val lastPosition: SourcePosition) extends TokenListElem {
   override def toString = if (tokens.isEmpty)
     "{" + an.notation.name.last + "}"
   else
     tokens.map(_.toString).mkString("{" + an.notation.name.last + " ", " ", " " + an.notation.name.last + "}")
   /** removes the redundant UnmatchedList wrapper around a single MatchedList in this list */
   def flatten {
      tokens = tokens map {
         case ul: UnmatchedList if ul.tl.length == 1 && ul.tl(0).isInstanceOf[MatchedList] =>
            ul.scanner.tl(0)
         case t => t
      }
   }
}

/**
 * An UnmatchedList is a TokenListElem resulting by reducing a sublist without using a notation
 * 
 * Other notations have determined that this sublist must be parsed into a subtree, but it is
 * not known (yet) which notation should be used.
 * 
 * @param tokens the TokenList that is to be reduced
 */
class UnmatchedList(val tl: TokenList) extends TokenListElem {
   val firstPosition = tl(0).firstPosition
   val lastPosition = tl(tl.length-1).lastPosition
   var scanner: Scanner = null
   override def toString = "{unmatched " + tl.toString + " unmatched}"
}