package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import utils.MyList._


/** helper object */
object TokenList {
   import java.lang.Character._
   /** the Tokenizer
    * @param the string to tokenize
    * @return the resulting TokenList
    * 
    * tokens are sequences of letters or individual other Unicode characters
    * 
    * whitespace separates Tokens and is never part of a Token
    * 
    * connectors connect the preceding and the succeeding Tokens into a single Token;
    * the connector is part of the Token
    */
   def apply(s: String) : TokenList = {
      val l = s.length
      // lexing state
      var i = 0        // position of net Char in s
      var current = "" // previously read prefix of the current Token
      var connect = false // current.last.getType == CONNECTOR_PUNCTUATION
      var whitespace = true //there was a whitespace before the current Token
      var tokens : List[Token] = Nil // Token's found so far in reverse order
      // the lexing loop
      while (i < l) {
         val c = s(i) // current Char
         val tp = c.getType // current Char's type
         // whitespace always starts a new Token, 
         if (c.isWhitespace) {
            if (current != "") {
               tokens ::= Token(current, i-current.length, whitespace)
               current = ""
            } 
            whitespace = true
         } else {
            // we are in a multi-character Token
            tp match {
               // after a connector, everything continues the Token
               case _ if connect =>
                  current += c
                  connect = false
               // letters, marks, and numbers continue the Token
               case _ if c.isLetter =>
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
                  if (current != "") {
                     tokens ::= Token(current, i-current.length, whitespace)
                     current = ""
                     whitespace = false
                  }
                  // look ahead: if a connector follows, start a multi-character Token
                  // otherwise, create a single-character Token
                  if (i < l-1 && s(i+1).getType == CONNECTOR_PUNCTUATION) {
                     current += c
                  } else {
                     tokens ::= Token(c.toString, i, whitespace)
                  }
            }
            whitespace = false
         }
         i += 1
      }
      //add the last Token, if any
      if (current != "")
         tokens ::= Token(current, i-current.length, whitespace)
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
         case FoundVar(_, _, _, tpOpt) =>
            tpOpt foreach doFoundArg
      }
      val (from,to) = an.fromTo
      val matched = new MatchedList(newTokens.reverse, an, tokens(from).first, tokens(to-1).last)
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
   /** the index of this TokenListElem's first character in the input */
   def first : Int
   /** the index of this TokenListElem's last character in the input */
   def last : Int
}

/** A Token is the basic TokenListElem
 * @param word the characters making up the Token (excluding whitespace)
 * @param first the index of the first character (starting from 0)
 * @param whitespaceBefore true iff the Token was preceded by whitespace (true for the first Token, too)
 */
case class Token(word: String, first:Int, whitespaceBefore: Boolean) extends TokenListElem {
   override def toString = word //+ "@" + first.toString
   def last = first + word.length - 1
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
class MatchedList(var tokens: List[TokenListElem], val an: ActiveNotation, val first: Int, val last: Int) extends TokenListElem {
   override def toString = tokens.map(_.toString).mkString("{" + an.notation.name + " ", " ", " " + an.notation.name + "}")
   def scan(nots: List[TextNotation]) {
      tokens = tokens map {
         case ml: MatchedList =>
            ml.scan(nots)
            ml
         case ul: UnmatchedList =>
            ul.scanner.scan(nots)
            if (ul.scanner.length == 1 && ul.scanner.isInstanceOf[MatchedList])
               ul.scanner.tl(0)
            else
               ul
         case t: Token =>
            throw ImplementationError("single Token in MatchedList") // impossible if produced by TokenList.reduce
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
   var scanner: Scanner = null
   override def toString = "{ " + tl.toString + " }"
   def first = tl(0).first
   def last = tl(tl.length - 1).last
}
