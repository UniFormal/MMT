package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import objects._
import utils.MyList._

/**
 * An EscapeManager handles escaping between languages during tokenization
 * 
 * @param handlers the individual EscapeHandler to be used 
 * 
 * It is used by the object [[info.kwarc.mmt.api.parser.TokenList]]
 */
class EscapeManager(handlers: List[LexerExtension]) {
   /**
    * @param s the tokenized String
    * @param i the next Char in s to be considered
    * @param firstPosition the SourcePosition of that Char
    * @return the produced Token if any of the LexerExtensions
    *   detects an escape at this position
    */
   def apply(s: String, i: Int, firstPosition: SourcePosition): Option[PrimitiveTokenListElem] = {
      handlers.find(_.applicable(s,i)) map {eh => eh.apply(s,i,firstPosition)}
   }
}

/**
 * A LexerExtension bypasses the default lexing algorithm
 */
abstract class LexerExtension {
   /** determines whether an accepted token begins at s(i) */
   def applicable(s: String, i: Int): Boolean
   /** pre: applicable(s,i) == true
    * 
    * @return the Token
    */
   def apply(s: String, i: Int, firstPosition: SourcePosition): PrimitiveTokenListElem
}

/**
 * a LexerExtension that detects ids (letter sequences) Tokens prefixed by delim
 * 
 * @param delim the begin Char
 * 
 * typical example: PrefixEscapeHandler(\)
 */
class PrefixEscapeHandler(delim: Char) extends LexerExtension {
  def applicable(s: String, i: Int) = s(i) == delim
  def apply(s: String, index: Int, firstPosition: SourcePosition) = {
     var i = index+1
     while (i < s.length && s(i).isLetter) {
           i += 1
     }
     val text = s.substring(index, i)
     Token(text, firstPosition, true)
  }
}

/**
 * A LexerExtension that lexes natural number literals
 * 
 * accepts nonLetter digit*
 */
class NatLiteralHandler(negativeAllowed: Boolean) extends LexerExtension {
  def applicable(s: String, i: Int) = s(i).isDigit && (i == 0 || ! s(i-1).isLetter)
  def apply(s: String, index: Int, firstPosition: SourcePosition) = {
     var i = index
     while (i < s.length && s(i).isDigit) {
        i += 1
     }
     val text = s.substring(index,i)
     CFExternalToken(text, firstPosition, objects.OMI(text.toInt))
  }
}

/**
 * An EscapeHandler detects foreign tokens, which parse into semi-formal objects
 */
abstract class EscapeHandler extends LexerExtension {
   /** determines whether an Escaped token begins at s(i) */
   def applicable(s: String, i: Int): Boolean
   /** return initial escape sequence, actual text, and terminal escape sequence */
   def apply(s: String, i: Int): (String,String,String)
   /** pre: applicable(s,i) == true
    * 
    * @return the Escaped Token
    */
   def apply(s: String, i: Int, firstPosition: SourcePosition): CFExternalToken = {
      val (begin,text,end) = apply(s, i)
      //TODO clean up
      val r = Reader(text)
      val (format,_) = r.readToken
      val (rest,_) = r.readAll
      r.close
      CFExternalToken(begin+text+end, firstPosition, OMSemiFormal(objects.Text(format, rest)))
   }
}

/**
 * an EscapeHandler that detects tokens delimited by begin and end
 * 
 * nested escapes are allowed
 * 
 * typical example: AsymmetricEscapeHandler(/*, */)
 */
class AsymmetricEscapeHandler(begin: String, end: String) extends EscapeHandler {
  def applicable(s: String, i: Int) = s.substring(i).startsWith(begin)
  def apply(s: String, index: Int) = {
     var level = 1
     var i = index
     while (i < s.length && level > 0) {
        if (s.substring(i).startsWith(begin)) {
           level += 1
           i += begin.length
        } else if (s.substring(i).startsWith(end)) {
           level -= 1
           i += end.length
        } else 
           i += 1
     }
     val text = s.substring(index+begin.length,i-end.length)
     (begin, text, end)
  }
}

/**
 * an EscapeHandler that detects tokens delimited by delim
 * 
 * @param delim the begin and end Char
 * @param exceptAfter the escape character to used delim within the escaped text
 * 
 * typical example: SymmetricEscapeHandler(", \)
 */
class SymmetricEscapeHandler(delim: Char, exceptAfter: Char) extends EscapeHandler {
  def applicable(s: String, i: Int) = s(i) == delim
  def apply(s: String, index: Int) = {
     var i = index
     while (i < s.length && s(i) != delim) {
        if (s.substring(i).startsWith(exceptAfter.toString + delim)) {
           i += 2
        } else
           i += 1
     }
     val text = s.substring(index+1,i-1)
     (delim.toString, text, delim.toString)
  }
}


object GenericEscapeHandler extends AsymmetricEscapeHandler(Reader.escapeChar.toString, Reader.unescapeChar.toString)
