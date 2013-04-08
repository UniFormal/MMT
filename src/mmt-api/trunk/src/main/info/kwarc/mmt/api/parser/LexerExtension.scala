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
 * A LexerExtension that lexes undelimited number literals
 * 
 * always accepts nonLetter digit*
 * 
 * @param floatAllowed if true, also accepts nonLetter digit* . digit*
 */
class NumberLiteralHandler(floatAllowed: Boolean) extends LexerExtension {
  def applicable(s: String, i: Int) = {
     val previousOK = if (i == 0)
        true
     else {
        val previous = s(i-1)
        ! previous.isLetter && ! (previous.getType == java.lang.Character.CONNECTOR_PUNCTUATION)
     }
     s(i).isDigit && previousOK
  }
  def apply(s: String, index: Int, firstPosition: SourcePosition) = {
     var i = index
     while (i < s.length && s(i).isDigit) {
        i += 1
     }
     if (floatAllowed && i < s.length && s(i) == '.') {
         i += 1
    	 while (i < s.length && s(i).isDigit) { //continuing
    		 i += 1
    	 }
         val text = s.substring(index,i)         	 
         CFExternalToken(text, firstPosition, objects.OMF(text.toFloat))

     } else {
    	 val text = s.substring(index,i)        			 
    	 CFExternalToken(text, firstPosition, objects.OMI(text.toInt))
     }
  }
}

abstract class QuoteEvalPart
case class QuotePart(text: String) extends QuoteEvalPart
case class EvalPart(text: String) extends QuoteEvalPart

/**
 * A LexerExtension that lexes natural number literals
 * 
 * accepts nonLetter digit*
 */
class QuoteEval(bQ: String, eQ: String, bE: String, eE: String) extends LexerExtension {
  def applicable(s: String, i: Int) = s.substring(i).startsWith(bQ)
  def apply(s: String, index: Int, fp: SourcePosition) = {
     var level = 1
     var i = index+1
     var parts : List[QuoteEvalPart] = Nil
     var current = ""
     while (i < s.length && level > 0) {
        if (s.substring(i).startsWith(bQ) && level % 2 == 0) {
           level += 1
           i += bQ.length
        } else if (s.substring(i).startsWith(eQ) && level % 2 == 1) {
           level -= 1
           i += eQ.length
        } else if (s.substring(i).startsWith(bE) && level % 2 == 1) {
           level += 1
           i += bE.length
           parts ::= QuotePart(current)
           current = ""
        } else if (s.substring(i).startsWith(eE) && level % 2 == 0) {
           level -= 1
           i += eE.length
           parts ::= EvalPart(current)
           current = ""
        } else {
           current += s(i)
           i += 1
        }
     }
     val text = s.substring(index,i)
     new ExternalToken(text) {
        val firstPosition = fp
        def parse(outer: ParsingUnit, boundVars: List[LocalName], parser: AbstractObjectParser) = {
           var current = fp.after(bQ) //invariant: first character of current part
           val parsed: List[Term] = parts map {
              case QuotePart(q) =>
                 current = current.after(q + bE) 
                 val t = OMSTR(q)
                 SourceRef.update(t, outer.source.copy(region = SourceRegion(current, current.after(q))))
                 t
              case EvalPart(e) =>
                 val cont = outer.context ++ Context(boundVars.map(VarDecl(_,None,None)) :_*)
                 val ref = outer.source.copy(region = SourceRegion(current, current.after(e)))
                 current = current.after(e + eE) 
                 val pu = ParsingUnit(ref, outer.scope, cont, e)
                 parser(pu)
           }
           OMSemiFormal(parsed.map(Formal(_)))
        }
     }
  }
}


/**
 * An EscapeHandler detects foreign tokens, which parse into semi-formal objects
 */
trait EscapeHandler extends LexerExtension {
   /** determines whether an Escaped token begins at s(i) */
   def applicable(s: String, i: Int): Boolean
   /** return initial escape sequence, actual text, and terminal escape sequence */
   def lex(s: String, i: Int): (String,String,String)
   def parse(begin: String, text: String, end: String): Term
   /** pre: applicable(s,i) == true
    * 
    * @return the Escaped Token
    */
   def apply(s: String, i: Int, firstPosition: SourcePosition): CFExternalToken = {
      val (begin,text,end) = lex(s, i)
      val t = parse(begin, text, end)
      CFExternalToken(begin+text+end, firstPosition, t)
   }
}

trait SemiFormalParser {
   def parse(begin: String, text: String, end: String): Term = {
      //TODO clean up
      val r = Reader(text)
      val (format,_) = r.readToken
      val (rest,_) = r.readAll
      r.close
      OMSemiFormal(objects.Text(format, rest))
   }
}

/**
 * an EscapeHandler that detects tokens delimited by begin and end
 * 
 * nested escapes are allowed
 * 
 * typical example: AsymmetricEscapeHandler(/*, */)
 */
abstract class AsymmetricEscapeHandler(begin: String, end: String) extends EscapeHandler {
  def applicable(s: String, i: Int) = s.substring(i).startsWith(begin)
  def lex(s: String, index: Int) = {
     var level = 1
     var i = index+1
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
abstract class SymmetricEscapeHandler(delim: Char, exceptAfter: Char) extends EscapeHandler  {
  def applicable(s: String, i: Int) = s(i) == delim
  def lex(s: String, index: Int) = {
     var i = index+1
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


object GenericEscapeHandler extends AsymmetricEscapeHandler(Reader.escapeChar.toString, Reader.unescapeChar.toString) with SemiFormalParser
object IEEEFloatLiteral extends AsymmetricEscapeHandler("f\"", "\"") {
   def parse(begin: String, text: String, end: String): Term = ???
}