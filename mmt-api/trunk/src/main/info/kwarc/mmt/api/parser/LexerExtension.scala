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
   /**
    * @param s the string to lex
    * @param i the current character
    * @return true iff this extension accepts a Token that begins at s(i)
    */
   def applicable(s: String, i: Int): Boolean
   /** pre: applicable(s,i) == true
    * @param s the string to lex
    * @param i the current character
    * @param firstPosition the SourcePosition of the first character
    * @return the lexed Token
    */
   def apply(s: String, i: Int, firstPosition: SourcePosition): PrimitiveTokenListElem
}

/**
 * a LexerExtension that detects id (letter sequences) Tokens prefixed by delim
 * 
 * @param delim the begin Char
 * 
 * typical example: PrefixedTokenLexer(\)
 */
class PrefixedTokenLexer(delim: Char) extends LexerExtension {
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
 * replaces words during lexing
 * @param maps a list of pairs (a,b) such that a will be lexed as the Token b
 */
abstract class WordReplacer extends LexerExtension {
   val maps: List[(String,String)]
   /** caches the result of applicable so that apply does not have to traverse the list again */
   private var memory: Option[(String,Int,(String,String))] = None
   def applicable(s: String, i: Int) = {
      val si = s.substring(i)
      val km = maps mapFind {case (k,m) =>
         if (si.startsWith(k)) {
            val next = i+k.length
            if (next == s.length || ! TokenList.canFollow(k.last, s(next)))
               Some((k,m))
            else None
         } else
            None
      }
      if (km.isDefined) {
         memory = Some((s,i,km.get))
         true
      } else
         false
   }
   def apply(s: String, i: Int, firstPosition: SourcePosition): Token = {
      val (k,m) = memory match {
         case Some((ms, mi, km)) if ms == s && mi == i => km
         case _ =>
            applicable(s, i)
            memory.get._3
      }
      val wsBefore = i == 0 || TokenList.isWhitespace(s(i-1))
      Token(m, firstPosition, wsBefore, Some(k))
   }
}

/** replaces typical multi-symbol operators (e.g., arrows and double brackets) with the corresponding Unicode symbol */
object UnicodeReplacer extends WordReplacer {
   val maps = List("->" -> "→", "<-" -> "←", "<->" -> "↔",
                   "=>" -> "⇒", "<=" -> "⇐", "<=>" -> "⇔",
                   "-->" -> "⟶", "<--" -> "⟵", "<-->" -> "⟷",
                   "==>" -> "⟹", "<==" -> "⟸","<==>" -> "⟺",
                   "=<" -> "≤", ">=" -> "≥",
                   "<<" -> "⟪", ">>" -> "⟫", "[[" -> "⟦", "]]" -> "⟧",
                   "\\/" -> "∨", "/\\" -> "∧",
                   "!=" -> "≠"
              )
}

/** the lexing part of a [[LexParseExtension]] */
abstract class LexFunction {
   /**
    * @param s the string to lex
    * @param i the current character
    * @return true iff this extension accepts a Token that begins at s(i)
    */
   def applicable(s: String, i: Int): Boolean
   /** @param s the string to lex
    *  @param i the current position in s
    *  @return initial escape sequence, actual text, and terminal escape sequence
    */
   def apply(s: String, i: Int): (String,String,String)   
}

/** the parsing part of a [[LexParseExtension]] */
abstract class ParseFunction {
   /** @param begin as returned by lex
    *  @param text as returned by lex
    *  @param end as returned by lex
    *  @return the parsed Term
    */
   def apply(begin: String, text: String, end: String): Term
}

/**
 * A LexParseExtension is a LexerExtension with a lex and a parse component.
 */
class LexParseExtension(lc: LexFunction, pc: ParseFunction) extends LexerExtension {
   def applicable(s: String, i: Int) = lc.applicable(s, i) 
   def apply(s: String, i: Int, firstPosition: SourcePosition): CFExternalToken = {
      val (begin,text,end) = lc(s, i)
      val t = pc(begin, text, end)
      CFExternalToken(begin+text+end, firstPosition, t)
   }
}

/**
 * A LexerExtension that lexes undelimited number literals
 * 
 * always accepts digit* after nonLetter
 * 
 * @param floatAllowed if true, accepts digit* [. digit+ [e [-] digit+]] after nonLetter
 * @param fractionAllowed if true, accepts digit* / digit* after nonLetter
 * 
 * It's not allowed that both parameters are true.
 */
class NumberLiteralLexer(floatAllowed: Boolean, fractionAllowed: Boolean) extends LexFunction {
  def applicable(s: String, i: Int) = {
     val previousOK = if (i == 0)
        true
     else {
        val previous = s(i-1)
        ! previous.isLetter && ! (previous.getType == java.lang.Character.CONNECTOR_PUNCTUATION)
     }
     s(i).isDigit && previousOK
  }
  def apply(s: String, index: Int) = {
     var i = index
     def scanDigits {
        while (i < s.length && s(i).isDigit) {
           i += 1
        }
     }
     def startsWithCharAndDigit (c: Char         ) = i+1 < s.length && s(i) == c                && s(i+1).isDigit
     def startsWithCharsAndDigit(c: Char, d: Char) = i+2 < s.length && s(i) == c && s(i+1) == d && s(i+2).isDigit
     scanDigits
     if (floatAllowed) {
       if (startsWithCharAndDigit('.')) {
          i += 2
          scanDigits
          if (startsWithCharAndDigit('e')) {
             i += 2
             scanDigits
          } else if (startsWithCharsAndDigit('e', '-')) {
             i += 2
             scanDigits
          }
       }
     } else if (fractionAllowed) {
        if (startsWithCharAndDigit('/')) {
           i+=2
           scanDigits
        }
     }
     ("", s.substring(index,i), "")
  }
}

/**
 * an EscapeHandler that detects tokens delimited by begin and end
 * 
 * nested escapes are allowed
 * 
 * typical example: AsymmetricEscapeHandler(/*, */)
 */
class AsymmetricEscapeLexer(begin: String, end: String) extends LexFunction {
  def applicable(s: String, i: Int) = s.substring(i).startsWith(begin)
  def apply(s: String, index: Int) = {
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
 * @param exceptAfter the escape character to use delim within the escaped text
 * 
 * typical example: SymmetricEscapeLexer(", \)
 */
class SymmetricEscapeLexer(delim: Char, exceptAfter: Char) extends LexFunction  {
  def applicable(s: String, i: Int) = s(i) == delim
  def apply(s: String, index: Int) = {
     var i = index+1
     while (i < s.length && s(i) != delim) {
        if (s.substring(i).startsWith(exceptAfter.toString + delim)) {
           i += 2
        } else
           i += 1
     }
     val text = s.substring(index+1,i)
     (delim.toString, text, if (i < s.length) delim.toString else "")
  }
}

class LiteralParser(rt: uom.RealizedType) extends ParseFunction {
   def apply(begin: String, text: String, end: String) = rt.parse(text)
}

class FixedLengthLiteralLexer(rt: uom.RealizedType, begin: String, length: Int) extends LexerExtension {
   def applicable(s: String, i: Int) = s.substring(i).startsWith(begin)
   def apply(s: String, i: Int, firstPosition: SourcePosition) = {
      val from = i+begin.length
      val text = s.substring(from, from+length)
      val t = rt.parse(text)
      CFExternalToken(begin+text, firstPosition, t)
   }
}

class SemiFormalParser(formatOpt: Option[String]) extends ParseFunction {
   def apply(begin: String, text: String, end: String): Term = {
      formatOpt match {
         case Some(format) =>
            OMSemiFormal(objects.Text(format, text))
         case None => 
            val r = Reader(text)
            val (format,_) = r.readToken
            val (rest,_) = r.readAll
            r.close
            OMSemiFormal(objects.Text(format, rest))
      }
   }
}

object GenericEscapeLexer extends LexParseExtension(
   new AsymmetricEscapeLexer(Reader.escapeChar.toString, Reader.unescapeChar.toString),
   new SemiFormalParser(None)
)

object QuoteLexer extends LexParseExtension(
   new SymmetricEscapeLexer('\"', '\\'), new SemiFormalParser(Some("quoted"))
)

abstract class QuoteEvalPart
case class QuotePart(text: String) extends QuoteEvalPart
case class EvalPart(text: String) extends QuoteEvalPart

/**
 * A LexerExtension that lexes nested formal/informal terms
 * 
 * @param bQ beginning of quoted (informal) part at toplevel or inside a formal part
 * @param eQ end of quoted (informal) part
 * @param bE beginning of evaluated (formal) part inside an informal part
 * @param eE end of evaluated (formal) part 
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
        def parse(outer: ParsingUnit, boundVars: List[LocalName], parser: ObjectParser) = {
           var current = fp.after(bQ) //invariant: first character of current part
           val parsed: List[Term] = parts map {
              case QuotePart(q) =>
                 current = current.after(q + bE) 
                 val t = uom.OMLiteral.OMSTR(q)
                 SourceRef.update(t, outer.source.copy(region = SourceRegion(current, current.after(q))))
                 t
              case EvalPart(e) =>
                 val cont = outer.context ++ Context(boundVars.map(VarDecl(_,None,None,None)) :_*)
                 val ref = outer.source.copy(region = SourceRegion(current, current.after(e)))
                 current = current.after(e + eE) 
                 val pu = ParsingUnit(ref, cont, e, NamespaceMap.empty) //TODO better namespace map
                 parser(pu)(ErrorThrower).toTerm
           }
           OMSemiFormal(parsed.map(Formal(_)))
        }
     }
  }
}