package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import objects._
import utils._
import utils.MyList._

import scala.util.Try

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
abstract class LexerExtension extends Rule {
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
class PrefixedTokenLexer(delim: Char, onlyLetters: Boolean = true, includeDelim: Boolean = true) extends LexerExtension {
  def applicable(s: String, i: Int) = s(i) == delim
  def apply(s: String, index: Int, firstPosition: SourcePosition) = {
     var i = index+1
     while (i < s.length && !s(i).isWhitespace && (!onlyLetters || s(i).isLetter)) {
        i += 1
     }
     val start = if (includeDelim) index else index + 1
     val word = s.substring(start, i)
     val text = if (includeDelim) None else Some(delim + word)
     Token(word, firstPosition, true, text)
  }
}

object MMTURILexer extends PrefixedTokenLexer('`', false, false)

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
    *  @return the result of lexing, and the original input that was lexed (including escape sequences etc.)
    */
   def apply(s: String, i: Int): (String,String)

   /** unapply(apply(s,_)) == s */
   def unapply(s: String): String
}

/** the parsing part of a [[LexParseExtension]] */
abstract class ParseFunction {
   /** @param text as returned by lex
    *  @return the parsed Term
    */
   def apply(text: String): Term

   /** unapply(apply(s)) = s */
   def unapply(t: Term): String

  /** just for overriding **/
  def applicable(s : String) = true
}

/**
 * A LexParseExtension is a LexerExtension with a lex and a parse component.
 */
class LexParseExtension(lc: LexFunction, pc: ParseFunction) extends LexerExtension {
   def applicable(s: String, i: Int) = lc.applicable(s, i) && pc.applicable(lc.apply(s,i)._1)
   def apply(s: String, i: Int, firstPosition: SourcePosition): CFExternalToken = {
      val (text,eaten) = lc(s, i)
      val t = pc(text)
      CFExternalToken(eaten, firstPosition, t)
   }

   def unapply(t: Term) = lc.unapply(pc.unapply(t))
}


/**
 * A LexerExtension that lexes undelimited number literals
 *
 * always accepts digit* after non-(letter or connector)
 *
 * @param floatAllowed if true, accepts digit* [. digit+ [e [-] digit+]] after nonLetter
 * @param fractionAllowed if true, accepts digit* / digit* after nonLetter
 *
 * It's not allowed that both parameters are true.
 */
class NumberLiteralLexer(floatAllowed: Boolean, fractionAllowed: Boolean, floatRequired : Boolean = false) extends LexFunction {
  /*
  def applicable(s: String, i: Int) = {
     val previousOK = if (i == 0)
        true
     else {
        val previous = s(i-1)
        ! previous.isLetter && ! (previous.getType == java.lang.Character.CONNECTOR_PUNCTUATION)
     }
     s(i).isDigit && previousOK
  }
  */
  def applicable(s: String, i: Int): Boolean = {
    val previousOK = if (i == 0)
      true
    else {
      val previous = s(i-1)
      ! previous.isLetter && ! (previous.getType == java.lang.Character.CONNECTOR_PUNCTUATION)
    }
    val isnumber = s(i).isDigit && previousOK
    if (isnumber && floatRequired) {
      var j = i
      var containsperiod = false
      while(j < s.length && (s(j).isDigit || s(j) == '.')) {
        if (s(j) == '.') {
          containsperiod = true
          j = s.length
        }
        j += 1
      }
      containsperiod
    } else isnumber
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
          i += 1
          scanDigits
          if (startsWithCharAndDigit('e')) {
             i += 1
             scanDigits
          } else if (startsWithCharsAndDigit('e', '-')) {
             i += 2
             scanDigits
          }
       }
     } else if (fractionAllowed) {
        if (startsWithCharAndDigit('/')) {
           i+=1
           scanDigits
        }
     }
     val text = s.substring(index,i)
     (text, text)
  }

  def unapply(s: String) = s
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
     val first = index+begin.length
     var i = first
     var level = 1
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
     val text = if (level > 0) s.substring(first) else s.substring(first,i-end.length)
     (text, begin+text+end)
  }

  def unapply(s: String) = begin + s + end
}

/**
 * an EscapeHandler that detects tokens delimited by delim
 *
 * @param delim the begin and end Char
 * @param exceptAfter the escape character to use delim within the escaped text
 *
 * typical example: SymmetricEscapeLexer(", \)
 */
class SymmetricEscapeLexer(delim: Char, exceptAfter: Char) extends LexFunction {
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
     (text, delim.toString + text + (if (i < s.length) delim.toString else ""))
  }

  def unapply(s: String) = delim.toString + s + delim.toString
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


case class FiniteKeywordsLexer(keys : List[String]) extends LexFunction {
  /** unapply(apply(s,_)) == s */
  val sortedkeys = keys.sortBy(k => -k.length)
  override def unapply(s: String): String = s

  override def applicable(s : String, i : Int) : Boolean = {
    if (i!=0 && s(i-1).isLetterOrDigit) return false
    val si = s.substring(i)
    sortedkeys.exists {k => si == k || (si.startsWith(k) && !si(k.length).isLetterOrDigit)}
  }

  override def apply(s: String, i: Int): (String,String) = {
    val ret = sortedkeys.find(k => s.substring(i).startsWith(k)).get
    (ret,ret)
  }
}

class LiteralParser(rt: uom.RealizedType) extends ParseFunction {
   def apply(text: String) = rt.parse(text)
   def unapply(t: Term) = t match {
     case l: OMLITTrait => l.toString
     case _ => throw ImplementationError("not a literal")
   }

  override def applicable(s: String): Boolean = Try(apply(s)) match {
    case scala.util.Success(v : OMLIT) => super.applicable(s)
    case _ => false
  }
}

class SemiFormalParser(formatOpt: Option[String]) extends ParseFunction {
   def apply(text: String): Term = {
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

   def unapply(t: Term) = {
     t match {
       case OMSemiFormal(Text(format, text)::Nil) => formatOpt match {
         case Some(_) => text
         case None => format + " " + text
       }
       case _ => throw ImplementationError("not a semiformal text")
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

/** auxiliary class for [[StringInterpolationToken]]: part of the interpolated string */
abstract class StringInterpolationPart {
  def reg: SourceRegion
}
case class StringPart(text: String, val reg: SourceRegion, var ref: SourceRef) extends StringInterpolationPart
case class MMTPart(unparsed: String, val reg: SourceRegion, var term: Term) extends StringInterpolationPart

/** the result of a [[StringInterpolationLexer]], carrying the continuation for parsing */
case class StringInterpolationToken(text: String, firstPosition: SourcePosition, parts: List[StringInterpolationPart], lexer: StringInterpolationLexer) extends ExternalToken(text) {
    def parse(eti: ExternalTokenParsingInput) = {
       // parse all MMT parts
       parts foreach {
          case s: StringPart =>
            s.ref = eti.outer.source.copy(region = s.reg)
          case m: MMTPart =>
            val e = m.unparsed
            val boundVars = BoundName.getVars(eti.boundNames)
            val cont = eti.outer.context ++ Context(boundVars.map(VarDecl(_,None,None,None,None)) :_*)
            val ref = eti.outer.source.copy(region = m.reg)
            val pu = ParsingUnit(ref, cont, e, eti.outer.nsMap)
            m.term = eti.parser(pu)(eti.errorCont).toTerm
       }
       lexer.makeTerm(this, eti)
    }
}

/**
 * A LexerExtension for string interpolation.
 *
 * @param trigger the first part of the opening  string before
 * @param str string delimiters for
 * @param mmt delimiters for mmt terms inside the string
 *
 * The opening bracket is split into two parts so that multiple instances with different triggers can coexist if they use the same str-bracket.
 * The lexed string is of the form: trigger str.begin S(0) mmt.begin M(0) mmt.end S(1) ... str.end
 * str.begin and str.end may occur inside M(i) if and only if they are part of another string interpolation (possibly with a different trigger).
 *
 * We do not store this nesting structure here - that is handled when recursively parsing M(i) later. But we keep track of it to avoid stopping too early.
 */
abstract class StringInterpolationLexer(trigger: String, str: Bracket, val mmt: Bracket) extends LexerExtension {
  def begin = trigger + str.begin
  def end = str.end
  /**
   * builds a term from the interpolated string
   * pre: parts begins and ends with StringPart, part types alternate
   */
  def makeTerm(token: StringInterpolationToken, eti: ExternalTokenParsingInput): Term

  def applicable(s: String, i: Int) = s.substring(i).startsWith(begin)
  def apply(s: String, index: Int, fp: SourcePosition) = {
     var level = 1 // nesting of string and mmt parts, odd: string part, even: mmt part, 0: end of token reached
     // the current position, current part, and list of parts, and the methods to move the position and create parts
     var currentPos = fp
     var startPosOfCurrent = currentPos
     var i = index+begin.length
     var parts: List[StringInterpolationPart] = Nil // invariant: alternating types of parts, begins and ends with a string part
     var current = ""
     def advanceBy(a: String, addToCurrent: Boolean) {
       currentPos = currentPos.after(a)
       i += a.length
       if (addToCurrent) current += a
     }
     def newPart(stringPart: Boolean, delim: String) {
       val reg = SourceRegion(startPosOfCurrent, currentPos)
       val part = if (stringPart) StringPart(current, reg, null) else MMTPart(current, reg, null)
       parts ::= part
       current = ""
       startPosOfCurrent = currentPos.after(delim)
       advanceBy(delim, false)
     }
     // step through the string
     while (i < s.length && level > 0) {
        if (s.substring(i).startsWith(str.begin) && level % 2 == 0) {
           level += 1
           advanceBy(str.begin, true)
        } else if (s.substring(i).startsWith(str.end) && level % 2 == 1) {
           level -= 1
           if (level == 0) {
             // the last string part, possibly empty
             newPart(stringPart = true, str.end)
           } else {
             advanceBy(str.end, true)
           }
        } else if (s.substring(i).startsWith(mmt.begin) && level % 2 == 1) {
           if (level == 1) {
             newPart(stringPart = true, mmt.begin)
           } else {
             advanceBy(mmt.begin, true)
           }
           level += 1
        } else if (s.substring(i).startsWith(mmt.end) && level % 2 == 0) {
           if (level == 2) {
             newPart(stringPart = false, mmt.end)
           } else {
             advanceBy(mmt.end, true)
           }
           level -= 1
        } else {
           advanceBy(s(i).toString, true)
        }
     }
     if (i == s.length) {
       // end of input reached, recover by adding final parts, we always end with a string part
       if (level % 2 == 0) {
         newPart(stringPart = true, "")
       } else {
         newPart(stringPart = false, "")
         newPart(stringPart = true, "")
       }
     }
     parts = parts.reverse
     val text = s.substring(index,i)
     new StringInterpolationToken(text, fp, parts, this)
  }
}

/** quotation of MMT terms */
class QuotationLexer(quoteType: GlobalName, quoteTerm: GlobalName) extends StringInterpolationLexer("q", Bracket("\"", "\""), Bracket("${","}")) {
  private object QuotedTerm extends uom.RepresentedRealizedType(OMS(quoteType), uom.TermLiteral)

  def makeTerm(token: StringInterpolationToken, eti: ExternalTokenParsingInput) = {
    var context = Context.empty
    val strings = token.parts map {
      case StringPart(s,_,_) => s
      case m: MMTPart =>
        val name = "SIP_" + context.length.toString
        context ++= OMV(name) % m.term
        " " + name + " "
    }
    val names = context.map(_.name)
    val str = strings.mkString
    val fullcontext = eti.outer.context ++ context //TODO not obvious which context should be used for the quoted term
    val srcref = eti.outer.source.copy(region = token.region)
    val pu = ParsingUnit(srcref, fullcontext, str, eti.outer.nsMap)
    val t = eti.parser(pu)(eti.errorCont).toTerm
    val freeVars = t.freeVars diff names
    if (freeVars.nonEmpty) {
      val e = SourceError("quotation lexer", srcref, "free variables in quoted term: " + names.mkString(", "))
      eti.errorCont(e)
    }
    OMA(OMS(quoteTerm), QuotedTerm(t) :: context.map(_.toOML))
  }
}
