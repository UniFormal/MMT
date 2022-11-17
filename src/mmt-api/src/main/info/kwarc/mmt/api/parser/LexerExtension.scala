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
      handlers.mapFind(_.apply(s,i,firstPosition))
   }
}

/**
 * A LexerExtension bypasses the default lexing algorithm
 */
abstract class LexerExtension extends Rule {
   /**
    * @param s the string to lex
    * @param i the current character
    * @param firstPosition the SourcePosition of the first character
    * @return the lexed Token, if applicable
    * 
    * lexer extensions are called at every position of the input and therefore must fail very quickly if they are not applicable
    * 
    * Make sure to call StringSlice(s,i) instead of s.substring(i) for efficiency.
    */
   def apply(s: String, i: Int, firstPosition: SourcePosition): Option[PrimitiveTokenListElem]
   /** if given, only applies to tokens that start with trigger */
   def trigger: Option[String]
   /** lexer with longer triggers have higher priority */
   override def priority = trigger.map(_.length).getOrElse(0)
}

/**
 * a LexerExtension that detects id (letter sequences) Tokens prefixed by delim
 *
 * @param delim the begin Char
 * @param includeDelim whether the delimiter is part of the token
 *
 * typical example: PrefixedTokenLexer(\)
 */
abstract class PrefixedTokenLexer(delim: Char, includeDelim: Boolean = true) extends LexerExtension {
  def trigger = Some(delim.toString)
  def apply(s: String, index: Int, firstPosition: SourcePosition): Option[Token] = {
     if (s(index) != delim) return None
     var i = index+1
     while (i < s.length && include(s(i))) {
        i += 1
     }
     val start = if (includeDelim) index else index + 1
     val word = s.substring(start, i)
     val text = if (includeDelim) None else Some(s"$delim$word")
     Some(Token(word, firstPosition, true, text))
  }
  /** true for the characters to be included before ending the token */
  @inline def include(c: Char): Boolean
}

object MMTURILexer extends PrefixedTokenLexer('☞', false) {
  @inline override def include(c: Char) = !c.isWhitespace && ! ("()" contains c)
}

/**
 * replaces words during lexing
 */
abstract class WordReplacer extends LexerExtension {
   /** a list of pairs (a,b) such that a will be lexed as the Token b;
    *  this method is called only once
    */
   def maps: List[(String,String)]
   /** longest keys are tried first */
   // lazy to make sure it does not run during class initialization, which is hard to debug
   private lazy val mapsSorted = maps.sortBy {case (k,_) => -k.length}
   def trigger = None
   def apply(s: String, i: Int, firstPosition: SourcePosition): Option[Token] = {
      val si = StringSlice(s,i)
      val km = mapsSorted mapFind {case (k,m) =>
         if (si.startsWith(k)) {
            val next = i+k.length
            if (next == s.length || ! TokenList.canFollow(k.last, s(next)))
               Some((k,m))
            else None
         } else
            None
      }
      km map {case (k,m) =>
        val wsBefore = i == 0 || TokenList.isWhitespace(s(i-1))
        Token(m, firstPosition, wsBefore, Some(k))
      }
   }
}

/** replaces typical multi-ascii-symbol operators (e.g., arrows and double brackets) with the corresponding Unicode symbol
 *  defined by the file */
object UnicodeReplacer extends WordReplacer {
  def maps = UnicodeMap.readMap("unicode/unicode-ascii-map")
}

object UnicodeMap {
  /** parses resource files of the form command|result into a list of (command,result)
   *  if multiple | occur, the last one is used
   *  empty lines and lines starting with // are skipped
   */
  def readMap(fileName: String) = {
    var m: List[(String,String)] = Nil
    val s = MMTSystem.getResourceAsString(fileName)
    stringToList(s,"\\n") foreach {l =>
      val lT = l.trim
      if (!lT.isEmpty && !lT.startsWith("//")) {
        val i = lT.lastIndexOf("|")
        if (i == -1 || i == lT.length - 1) {
          // | not in l or | is last character of l
          throw GeneralError(s"illegal line in $fileName: $l")
        }
        val command = lT.substring(0,i)
        val result = lT.substring(i+1)
        m ::= (command,result)
      }
    }
    m
  }
}

/** the lexing part of a [[LexParseExtension]] */
abstract class LexFunction {
   /**
    * @param s the string to lex
    * @param i the current character
    * @return true iff this extension accepts a Token that begins at s(i)
    */
   def applicable(s: String, i: Int): Boolean
   def trigger: Option[String]

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
   def apply(s: String, i: Int, firstPosition: SourcePosition): Option[CFExternalToken] = {
      if (!lc.applicable(s, i)) return None
      val (text,eaten) = lc(s, i)
      if (!pc.applicable(text)) return None
      val t = pc(text)
      Some(CFExternalToken(eaten, firstPosition, t))
   }

   def unapply(t: Term) = lc.unapply(pc.unapply(t))
   def trigger = lc.trigger
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
      val previous = s(i-1); // NOTE@twiesing: The ; is needed here
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
  def trigger = None
  def apply(s: String, index: Int) = {
     var i = index
     def scanDigits: Unit = {
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
  def applicable(s: String, i: Int) = StringSlice(s,i).startsWith(begin)
  def trigger = Some(begin)
  def apply(s: String, index: Int) = {
     val first = index+begin.length
     var i = first
     var level = 1
     while (i < s.length && level > 0) {
        if (StringSlice(s,i).startsWith(begin)) {
           level += 1
           i += begin.length
        } else if (StringSlice(s,i).startsWith(end)) {
           level -= 1
           i += end.length
        } else
           i += 1
     }
     val text = s.substring(first,i-end.length)
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
  def trigger = Some(delim.toString)
  def apply(s: String, index: Int) = {
     var i = index+1
     while (i < s.length && s(i) != delim) {
        if (StringSlice(s,i).startsWith(exceptAfter.toString + delim)) {
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
   def apply(s: String, i: Int, firstPosition: SourcePosition): Option[CFExternalToken] = {
      if (!StringSlice(s,i).startsWith(begin)) return None
      val from = i+begin.length
      val text = s.substring(from, from+length)
      val t = rt.parse(text)
      Some(CFExternalToken(begin+text, firstPosition, t))
   }
  def trigger = None
}


case class FiniteKeywordsLexer(keys : List[String]) extends LexFunction {
  /** unapply(apply(s,_)) == s */
  val sortedkeys = keys.sortBy(k => -k.length)
  override def unapply(s: String): String = s

  def applicable(s : String, i : Int) : Boolean = {
    if (i!=0 && s(i-1).isLetterOrDigit) return false
    val si = StringSlice(s,i)
    sortedkeys.exists {k => si.startsWith(k) && (si.length == k.length || !si.charAt(k.length).isLetterOrDigit)}
  }
  def trigger = None

  def apply(s: String, i: Int): (String,String) = {
    val ret = sortedkeys.find(k => StringSlice(s,i).startsWith(k)).get
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
            val tm = eti.callbackParse(m.reg, e)
            m.term = tm
       }
       lexer.makeTerm(this, eti)
    }
}

/**
 * A LexerExtension for string interpolation.
 *
 * @param operator the first part of the opening  string before
 * @param str string delimiters for
 * @param mmt delimiters for mmt terms inside the string
 *
 * The opening bracket is split into two parts so that multiple instances with different operators can coexist if they use the same str-bracket.
 * The lexed string is of the form: operator str.begin S(0) mmt.begin M(0) mmt.end S(1) ... str.end
 * str.begin and str.end may occur inside M(i) if and only if they are part of another string interpolation (possibly with a different operator).
 *
 * We do not store this nesting structure here - that is handled when recursively parsing M(i) later. But we keep track of it to avoid stopping too early.
 */
abstract class StringInterpolationLexer(operator: String, str: Bracket, val mmt: Bracket) extends LexerExtension {
  def begin = operator + str.begin
  def end = str.end
  def trigger = Some(begin)
  /**
   * builds a term from the interpolated string
   * pre: parts begins and ends with StringPart, part types alternate
   */
  def makeTerm(token: StringInterpolationToken, eti: ExternalTokenParsingInput): Term

  def apply(s: String, index: Int, fp: SourcePosition): Option[StringInterpolationToken] = {
     if (!StringSlice(s,index).startsWith(begin)) return None
     var level = 1 // nesting of string and mmt parts, odd: string part, even: mmt part, 0: end of token reached
     // the current position, current part, and list of parts, and the methods to move the position and create parts
     var currentPos = fp
     var startPosOfCurrent = currentPos
     var i = index
     var parts: List[StringInterpolationPart] = Nil // invariant: alternating types of parts, begins and ends with a string part
     var current = ""
     def advanceBy(a: String, addToCurrent: Boolean): Unit = {
       currentPos = currentPos.after(a)
       i += a.length
       if (addToCurrent) current += a
     }
     def newPart(stringPart: Boolean, delim: String, includeDelimInPart: Boolean): Unit = {
       val (endPosOfCurrent,startPosOfNext) = if (includeDelimInPart) {
         advanceBy(delim.init, false)
         val ep = currentPos
         advanceBy(delim.last.toString, false)
         (ep,currentPos)
       } else {
         val cp = currentPos
         advanceBy(delim, false)
         (cp,cp) // TODO actually first cp should be one lower
       }
       val reg = SourceRegion(startPosOfCurrent, endPosOfCurrent)
       val part = if (stringPart) StringPart(current, reg, null) else MMTPart(current, reg, null)
       parts ::= part
       current = ""
       startPosOfCurrent = startPosOfNext
     }
     advanceBy(begin, false)
     // step through the string
     while (i < s.length && level > 0) {
        val si = StringSlice(s,i)
        if (si.startsWith(str.begin) && level % 2 == 0) {
           level += 1
           advanceBy(str.begin, true)
        } else if (si.startsWith(str.end) && level % 2 == 1) {
           level -= 1
           if (level == 0) {
             // the last string part, possibly empty
             newPart(stringPart = true, str.end, true)
           } else {
             advanceBy(str.end, true)
           }
        } else if (si.startsWith(mmt.begin) && level % 2 == 1) {
           if (level == 1) {
             newPart(stringPart = true, mmt.begin, false)
           } else {
             advanceBy(mmt.begin, true)
           }
           level += 1
        } else if (si.startsWith(mmt.end) && level % 2 == 0) {
           if (level == 2) {
             newPart(stringPart = false, mmt.end, true)
           } else {
             advanceBy(mmt.end, true)
           }
           level -= 1
        } else {
           advanceBy(s(i).toString, true)
        }
     }
     if (current.nonEmpty) {
       // end of input reached, recover by adding a final part
       newPart(stringPart = level == 1, " ", false)
     }
     parts = parts.reverse
     val text = s.substring(index,i)
     Some(new StringInterpolationToken(text, fp, parts, this))
  }
}

/** quotation of MMT terms
 *  @param quoteType the type of literals given by quoted terms
 *  @param quoteTerm the constructor that takes a term literal and a substitution for its interpolation points
 */
class QuotationLexer(quoteType: GlobalName, quoteTerm: GlobalName) extends StringInterpolationLexer("q", Bracket("\"", "\""), Bracket("${","}")) {
  private object QuotedTerm extends uom.RepresentedRealizedType(OMS(quoteType), uom.TermLiteral)

  def makeTerm(token: StringInterpolationToken, eti: ExternalTokenParsingInput) = {
    var context = Context.empty
    val strings = token.parts map {
      case StringPart(s,_,_) => s
      case m: MMTPart =>
        val name = LocalName("SIP_" + context.length.toString)
        context ++= VarDecl(name, df = m.term)
        " " + name + " "
    }
    val names = context.map(_.name)
    val str = strings.mkString
    val fullcontext = eti.outer.context ++ context //TODO not obvious which context should be used for the quoted term
    val srcref = eti.outer.source.copy(region = token.region)
    val pu = ParsingUnit(srcref, fullcontext, str, eti.outer.iiContext)
    val parser = eti.parser
    val t = parser(pu)(eti.errorCont).toTerm
    val freeVars = t.freeVars diff names
    if (freeVars.nonEmpty) {
      val e = SourceError("quotation lexer", srcref, "free variables in quoted term: " + names.mkString(", "))
      eti.errorCont(e)
    }
    OMA(OMS(quoteTerm), QuotedTerm(t) :: context.map(_.toOML))
  }
}
