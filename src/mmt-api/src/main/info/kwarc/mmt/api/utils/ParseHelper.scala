package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}

import scala.collection.mutable
import scala.util.parsing.combinator.{PackratParsers, Parsers, RegexParsers}
import scala.util.parsing.input.{Position, Reader}

/**
  * helper class for working with substrings without copying
  * (standard Java substring method creates a copy)
  */
case class StringSlice(s: String, from: Int, to: Int) extends java.lang.CharSequence {
   override def toString = s.substring(from,to)
   def charAt(n: Int) = s(from+n)
   def length = to-from
   def subSequence(f: Int, t: Int) = StringSlice(s, from+f,from+t)
   def startsWith(prefix: String) = {
      prefix.length <= length &&
      s.substring(from, from + prefix.length) == prefix
   }
}

object StringSlice {
   def apply(s: String, from: Int) : StringSlice = StringSlice(s, from, s.length)
}

/**
  * a pair of matching brackets, e.g., BracketPair("[","]",false) or BracketPair("//","\n",true)
  * see Unparsed.takeUntilString
  * @param ignore if true, the bracketed part can be dropped from the read string
  */
case class BracketPair(open: String, close: String, ignore: Boolean)

/** \n, \r, and \r\n are read as \n */
class Unparsed(input: String, error: String => Nothing) extends Reader[Char] {self =>
   private var current: Int = 0
   private val length = input.length

   def empty = current == length
   def left = length - current

   override def toString = "unparsed string at position " + getSourcePosition + " starting with " + remainder.subSequence(0,Math.min(left,10))

   /** number of characters that have been eaten */
   private var poffset: Int = 0
   private var line : Int = 0
   private var column : Int = 0
   private def advancePositionBy(c: Char): Unit = {
      poffset += 1
      column += 1
      if (c == '\n') {
         line += 1
         column = 0
      }
   }
   /** the position of the next character to be read */
   def getSourcePosition = SourcePosition(poffset, line, column)

   /** get the remaining input (no copy) */
   def remainder = StringSlice(input, current)

   def head = input(current)
   /** get the next n characters without advancing the position (no copy) */
   def getnext(n:Int) = StringSlice(input,current,current + n)

   def errorExpected(exp: String) = error("expected: " + exp + "; found " + remainder.subSequence(0,200))

   /** get the next character and advance position */
   def next() = {
      if (empty) error("expected character, found nothing")
      var c = head
      current += 1
      if (c == '\r') {
         if (!empty && head == '\n') {
            current += 1
         }
         advancePositionBy(c)
         c = '\n'
      }
      advancePositionBy(c)
      c
   }
   /** discard initial character */
   def tail: this.type = {
      next()
      this
   }
   /** discard initial whitespace */
   def trim: this.type = {
      while (!empty && head.isWhitespace) next()
      this
   }
  /** discard a specific initial string */
   def drop(s: String): Unit = {
      if (StringSlice(input, poffset).startsWith(s)) {
         current += s.length
         s.foreach {c => advancePositionBy(c)}
      } else
         errorExpected(s)
   }
   /** discard a specific initial char */
   def drop(c: Char): Unit = drop(c.toString)
   /** drops a String if possible
    *  @return true if dropped
    */
   def takeIf(s: String): Boolean = {
    if (remainder.startsWith(s)) {
      drop(s)
      true
    } else
      false
   }

   /** read characters that satisfy a condition */
   def takeWhile(test: Char => Boolean): String = {
      val sb = new StringBuilder
      while ({
         val testchar = if (remainder.startsWith("\r\n")) '\n' else head
         test(testchar)
      }) {
        if (remainder.startsWith("\r\n")) next()
        sb += next()
      }
      sb.toString()
   }

  /** read characters that satisfy a condition, stops when string is empty (rather than throwing an error) */
  def takeWhileSafe(test: Char => Boolean): String = {
    val sb = new StringBuilder
    while (input.isDefinedAt(current) && {
      val testchar = if (remainder.startsWith("\r\n")) '\n' else head
      test(testchar)
    }) {
      if (remainder.startsWith("\r\n")) next()
      sb += next()
    }
    sb.toString()
  }

   import scala.util.matching.Regex
   /** matches at the beginning of the stream and returns the matched prefix */
   def takeRegex(regex: String): Regex.Match = {
      val r = new Regex(regex)
      val mt = r.findPrefixMatchOf(remainder).getOrElse {
         errorExpected("match of " + regex)
      }
      drop(mt.matched)
      mt
   }

   /** returns the string until the first match of a regular expression */
   def takeUntilRegex(regex: String): String = {
      val mt = takeRegex("(.*)"+regex)
      mt.group(1)
   }

   /**
    * returns all characters up to the next unescaped occurrence of 'until' (that occurrence is eaten but not returned)
    * @param until the delimiter to scan for
    * @param exceptAfter the an escape character
    * @return the found string (excluding the until), and false iff end of input reached
    */
   def takeUntilChar(until: Char, exceptAfter: Char): (String,Boolean) = {
      val seen = new mutable.StringBuilder()
      while (!empty && head != until) {
         if (head == exceptAfter) {
            seen.addOne(head)
               next()
         }
         seen.addOne(head)
         next()
      }
      if (empty) {
         (seen.mkString, false)
      }
      else {
         next()
         (seen.mkString,true)
      }
   }

   /** return all characters until a certain string is encountered outside well-nested brackets */
   def takeUntilString(until: String, brackets: List[BracketPair]): String = {
      val seen = new mutable.StringBuilder()
      while (true) {
        val r = remainder
        if (empty) {
          error("expected a closing bracket, found end of input")
        } else if (r.startsWith(until)) {
          drop(until)
          return seen.mkString
        } else {brackets.find(bp => r.startsWith(bp.open)) match {
          case Some(bp) =>
            drop(bp.open)
            val s = takeUntilString(bp.close, brackets)
            if (!bp.ignore) {
               seen ++= bp.open
               seen ++= s
               seen ++= bp.close
            }
          case None =>
            seen.addOne(next())
        }}
      }
      seen.mkString // impossible
   }

   // Reader Instance

   override def offset : Int          = current
   override def source : CharSequence = input
   class UnparsedPosition extends Position {
      val line   : Int = self.line
      val column : Int = self.column

      /* Potentially inefficient? */
      def lineContents : String = {
         val curr : Int = current
         var l, r : Int = curr
         val delims = List('\n', '\r')
         while (!delims.contains(input(l-1))) { l -= 1 }
         while (!delims.contains(input(r+1))) { r += 1 }
         input.substring(l,r)
      }
   }

   def pos   : Position     = new UnparsedPosition
   def atEnd : Boolean      = empty
   def rest  : Reader[Char] = tail
   def first : Char         = head
}

trait UnparsedParsers extends RegexParsers with PackratParsers {
  override type Elem  = Char
  override type Input = Reader[Char]
}