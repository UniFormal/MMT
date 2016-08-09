package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api.parser.SourcePosition

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

/** used by some [[Unparsed]] methods to determine bracketing structures */
case class Brackets(pairs: List[(String,String)])

/** \n, \r, and \r\n are read as \n */
class Unparsed(input: String, error: String => Nothing) {
   private var current: Int = 0
   private val length = input.length
   
   def empty = current == length
   
   /** number of characters that have been eaten */
   private var offset: Int = 0
   private var line : Int = 0
   private var column : Int = 0
   private def advancePositionBy(c: Char) {
      offset += 1
      column += 1
      if (c == '\n') {
         line += 1
         column = 0
      }
   }
   /** the position of the next character to be read */
   def getSourcePosition = SourcePosition(offset, line, column)

   def remainder = StringSlice(input, current)

   def head = input(current)
   def getnext(n:Int) = StringSlice(input,current,current + n)

   def next() = {
      if (empty) error("expected character, found nothing")
      var c = head
      current += 1
      if (c == '\r') {
         if (!empty && head == '\n') {
            current += 1
         }
         c = '\n'
      }
      advancePositionBy(c)
      c
   }
   def tail: this.type = {
      next
      this
   }
   def trim: this.type = {
      while (!empty && head.isWhitespace) next()
      this
   }
   def drop(s: String) {
      if (StringSlice(input, offset).startsWith(s)) {
         current += s.length
         s.foreach {c => advancePositionBy(c)}
      } else
         error(s"expected $s, found " + remainder.subSequence(0,200))
   }
   def next(test: Char => Boolean): String = {
      if (test(head)) next() + next(test) else ""
   }
   
   import scala.util.matching.Regex
   /** matches at the beginning of the stream and returns the matched prefix */
   def next(regex: String): Regex.Match = {
      val r = new Regex(regex)
      val mt = r.findPrefixMatchOf(remainder).getOrElse {
         error(s"expected match of $regex, found " + remainder.subSequence(0,200))
      }
      drop(mt.matched)
      mt
   }
   
   /**
    * returns all characters up to the next unescaped occurrence of 'until' (that occurrence is eaten but not returned)
    * @param until the delimiter to scan for
    * @param exceptAfter the an escape character
    * @return the found string (excluding the until), and false iff end of input reached  
    */
   def next(until: Char, exceptAfter: Char): (String,Boolean) = {
      var seen = ""
      while (!empty && head != until) {
         if (head == exceptAfter) {
            seen += head
			      next
         }
         seen += head
         next
      }
      if (empty) {
         (seen, false)
      }
      else {
         next
         (seen, true)
      }
   }
   
   //def nextSkipNestedBrackets(until: String, brackets: Brackets): (String, Boolean) 
}