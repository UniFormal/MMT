package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api.parser.SourcePosition

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

   def remainder = input.substring(current)
   def head = input(current)
   def next() = {
      if (empty) error("expected character, found nothing")
      var c = head
      rest += 1
      if (c == '\r') {
         if (!empty && head == '\n') {
            rest += 1
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
      if (rest.startsWith(s)) {
         current += s.length
         s.foreach {c => advancePositionBy(c)}
      } else
         error(s"expected $s, found $rest")
   }
   def next(test: Char => Boolean): String = {
      if (test(head)) next() + next(test) else ""
   }
   
   import scala.util.matching.Regex
   /** matches at the beginning of the stream and returns the matched prefix */
   def next(regex: String): Regex.Match = {
      val m = new Regex(regex)
      val mt = m.findPrefixMatchOf(remainder).getOrElse {
         error(s"expected match of $regex, found $rest")
      }
      drop(mt.matched)
      mt
   }
   
   /**
    * returns all characters up to the next unescaped occurrence of 'until' (that occurrence is eaten but not returned)
    * @param until the delimiter to scan for
    * @param exceptAfter the an escape character  
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
}