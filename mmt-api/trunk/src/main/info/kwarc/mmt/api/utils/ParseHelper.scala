package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api.parser.SourcePosition

/** \n, \r, and \r\n are read as \n */
class Unparsed(s: String, error: String => Nothing) {
   private var rest: String = s
   
   /** number of characters that have been eaten */
   private var offset: Int = 0
   private var line : Int = 0
   private var column : Int = 0
   private def advancePositionBy(c: Char) {
      offset += 1
      if (c == '\n') {
         line += 1
         column = 0
      }
   }
   /** the position of the next character to be read */
   def getSourcePosition = SourcePosition(offset, line, column)

   def remainder = rest
   def head = rest(0)
   def next() = {
      if (rest.isEmpty) error("expected character, found nothing")
      var c = head
      rest = rest.substring(1)
      if (c == '\r') {
         if (remainder.nonEmpty && head == '\n') {
            rest = rest.substring(1)
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
      while (!remainder.isEmpty && head.isWhitespace) next()
      this
   }
   def drop(s: String) {
      if (rest.startsWith(s)) {
         rest = rest.substring(s.length)
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
      val mt = m.findPrefixMatchOf(rest).getOrElse {
         error(s"expected match of $regex, found $rest")
      }
      drop(mt.matched)
      mt
   }
   
   /**
    * returns all characters up to the next unescaped occurrent of 'until' (that occurrence is eaten but not returned)
    * @param until the delimiter to scan for
    * @param exceptAfter the an escape character  
    * @param unescape maps a string after the escape character to the eaten escape sequence and its unescaped value
    */
   def next(until: Char, exceptAfter: Char)(unescape: String => (String,String)): String = {
      var seen = ""
      while (head != until) {
         if (head == exceptAfter) {
            next
            val (eaten, unescaped) = unescape(rest)
            drop(eaten)
            seen += unescaped
         } else {
            seen += head
            next
         }
      }
      next
      seen
   }
}