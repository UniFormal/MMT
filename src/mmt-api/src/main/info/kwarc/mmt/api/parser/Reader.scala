package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import frontend._
import utils._

/** a Java-style reader that provides MMT-specific read methods
 * @param jr the underlying Java reader
 *
 * MMT documents are delimited by the control characters
 * FS, GS, RS, US (ASCII decimal 28-31) for end of document, module, declaration, object, respectively
 *
 * PU1 and PU2 (ASCII decimal 145, 146) are used to escape in/out of other formats
 * content between balanced PU1 and PU2 is skipped
 *
 */
// This class generally uses integers (0 to 65535 and -1 for end-of-file) to represent characters.
class Reader(val jr: java.io.BufferedReader) {
   import Reader._

   /** the delimiter that terminated the previous read operation */
   private var lastDelimiter: Int = 65536 // initialized as invalid Char
   //TODO remove
   @MMT_TODO("needs review")
   def forceLastDelimiter(i : Int) = lastDelimiter = i
   /**
    * true if the last read operation hit the end of the current input stream
    */
   def endOfInput = -1 == lastDelimiter
   /**
    * true if the last read operation hit the end of the current document
    */
   def endOfDocument = -1::FS.andabove contains lastDelimiter
   /**
    * true if the last read operation hit the end of the current module
    */
   def endOfModule = -1::GS.andabove contains lastDelimiter
   /**
    * true if the last read operation hit the end of the current declaration
    */
   def endOfDeclaration = -1::RS.andabove contains lastDelimiter
   /**
    * true if the last read operation hit the end of the current object
    */
   def endOfObject = -1::US.andabove contains lastDelimiter

   //current position (offset counts \r\n as 1 character)
   private var line : Int = 0
   private var column : Int = 0
   private var offset : Int = 0
   /** most recent non-whitespace character that was read */
   private var sourcePosition: SourcePosition = null

   /** @return the position of the next character to be read */
   def getNextSourcePosition = SourcePosition(offset, line, column)
   /** @return the position of the most recently read non-whitespace character */
   def getLastReadSourcePosition = Option(sourcePosition).getOrElse(getNextSourcePosition) // null only at beginning
   
   /**
    * sets the current position
    * @param s a source position to overwrite the position of the next character to be read
    * This only affects the SourceRegion returned by read operations,
    * not the actual position in the stream.
    */
   def setNextSourcePosition(s: SourcePosition): Unit = {
      line = s.line
      column = s.column
      offset = s.offset
   }
   private var ignoreLineFeed = false
   /** reads the next n characters without advancing the read position */
   private def lookAhead(n: Int): String = {
      jr.mark(n)
      var s = ""
      Range(0,n).foreach {_ =>
         val c = jr.read
         if (c != -1) s += c.toChar
      }
      jr.reset
      s
   }

   /** checks if the remainder starts with a certain String */
   def startsWith(s: String): Boolean = {
     val r = lookAhead(s.length)
     r == s
   }

   /** read one character
    * \n, \r, and \r\n are read as \n
    */
   private def read: Int = {
      var c = jr.read
      if (c == -1) {
        return c
      }
      if (ignoreLineFeed && c == '\n') {
         c = jr.read
      }
      ignoreLineFeed = false
      if (! whitespace(c))
         sourcePosition = SourcePosition(offset, line, column)
      if (c == '\n' || c == '\r') {
          line += 1
          column = 0
          ignoreLineFeed = (c == '\r')
          c = '\n'
       } else {
          column += 1
       }
       offset += 1
       c
   }

   /** as read but skips initial whitespace */
   private def readSkipWS: Int = {
      var c:Int = 0
      do {
         c = read
      } while (whitespace(c))
      c
   }

   /**
    * read everything up to and including the next delimiter
    * @param goal the delimiters to consider
    * @return the read string and its SourceRegion, both excluding the delimiter
    * This respects escaping and skips delimiters occurring within well-balanced escapes.
    */
   /*TODO it seems this erroneously includes the terminal delimiter.
     Presumably, this is harmless for non-empty objects because more constrained regions are computed anyway by the ObjectParser*/
   private def readUntil(goal: Int*): (String, SourceRegion) = {
       val buffer = new StringBuilder
       var continue = true
       var c: Int = readSkipWS
       val start = sourcePosition
       while (continue) {
          if (c == -1 || goal.contains(c)) {
             continue = false
          } else {
             buffer.append(c.toChar)
             c = read
          }
       }
       lastDelimiter = c
      var res = buffer.result

       val end = { // there's some weird error here
          val diff = (sourcePosition.offset-start.offset)- res.length
          if (diff == 0) sourcePosition else sourcePosition-diff
       }

       // remove trailing whitespace
       while (res != "" && whitespace(res.last)) {
          res = res.dropRight(1)
       }
       (res, SourceRegion(start, end))
   }
   /** reads until end of current document, terminated by the ASCII character FS (decimal 28)
    */
   def readDocument = readUntil(FS.andabove:_*)
   /** reads until end of current module, terminated by the ASCII characters GS or FS (decimal 28-29)
    */
   def readModule = readUntil(GS.andabove:_*)
   /** reads until end of current declaration, terminated by the ASCII character RS, GS, or FS (decimal 28-30)
    */
   def readDeclaration = readUntil(RS.andabove:_*)
   /** reads until end of current object, terminated by the ASCII character RS, GS, FS, or US (decimal 28-31)
    */
   def readObject = readUntil(US.andabove:_*)
   /** reads until end of current Token, terminated by whitespace
    */
   def readToSpace = readUntil(32::US.andabove:_*)

   /** reads until end of current Token, terminated by whitespace or by switch from letter-like to symbol-like characters
    */
   def readToken = {
      var s = ""
      var i = readSkipWS
      val start = sourcePosition
      var stop = false
      if ((-1 :: Reader.delims.map(_.toInt)).contains(i)) {
         stop = true
         lastDelimiter = i
      }
      while (! stop) {
         s += i.toChar
         val l = lookAhead(1)
         if (l == "") {
            stop = true
            lastDelimiter = -1
         } else if (! TokenList.canFollow(i.toChar, l(0)) && l(0) != '/') {
            stop = true
            val j = l(0).toInt
            lastDelimiter = 32
         } else {
            i = read
         }
      }
      val end = sourcePosition
      (s, SourceRegion(start, end))
   }
   /** reads until EOF */
   def readAll = readUntil()

   /** closes the underlying Java reader */
   def close: Unit = {
      jr.close
   }
}

/** helper object
 */
object Reader {
   def apply(file: File) = new Reader(File.Reader(file))
   def apply(s: String) = new Reader(new java.io.BufferedReader(new java.io.StringReader(s)))
   //Note: 28-31 have isWhitespace == true
   def whitespace(c:Int) = !(-1::delims).contains(c) && c.toChar.isWhitespace
   abstract class MMTDelim {
      def chars : List[Int]
      def is(a : Int) = chars contains a
      def andabove : List[Int]
      def toChar = chars.last.toChar
   }
   /** the ASCII character FS (decimal 28) ends MMT documents */
   object FS extends MMTDelim {
      val chars = List(28)
      def andabove = chars
   }
   /** the ASCII character GS (decimal 29) ends MMT modules */
   object GS extends MMTDelim {
      val chars = List('\u275A'.toInt,29)
      def andabove = chars ::: FS.chars
   }
   /** the ASCII character GS (decimal 30) ends MMT declaration within modules */
   object RS extends MMTDelim {
      val chars = List('\u2759'.toInt,30)
      def andabove = chars ::: GS.andabove
   }
   /** the ASCII character GS (decimal 31) ends MMT objects */
   object US extends MMTDelim {
      val chars = List('\u2758'.toInt,31)
      def andabove = chars ::: RS.andabove
   }
   /** the special delimiters */
   def delims: List[Char] = US.andabove.map(_.toChar)
}
