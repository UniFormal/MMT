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
   
   /**
    * true if the last read operation hit the end of the current document
    */
   def endOfDocument = lastDelimiter <= FS
   /**
    * true if the last read operation hit the end of the current module
    */
   def endOfModule = lastDelimiter <= GS
   /**
    * true if the last read operation hit the end of the current declaration
    */
   def endOfDeclaration = lastDelimiter <= RS
   /**
    * true if the last read operation hit the end of the current object
    */
   def endOfObject = lastDelimiter <= US 

   //current position (offset counts \r\n as 1 character)
   private var line : Int = 0
   private var column : Int = 0
   private var offset : Int = 0
   private var sourcePosition: SourcePosition = null

   /** @return the position of the next character to be read
    */
   def getSourcePosition = SourcePosition(offset, line, column)
   /**
    * sets the current position
    * @param s a source position to overwrite the position of the next character to be read
    * This only affects the SourceRegion returned by read operations,
    * not the actual position in the stream.  
    */
   def setSourcePosition(s: SourcePosition) {
      line = s.line
      column = s.column
      offset = s.offset
   }
   private var ignoreLineFeed = false
   /** read one character
    * \n, \r, and \r\n are read as \n
    */
   private def read: Int = {
      var c = jr.read
      if (c == -1)
         return c
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
   /** as read but skips initial whitespace
    */
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
   private def readUntil(goal: Int*): (String, SourceRegion) = {
       val buffer = new StringBuilder
       var level = 0
       var continue = true
       var c: Int = readSkipWS
       val start = sourcePosition
       while (continue) {
          if (c == -1 || (goal.contains(c) && level == 0)) {
             continue = false
          } else {
             buffer.append(c.toChar)
             if (c == escape)
                level += 1
             else if (c == unescape)
                level -= 1
             c = read
          }
       }
       if (level != 0)
          {} //unbalanced escape
       lastDelimiter = c
       val end = sourcePosition
       var res = buffer.result
       // remove trailing whitespace
       while (res != "" && whitespace(res.last))
          res = res.substring(0, res.length - 1)
       (res, SourceRegion(start, end))
   }
   /** reads until end of current document, terminated by the ASCII character FS (decimal 28)
    */
   def readDocument = readUntil(FS)
   /** reads until end of current module, terminated by the ASCII characters GS or FS (decimal 28-29)
    */
   def readModule = readUntil(GS,FS)
   /** reads until end of current declaration, terminated by the ASCII character RS, GS, or FS (decimal 28-30)
    */
   def readDeclaration = readUntil(RS,GS,FS)
   /** reads until end of current object, terminated by the ASCII character RS, GS, FS, or US (decimal 28-31)
    */
   def readObject = readUntil(US,RS,GS,FS)
   /** reads until end of current Token, terminated by whitespace
    */
   def readToken = readUntil(32,US,RS,GS,FS)
   /** reads until EOF
    */
   def readAll = readUntil()
   
   /** closes the underlying Reader */
   def close {
      jr.close
   }
}

/** helper object
 */
object Reader {
   def apply(file: File) = new Reader(File.Reader(file))
   def apply(s: String) = new Reader(new java.io.BufferedReader(new java.io.StringReader(s)))
   //Note: 28-31 have isWhitespace == true
   def whitespace(c:Int) = ! List(-1,US,RS,GS,FS,escape,unescape).contains(c) && c.toChar.isWhitespace
   /** the ASCII character FS (decimal 28) ends MMT documents */
   val FS = 28
   /** the ASCII character GS (decimal 29) ends MMT modules */
   val GS = 29
   /** the ASCII character GS (decimal 30) ends MMT declaration within modules */
   val RS = 30
   /** the ASCII character GS (decimal 31) ends MMT objects */
   val US = 31
   /** the ASCII character ESC (decimal 27) begins escaped parts */
   val escape = 27
   val escapeChar = '\u001b'
   /** the ASCII character CAN (decimal 24 ends escaped parts */
   val unescape = 24
   val unescapeChar = '\u0018'
}