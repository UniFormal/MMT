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
 */
// this is meant to be used in a future reimplementation of the TextReader
class Reader(jr: java.io.BufferedReader) {
   import Reader._
   
   private var lastDelimiter: Int = 65536 //invalid Char
   
   def endOfDocument = lastDelimiter <= FS
   def endOfModule = lastDelimiter <= GS
   def endOfDeclaration = lastDelimiter <= RS
   def endOfObject = lastDelimiter <= US 

   //current position
   private var line : Int = 0
   private var column : Int = 0
   private var offset : Int = 0
   private var sourcePosition: SourcePosition = null
   def getSourcePosition = SourcePosition(offset, line, column)
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
      if (! c.toChar.isWhitespace)
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
      } while (! List(-1,US,RS,GS,FS).contains(c) && c.toChar.isWhitespace)
      c
   }
   
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
       (buffer.result.trim, SourceRegion(start, end))
   }
   /** reads until end of current document
    */
   def readDocument = readUntil(FS)
   /** reads until end of current module
    */
   def readModule = readUntil(GS,FS)
   /** reads until end of current declaration
    */
   def readDeclaration = readUntil(RS,GS,FS)
   /** reads until end of current object
    */
   def readObject = readUntil(US,RS,GS,FS)
   /** reads until end of current whitespace-separated token
    */
   def readToken = readUntil(32,US,RS,GS,FS)
   
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
   //ascii codes
   //Note: 28-31 have isWhitespace == true
   val FS = 28
   val GS = 29
   val RS = 30
   val US = 31
   val escape = 145 //PU1
   val unescape = 146 //PU2
}