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
class Reader(val jr: java.io.Reader) {
   private val FS = 28
   private val GS = 29
   private val RS = 30
   private val US = 31
   private val escape = 145
   private val unescape = 146

   private var line : Int = 0
   private var column : Int = 0
   private var offset : Int = 0
   private var sourcePosition: SourcePosition = null
   private var ignoreLineFeed = false
   /** read one character
    * \n, \r, and \r\n are read as \n
    */
   def read: Int = {
      var c = jr.read
      if (c == -1)
         return c
      if (ignoreLineFeed && c == '\n') {
         c = jr.read.toChar
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
   def readSkipWS: Int = {
      var c:Int = 0
      do {
         c = read
      } while (c != -1 && c.toChar.isWhitespace)
      c
   }
   
   private def readUntil(goal: Int): Option[(String, SourceRegion)] = {
       val buffer = new StringBuilder
       var level = 0
       var continue = true
       var c: Int = readSkipWS
       val start = sourcePosition
       while (continue) {
          if (c == -1 || (c == goal && level == 0)) {
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
       if (c == -1) {
          None
       } else {
          val end = sourcePosition
          Some((buffer.result.trim, SourceRegion(start, end)))
       }
   }
   /** reads until end of current document
    */
   def readDocument = readUntil(FS)
   /** reads until end of current module
    */
   def readModule = readUntil(GS)
   /** reads until end of current declaration
    */
   def readDeclaration = readUntil(RS)
   /** reads until end of current object
    */
   def readObject = readUntil(US)
   /** reads until end of current whitespace-separated token
    */
   def readToken = readUntil(32)
}

/** helper object
 */
object Reader {
   implicit def toJavaReader(r: Reader) = r.jr
   def apply(file: File) = new Reader(File.Reader(file))
}