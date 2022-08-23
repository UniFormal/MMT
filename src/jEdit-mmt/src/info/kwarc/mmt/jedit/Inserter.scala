package info.kwarc.mmt.jedit

import info.kwarc.mmt.api.parser._

import org.gjt.sp.jedit._
import textarea._

object Inserter {
   private def getChar(d: Reader.MMTDelim) = {
     val chars = d.chars
     val c = if (MMTOptions.lowAsciiDelims.get.getOrElse(false)) chars.last else chars.head
     c.toChar.toString
   }

   private def insert(ta: TextArea, ifmmt: String, ifother: String): Unit = {
      val insert = if (ta.getBuffer.getMode.getName == "mmt") ifmmt else ifother
      ta.replaceSelection(insert)
   }
   def insertUSorTab(ta: TextArea): Unit = {
      val offset = ta.getCaretPosition
      insert(ta, getChar(Reader.US), "\t")
   }
   def insertRSReturn(ta: TextArea): Unit = {
      insert(ta, getChar(Reader.RS) + "\n", "\n")
   }
   def insertGSReturn(ta: TextArea): Unit = {
      insert(ta, getChar(Reader.GS) + "\n", "\n")
   }
}
