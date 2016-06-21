package info.kwarc.mmt.jedit

import info.kwarc.mmt.api.parser._

import org.gjt.sp.jedit._
import textarea._

object Inserter {
   def insert(ta: TextArea, ifmmt: String, ifother: String) {
      val insert = if (ta.getBuffer.getMode.getName == "mmt") ifmmt else ifother
      ta.replaceSelection(insert)
   }
   def insertUSorTab(ta: TextArea) {
      val offset = ta.getCaretPosition
      insert(ta, Reader.US.toChar.toString, "\t")
   }
   def insertRSReturn(ta: TextArea) {
      insert(ta, Reader.RS.toChar.toString + "\n", "\n")
   }
   def insertGSReturn(ta: TextArea) {
      insert(ta, Reader.GS.toChar.toString + "\n", "\n")
   }
}
