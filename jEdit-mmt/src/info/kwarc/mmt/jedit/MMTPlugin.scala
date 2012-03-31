package info.kwarc.mmt.jedit
import org.gjt.sp.jedit._
import org.gjt.sp.jedit.textarea._

import info.kwarc.mmt.api._
import frontend._
import libraries._
import utils.FileConversion._

/**
 * The main class of the MMTPlugin
 * after initialization, it creates a Controller and executes home/startup.mmt
 * logging information is sent to home/mmtplugin.log
 * the home directory is obtained from jEdit, e.g., settings/plugins/info.kwarc.mmt.jedit.MMTPlugin
 */
class MMTPlugin extends EditPlugin {
   val controller : Controller = new Controller
   private def log(msg: String) {controller.report("jEdit", msg)}
   /** called by jEdit when plugin is loaded */
   override def start() {
      val home = getPluginHome()
      home.mkdirs()
      controller.setFileReport(home / "mmtplugin.log")
      controller.setHome(home)
      controller.setCheckStructural
      val startup = new java.io.File(home, "startup.mmt")
      if (startup.isFile())
         controller.handle(ExecFile(startup))
      else
         controller.report("error", "could not find startup.mmt file")
   }
   /** called by jEdit when plugin is unloaded */
   override def stop() {
      controller.cleanup
   }
   
   def read(view : View) {
   }
}

object MMTPlugin {
   def isIDChar(c: Char) = (! Character.isWhitespace(c)) && "()[]{}:.".forall(_ != c)
   def getSourceRef(e: metadata.HasMetaData) : Option[SourceRef] = e.metadata.getLink(SourceRef.metaDataKey) match {
         case u :: _ => Some(SourceRef.fromURI(u))
         case Nil => None
   }
   def getCurrentID(buffer: Buffer, caretPosition: Int) : Option[(Int,Int,Int,String)] = {
      val line = buffer.getLineOfOffset(caretPosition)
      val lineStart = buffer.getLineStartOffset(line)
      val lineLength = buffer.getLineLength(line)
      if (lineLength == 0)
         return None
      var offset = caretPosition - lineStart
      val lineText = buffer.getLineText(line)
      if (offset == lineLength)
         offset = offset - 1
      var left = 0 // number of character to the left of the caret that are id characters
      while (left < offset && isIDChar(lineText(offset - left - 1))) {left = left + 1}
      var right = 0 // number of character to the right of the caret that are id characters
      val rMax = lineText.length - offset
      while (right < rMax && isIDChar(lineText(offset + right))) {right = right + 1}
      if (left + right == 0) None
      else Some((line, caretPosition - left, caretPosition + right, lineText.substring(offset - left, left + right)))
   }
}