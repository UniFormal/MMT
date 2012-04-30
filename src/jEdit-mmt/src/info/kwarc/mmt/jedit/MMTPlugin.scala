package info.kwarc.mmt.jedit
import info.kwarc.mmt.api._
import parser._

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
      controller.setHome(home)
      val startup = new java.io.File(home, "startup.mmt")
      if (startup.isFile())
         controller.handle(ExecFile(startup))
      //else
      //   controller.report("error", "could not find startup.mmt file")
   }
   /** called by jEdit when plugin is unloaded */
   override def stop() {
      controller.cleanup
   }
   
   def showinfo(view : View) {
      val wm = view.getDockableWindowManager
      wm.addDockableWindow("mmt-dockable")
      val d = wm.getDockableWindow("mmt-dockable").asInstanceOf[MMTDockable]
      val ta = view.getTextArea
      MMTPlugin.getCurrentID(view.getBuffer, ta.getCaretPosition) match {
         case None =>
         case Some((_,_,_,id)) => d.setText(id)
      }
   }
}

object MMTPlugin {
   def isIDChar(c: Char) = (! Character.isWhitespace(c)) && "()[]{}:.".forall(_ != c)
   def getSourceRef(e: metadata.HasMetaData) : Option[SourceRef] = e.metadata.getLink(SourceRef.metaDataKey) match {
      case u :: _ => Some(SourceRef.fromURI(u))
      case Nil => None
   }
   /** finds the id at a certain offset in a buffer
    * @param buffer a buffer
    * @parem index offset in the buffer
    * @return the id at the offset, if any, as (line,beginOffsetInBuffer,endOffsetInBuffer,id)
    */ 
   def getCurrentID(buffer: Buffer, index: Int) : Option[(Int,Int,Int,String)] = {
      val line = buffer.getLineOfOffset(index)
      val lineStart = buffer.getLineStartOffset(line)
      val lineLength = buffer.getLineLength(line)
      if (lineLength == 0)
         return None
      var offset = index - lineStart
      val lineText = buffer.getLineText(line)
      if (offset == lineLength)
         offset = offset - 1
      if (! isIDChar(lineText(offset)))
         return None
      var left = offset // first character of id
      while (left > 0 && isIDChar(lineText(left - 1))) {left = left - 1}
      var right = offset // last character of id
      while (right < lineLength - 1 && isIDChar(lineText(right + 1))) {right = right + 1}
      Some((line, lineStart + left, lineStart + right + 1, lineText.substring(left, right + 1)))
   }
}