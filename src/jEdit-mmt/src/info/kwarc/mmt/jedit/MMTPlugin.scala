package info.kwarc.mmt.jedit
import info.kwarc.mmt.api._
import parser._

import org.gjt.sp.jedit._
import textarea._
import msg._

import errorlist._

import info.kwarc.mmt.api._
import frontend._
import utils._
import utils.FileConversion._

/**
 * The main class of the MMTPlugin
 * after initialization, it creates a Controller and executes home/startup.mmt
 * logging information is sent to home/mmtplugin.log
 * the home directory is obtained from jEdit, e.g., settings/plugins/info.kwarc.mmt.jedit.MMTPlugin
 */
class MMTPlugin extends EBPlugin {
   val controller : Controller = new Controller
   val errorSource = new DefaultErrorSource("MMT")
   
   val compileActions = new CompileActions(this)
   
   private def log(msg: String) {controller.report("jedit", msg)}
   /** called by jEdit when plugin is loaded */
   override def start() {
      val home = getPluginHome()
      home.mkdirs()
      controller.setHome(home)
      val startup = jEdit.getProperty(MMTProperty("startup"))
      if (startup != null) {
         val file = new java.io.File(home, startup)
         if (file.isFile)
            controller.reportException(controller.handle(ExecFile(file)))
      }
      //else
      //   controller.report("error", "could not find startup.mmt file")
      errorlist.ErrorSource.registerErrorSource(errorSource)
      // add this only after executing the startup file because the status bar is not available yet
      // this command itself may also not be logged
      controller.report.addHandler(StatusBarLogger)
   }
   /** called by jEdit when plugin is unloaded */
   override def stop() {
      controller.cleanup
      errorlist.ErrorSource.unregisterErrorSource(errorSource)
   }
   
   //private val taKey = "info.kwarc.mmt.jedit.MMTTextAreaExtension"
   private def customizeEditPane(editPane: EditPane) {
       val ta = editPane.getTextArea
       val painter = ta.getPainter
       val sc = new StyleChanger(editPane, "mmt")
       val taExt = new MMTTextAreaExtension(controller, editPane)
       painter.addExtension(TextAreaPainter.DEFAULT_LAYER, taExt)
       painter.addExtension(TextAreaPainter.DEFAULT_LAYER, sc)
       val ma = new MMTMouseAdapter(editPane)
       painter.addMouseListener(ma)

       /* old code used for cleaning up? seems unnecessary
         painter.putClientProperty(taKey, taExt)
          case EditPaneUpdate.DESTROYED =>
              log("handling " + epu.paramString)
              val taExt = painter.getClientProperty(taKey).asInstanceOf[TextAreaExtension]
              if (taExt != null) {
                 painter.removeExtension(taExt)
                 painter.putClientProperty(taKey, null)
              }*/
   }
   override def handleMessage(message: EBMessage) {
      message match {
        //add MMTTextAreaExtension to every newly-created TextArea
        case epu: EditPaneUpdate =>
           epu.getWhat match {
             case EditPaneUpdate.CREATED =>
                log("handling " + epu.paramString)
                val editPane = epu.getSource.asInstanceOf[EditPane]
                customizeEditPane(editPane)
             case _ =>
            }
        case vup: ViewUpdate =>
           val view = vup.getView
           vup.getWhat match {
              case ViewUpdate.CREATED =>
                 log("handling " + vup.paramString)
                 //view.getEditPanes foreach customizeEditPane
              case _ =>
           }
        case _ =>
      }
   }
   
   def showinfo(view : View) {
      val wm = view.getDockableWindowManager
      wm.addDockableWindow("mmt-dockable")
      val d = wm.getDockableWindow("mmt-dockable").asInstanceOf[MMTDockable]
      d.init
   }
}

object MMTPlugin {
   def isIDChar(c: Char) = (! Character.isWhitespace(c)) && "()[]{}:.".forall(_ != c)
   /** finds the id at a certain offset in a buffer
    * @param buffer a buffer
    * @param index offset in the buffer
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

object StatusBarLogger extends ReportHandler("jEdit") {
  def apply(ind: String, group: String, msg: String) {
     val v = jEdit.getActiveView
     if (v != null) {
        v.getStatus.setMessage("MMT " + group + ": " + msg)
     }
  }
}