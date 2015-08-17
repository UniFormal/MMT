package info.kwarc.mmt.jedit

import errorlist._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils.File._
import org.gjt.sp.jedit._
import org.gjt.sp.jedit.msg._
import org.gjt.sp.jedit.textarea._


/**
 * The main class of the MMTPlugin
 * after initialization, it creates a Controller and executes home/startup.mmt
 * logging information is sent to home/mmtplugin.log
 * the home directory is obtained from jEdit, e.g., settings/plugins/info.kwarc.mmt.jedit.MMTPlugin
 */
class MMTPlugin extends EBPlugin with Logger {
   val controller = new Controller
   val report = controller.report
   val logPrefix = "jedit"
   val errorSource = new DefaultErrorSource("MMT")

   val compileActions = new CompileActions(this)

   /** implements onNavigate hook in terms of the methods of MMTHyperlink */
   val mmtListener = new ChangeListener {
      override def onNavigate(p: Path) {
         log("navigating to " + p)
         val ref = controller.globalLookup.getO(p) flatMap {
            e => MMTHyperlink.elemToSourceRef(controller, e)
         }
         ref foreach {r =>
            val view = jEdit.getActiveView
            MMTHyperlink.navigateTo(view, r)
         }
      }
   }

   /** called by jEdit when plugin is loaded */
   override def start {
      val home = getPluginHome
      home.mkdirs
      controller.setHome(home)
      val startup = MMTOptions.startup.get.getOrElse("startup.msl")
      val file = home resolve startup
      controller.handle(ExecFile(file, None))
      errorlist.ErrorSource.registerErrorSource(errorSource)
      val archives = MMTOptions.archives.get.getOrElse("mars")
      controller.handle(AddArchive(home resolve archives))
      // add this only after executing the startup file because the status bar is not available yet
      // this command itself may also not be logged
      controller.report.addHandler(StatusBarLogger)
      controller.extman.addExtension(mmtListener)
      // make tooltips stay longer
      javax.swing.ToolTipManager.sharedInstance().setDismissDelay(100000)
   }
   /** called by jEdit when plugin is unloaded */
   override def stop {
      controller.cleanup
      errorlist.ErrorSource.unregisterErrorSource(errorSource)
   }

   private def customizeEditPane(editPane: EditPane) {
       val ta = editPane.getTextArea
       val painter = ta.getPainter
       val sc = new StyleChanger(editPane, "mmt")
       val taExt = new MMTTextAreaExtension(controller, editPane)
       painter.addExtension(TextAreaPainter.DEFAULT_LAYER, taExt)
       //painter.addExtension(TextAreaPainter.DEFAULT_LAYER, sc)
       val ma = new MMTMouseAdapter(editPane)
       painter.addMouseListener(ma)

       /* old code used for cleaning up - seems unnecessary
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
                 view.getEditPanes foreach customizeEditPane
              case _ =>
           }
        case _ =>
      }
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
  def apply(ind: Int, caller: => String, group: String, msg: List[String]) {
     val v = jEdit.getActiveView
     if (v != null) {
        v.getStatus.setMessage("MMT " + group + ": " + msg.headOption.getOrElse(""))
     }
  }
}