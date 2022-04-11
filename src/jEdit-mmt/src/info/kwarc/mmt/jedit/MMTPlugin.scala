package info.kwarc.mmt.jedit

import java.awt.Font
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.utils.File._
import org.gjt.sp.jedit._
import org.gjt.sp.jedit.msg._
import org.gjt.sp.jedit.textarea._

import javax.swing.SwingUtilities

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
  val errorSource = new MMTErrorSource

  /** these are not used in the code, but called by actions in actions.xml */
  val buildActions = new BuildActions(this)
  val editActions = new EditActions(this)
  val debugActions = new DebugActions(this)

  /** set by [[MMTSideKick]], read by [[MMTGutterExtension]] */
  val progressTracker = new scala.collection.mutable.ListMap[Buffer,MMTTask]
  
  /** convenience */
  def asString(o: objects.Obj) = controller.presenter.asString(o)
   
   /** implements onNavigate hook in terms of the methods of MMTHyperlink */
   val mmtListener = new ChangeListener {
      override def logPrefix = "jedit-navigate"
      override def onNavigate(p: Path) {
         log("navigating to " + p)
         val ref = controller.getO(p) flatMap SourceRef.get
         ref foreach navigateTo
      }
     override def onNavigateSource(r: SourceRef) {
       log("navigating to " + r)
       navigateTo(r)
     }
     def navigateTo(r: SourceRef) {
       val rPO = MMTHyperlink.makeSourceRefPhysical(controller, r)
       rPO match {
         case Some(rP) =>
           val view = jEdit.getActiveView
           MMTHyperlink.navigateTo(view,rP)
         case None =>
           log("no physical source ref found")
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
      if (file.exists)
         controller.runMSLFile(file, None)

      val conf = MMTOptions.config.get.getOrElse("mmtrc")
      val confFile = home resolve conf
      if (confFile.exists)
         controller.loadConfig(MMTConfig.parse(confFile), false)

      errorlist.ErrorSource.registerErrorSource(errorSource)
      val archives = MMTOptions.archives.get orElse
        controller.getMathHub.map(_.local.toString) getOrElse "mars"
      val archivesFolder = home resolve archives
      controller.addArchive(archivesFolder)
      // if no lmh root has been defined (e.g., in the custom mmtrc file), we use the archives folder
      if (controller.getMathHub.isEmpty && archives != "mars") {
        val cf = new MMTConfig
        cf.addEntry(LMHConf(archivesFolder, true, None))
        controller.loadConfig(cf,false)
      }
      // status bar is not actually available yet at this point
      controller.report.addHandler(StatusBarLogger)
      controller.extman.addExtension(mmtListener)
      jEdit.getViews foreach customizeView
      // make tooltips stay longer
      javax.swing.ToolTipManager.sharedInstance().setDismissDelay(100000)
      // tooltip font is set in handleMessage
   }
   /** called by jEdit when plugin is unloaded */
   override def stop {
      controller.cleanup
      errorlist.ErrorSource.unregisterErrorSource(errorSource)
      jEdit.getViews foreach clearMMTToolBar
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
                 customizeView(view)
                 // set tooltip font; this is only needed once and should be done in start; but it's unclear how to get the font if there is no view yet
                 val font = view.getTextArea.getPainter().getFont()
                 javax.swing.UIManager.put("ToolTip.font",font)
            case ViewUpdate.CLOSED =>
                 log("handling " + vup.paramString)
                 clearMMTToolBar(view)
            case _ =>
           }
        case bup: BufferUpdate =>
          val buffer = bup.getBuffer
          val file = utils.File(buffer.getPath)
          bup.getWhat match {
            case BufferUpdate.CLOSED =>
              errorSource.removeMMTFileErrors(file)
            case _ =>
          }
        case _ =>
      }
   }

  /** helper function that creates a thread and executes it */
  def invokeLater(code: => Unit) {
     SwingUtilities.invokeLater(
        new Runnable() {
          def run() {
            code
          }
        }
     )
  }

  private def customizeView(view: View) {
     view.getEditPanes foreach customizeEditPane
     addMMTToolBar(view)
  }
  private def customizeEditPane(editPane: EditPane) {
    val ta = editPane.getTextArea
    val painter = ta.getPainter
    if (!painter.getExtensions.exists(_.isInstanceOf[MMTTextAreaExtension])) {
      val taExt = new MMTTextHighlighting(controller, editPane)
      val tooltipExt = new MMTToolTips(controller, editPane)
      val gutterExt = new MMTGutterExtension(this, editPane)
      // val annotExt = new MMTGutterAnnotations(this, editPane) // never used, but could be reactivated
      painter.addExtension(TextAreaPainter.BELOW_MOST_EXTENSIONS_LAYER, tooltipExt) // jedit tries lower layers first when looking for a tooltip; we must be below error list
      // This only painted delimiters, which is now done by syntax highlighting, and did semantic highlighting, which never worked anyway
      painter.addExtension(TextAreaPainter.TEXT_LAYER, taExt)
      ta.getGutter.addExtension(TextAreaPainter.BELOW_MOST_EXTENSIONS_LAYER, gutterExt)
      //ta.getGutter.addExtension(TextAreaPainter.BELOW_MOST_EXTENSIONS_LAYER-1, annotExt)
      //ta.getGutter.addMouseListener(annotExt.mouseAdapter)
    }
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

  /** adds MMT toolbar */
  def addMMTToolBar(view: View) {
    invokeLater {
      clearMMTToolBar(view)
      val viewToolBar = view.getToolBar
      if (viewToolBar != null) {
        log("adding toolbar...")
        val newbar = new MMTToolBar(this)
        viewToolBar.add(newbar)
      }
    }
  }
  /** removes MMT toolbar */
  def clearMMTToolBar(view: View) {
    val viewToolBar = view.getToolBar
    if (viewToolBar != null) {
      log("Number of components of this view: " + viewToolBar.getComponentCount.toString)
      viewToolBar.getComponents foreach {comp =>
        if(comp.isInstanceOf[MMTToolBar]) {
          log("removing tool bar")
          viewToolBar.remove(comp)
        }
      }
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
   
   /** get the MMT plugin, helpful for debugging in jEdit */
   def plugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
   /** easy access to debug commands in the MMT plugin */
   def debug = plugin.debugActions
}

/** base class for MMT's text area extensions */
abstract class MMTTextAreaExtension(editPane: EditPane) extends TextAreaExtension {
  protected val gutterWidth = 12 // hard-coded in Gutter
  protected val view = editPane.getView
  protected val textArea = editPane.getTextArea
  protected val lineHeight = textArea.getPainter.getFontMetrics.getHeight
  
  /** @param y vertical position (same as passed by jEdit) */
  protected def drawMarker(gfx: java.awt.Graphics2D, color: java.awt.Color, y: Int, oval: Boolean) {
    val diameter = (lineHeight-2) min (gutterWidth-2)
	  val horiMargin = (gutterWidth-diameter)/2
	  val vertiMargin = (lineHeight-diameter)/2
	  gfx.setColor(color)
    if (oval)
      gfx.fillOval(horiMargin,y+vertiMargin,diameter,diameter)
    else
      gfx.fillRect(horiMargin,y+vertiMargin,diameter,diameter)
  }

  /** @param y vertical position (same as passed by jEdit) */
  protected def drawChar(gfx: java.awt.Graphics2D, color: java.awt.Color, y: Int, char : Char) {
    val painter = textArea.getPainter
    gfx.setColor(color)
    val horiMargin = (gutterWidth-painter.getStringWidth(char.toString))/2
    val fm = painter.getFontMetrics
    val baseLine = y + painter.getFontHeight - (fm.getLeading()) - fm.getDescent()  // taken from TextAreaPainter#PaintText
    gfx.drawString(char.toString, horiMargin, baseLine)
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
