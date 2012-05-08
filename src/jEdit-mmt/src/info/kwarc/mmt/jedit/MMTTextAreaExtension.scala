package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import frontend._

import org.gjt.sp.jedit._
import textarea._

/** A TextAreaExtension that is added to every EditPane
 *  it can be used for custom painting, e.g., background highlighting, tooltips
 *  Currently it does nothing
 */
class MMTTextAreaExtension(controller: Controller, editPane: EditPane) extends TextAreaExtension {
   private def log(msg: String) {controller.report("jedit-painter", msg)}
   private val textArea = editPane.getTextArea
   private val painter = textArea.getPainter
   override def paintValidLine(gfx: java.awt.Graphics2D, screenLine: Int, physicalLine: Int, startOffset: Int, endOffset: Int, y: Int) {
     gfx.setColor(java.awt.Color.RED)
     val height = painter.getLineHeight()
     //val startPoint = textArea.offsetToXY(startOffset)
     //val endPoint = textArea.offsetToXY(endOffset)
     gfx.fillRect(0, y, 500, height)
   }
   //def getToolTipText(xCoord: Int, yCoord: Int) {} 
}

//textArea.getPainter.addTextAreaExtension(layer, extension)


//to highlight the current expression implement this
//class MMTMatcher extends org.gjt.sp.jedit.textarea.StructureMatcher
//call editPane.getTextArea().addStructureMatcher, e.g., in sidekick parser's activate method to register it

