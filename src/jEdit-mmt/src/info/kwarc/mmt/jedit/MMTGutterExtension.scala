package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._

import org.gjt.sp.jedit._
import org.gjt.sp.jedit.textarea._

class MMTGutterExtension(mmt: MMTPlugin, editPane: EditPane) extends MMTTextAreaExtension(editPane) {
  
  override def paintValidLine(gfx: java.awt.Graphics2D, screenLine: Int, physicalLine: Int, startOffset: Int, endOffset: Int, y: Int): Unit = {
    val task = mmt.progressTracker.get(editPane.getBuffer).getOrElse(return)
    val lastReport = task.getReports.headOption match {
      case Some(r: MMTInterpretationProgress) => r
      case _ => return
    }
    if (! (lastReport.sourceLine contains physicalLine)) return
	  val color = lastReport match {
      case r: Parsed => java.awt.Color.RED
      case r: Checked => java.awt.Color.YELLOW
      case r: Elaborated => java.awt.Color.GREEN
      case _ => return
    }
    drawMarker(gfx, java.awt.Color.YELLOW, y, task.isKilled)
  }
  
  override def getToolTipText(x: Int, y: Int): String = {
		if (!editPane.getBuffer.isLoaded)
			 return null
    val task = mmt.progressTracker.get(editPane.getBuffer).getOrElse(return null)
    val lastReport = task.getReports.headOption match {
      case Some(r: MMTInterpretationProgress) => r
      case _ => return null
    }
    val textArea = editPane.getTextArea
		val offset = textArea.xyToOffset(x,y)
		if (offset == -1)
			return null
    val line = textArea.getLineOfOffset(offset)
    if (! (lastReport.sourceLine contains line)) return null
    lastReport match {
      case r: Parsed => "finished parsing " + r.element.path
      case r: Checked => "finished checking " + r.element.path
      case r: Elaborated => "finished elaborating " + r.element.path
      case _ => null
    }
  }
}