package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._

import org.gjt.sp.jedit._
import org.gjt.sp.jedit.textarea._

class MMTGutterExtension(mmt: MMTPlugin, editPane: EditPane) extends TextAreaExtension {
  private val gutterWidth = 12 // hard-coded in Gutter
  
  override def paintValidLine(gfx: java.awt.Graphics2D, screenLine: Int, physicalLine: Int, startOffset: Int, endOffset: Int, y: Int) {
    val task = mmt.progressTracker.get(editPane.getBuffer).getOrElse(return)
    val lastReport = task.getReports.headOption match {
      case Some(r: MMTInterpretationProgress) => r
      case _ => return
    }
    if (! (lastReport.sourceLine contains physicalLine)) return
	  val textArea = editPane.getTextArea
	  val lineHeight = textArea.getPainter.getFontMetrics.getHeight
	  val diameter = (lineHeight-2) min (gutterWidth-2)
	  val horiMargin = (gutterWidth-diameter)/2
	  val vertiMargin = (lineHeight-diameter)/2
	  val color = lastReport match {
      case r: Parsed => java.awt.Color.RED
      case r: Checked => java.awt.Color.YELLOW
      case r: Elaborated => java.awt.Color.GREEN
      case _ => return
    }
	  gfx.setColor(color)
	  if (task.isKilled) {
	    gfx.fillRect(horiMargin,y+vertiMargin,diameter,diameter)
    } else {
      gfx.fillOval(horiMargin,y+vertiMargin,diameter,diameter)
    }
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