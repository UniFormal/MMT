package info.kwarc.mmt.jedit

import org.gjt.sp.jedit._

import java.awt.event._

/** a MouseAdapter that the MMTPlugin adds to a TextAreaPainter in order to control mouse interaction */
class MMTMouseAdapter(editPane: EditPane) extends MouseAdapter {
   private val textArea = editPane.getTextArea
   override def mouseClicked(e: MouseEvent) {
      if (e.getButton() == MouseEvent.BUTTON1 && e.getClickCount == 2) {
         textArea.selectToMatchingBracket
      }
   }
}