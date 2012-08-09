package info.kwarc.mmt.jedit

import org.gjt.sp.jedit._

import java.awt.event._

class MMTMouseAdapter(editPane: EditPane) extends MouseAdapter {
   override def mouseClicked(e: MouseEvent) {
      if (e.getButton() == MouseEvent.BUTTON1 && e.getClickCount == 2) {
         editPane.getTextArea.selectToMatchingBracket
      }
   }
}