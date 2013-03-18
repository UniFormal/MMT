package info.kwarc.mmt.jedit

import org.gjt.sp.jedit._
import textarea._
import java.awt.event._

/** a MouseAdapter that the MMTPlugin adds to a TextAreaPainter in order to control mouse interaction */
class MMTMouseAdapter(editPane: EditPane) extends MouseAdapter {
   private val textArea = editPane.getTextArea
   private val view = editPane.getView
   override def mouseClicked(e: MouseEvent) {
      if (e.getButton() == MouseEvent.BUTTON1 && e.getClickCount == 2) {
         val as = MMTSideKick.getAssetAtOffset(view, textArea.getCaretPosition)
         val sel = new Selection.Range(as.region.start.offset, as.region.end.offset)
         textArea.setSelection(sel)
         //textArea.selectToMatchingBracket
      }
   }
}