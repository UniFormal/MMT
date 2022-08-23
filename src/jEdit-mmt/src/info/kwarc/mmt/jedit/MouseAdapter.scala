package info.kwarc.mmt.jedit

import org.gjt.sp.jedit._
import textarea._
import java.awt.event._

import info.kwarc.mmt.api.gui.MMTObjAsset

/** a MouseAdapter that the MMTPlugin adds to a TextAreaPainter in order to control mouse interaction */
class MMTMouseAdapter(editPane: EditPane) extends MouseAdapter {
   private val textArea = editPane.getTextArea
   private val view = editPane.getView
   override def mouseClicked(e: MouseEvent): Unit = {
      if (e.getButton() == MouseEvent.BUTTON1 && e.getClickCount == 2) {
         val as = MMTSideKick.getAssetAtOffset(view, textArea.getCaretPosition)
         as match {
            case Some(ma: JObjAsset) =>
               textArea.setSelection(ma.toSelection)
               //textArea.selectToMatchingBracket
            case _ =>
         }
      }
   }
}
