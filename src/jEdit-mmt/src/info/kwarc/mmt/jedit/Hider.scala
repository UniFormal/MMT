package info.kwarc.mmt.jedit

import org.gjt.sp.jedit._
import textarea._

/** implements hiding-related functions */
object Hider {
    val hideStart = "%{HIDE"
    val hideEnd = "}%"
    val showStart = "%{SHOW"
    val showEnd = "}%"
    /** show all hidable parts in the current selection */
    def show(ta: TextArea) {
       if (ta.getSelectionCount() != 1) return
       val sel = ta.getSelectedText()
       val selChanged = sel.replace("{HIDE","{SHOW")
       ta.setSelectedText(selChanged)
    }
    /** hide all hidable parts in the current selection */
    def hide(ta: TextArea) {
       if (ta.getSelectionCount() != 1) return
       val sel = ta.getSelectedText()
       val selChanged = sel.replace("{SHOW","{HIDE")
       ta.setSelectedText(selChanged)
    }
    /** toggle all hidable parts in the current selection */
    def hideshow(ta: TextArea) {
       if (ta.getSelectionCount() != 1) return
       var sel = ta.getSelectedText()
       sel = sel.replace("{SHOW","{HIXXXDE").replace("{HIDE", "{SHOW").replace("{HIXXXDE", "{HIDE")
       ta.setSelectedText(sel)
    }
    /** makes the current selection hidable and hides it */
    def makeHidable(ta: TextArea) {
       if (ta.getSelectionCount() != 1) return
       val sel = ta.getSelectedText()
       val selChanged = hideStart + sel + hideEnd
       ta.setSelectedText(selChanged)
    }
    /** hides the hideable part that the caret is in */
    def hideCurrent(ta: TextArea) {
       ta.selectBlock
       hide(ta)
    }
}