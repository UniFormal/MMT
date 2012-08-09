package info.kwarc.mmt.jedit

import org.gjt.sp.jedit._
import textarea._

/** implements hiding-related functions */
object Hider {
    /** show all hidable parts in the current selection */
    def show(ta: TextArea) {
       val sel = ta.getSelectedText()
       if (sel == null) return
       val selChanged = sel.replace("{HIDE","{SHOW")
       ta.setSelectedText(selChanged)
    }
    /** hide all hidable parts in the current selection */
    def hide(ta: TextArea) {
       val sel = ta.getSelectedText()
       if (sel == null) return
       val selChanged = sel.replace("{SHOW","{HIDE")
       ta.setSelectedText(selChanged)
    }
}