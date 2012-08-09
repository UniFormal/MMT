package info.kwarc.mmt.jedit

import org.gjt.sp.jedit._
import textarea._

object Hider {
    def show(ta: TextArea) {
       val sel = ta.getSelectedText()
       if (sel == null) return
       val selChanged = sel.replace("{HIDE","{SHOW")
       ta.setSelectedText(selChanged)
    }
    def hide(ta: TextArea) {
       val sel = ta.getSelectedText()
       if (sel == null) return
       val selChanged = sel.replace("{SHOW","{HIDE")
       ta.setSelectedText(selChanged)
    }
}