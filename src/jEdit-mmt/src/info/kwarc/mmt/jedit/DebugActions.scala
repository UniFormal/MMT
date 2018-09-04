package info.kwarc.mmt.jedit

import org.gjt.sp.jedit._

class DebugActions(mmt: MMTPlugin) {
   def getAssetAtOffset(view: View) = {
     val ta = view.getTextArea
     val offset = ta.getCaretPosition
     val aO = MMTSideKick.getAssetAtOffset(view, offset)
     aO match {
       case None => "None, offset is " + offset
       case Some(a) =>
        ta.setSelection(a.toSelection)
        a match {
          case el: MMTElemAsset =>
            mmt.controller.presenter.asString(el.elem)
          case oa: MMTObjAsset =>
            mmt.controller.presenter.asString(oa.obj)
          case a => a.toString
        }
     }
   }
}