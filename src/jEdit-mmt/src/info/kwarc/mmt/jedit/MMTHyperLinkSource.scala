package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import org.gjt.sp.jedit._
import sidekick._
import gatchan.jedit.hyperlinks._

class MyHyperlink(start: Int, end: Int, startLine: Int, path: Path, ref: SourceRef) extends AbstractHyperlink(start, end, startLine, path.toPath) {
   def click(view: View) {
      val buffer = jEdit.getBuffer(ref.container.pathAsString)
      val editPane = view.goToBuffer(buffer)
      editPane.getTextArea.moveCaretPosition(ref.region.start.offset)
   }
}

class MMTHyperlinkSource extends HyperlinkSource {
   val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
   val controller = mmt.controller
   private var currentLink : Hyperlink = null
   def getHyperlink(buffer: Buffer, caretPosition: Int) : Hyperlink = {
      // return a previously found link if it is still applicable
      if (currentLink != null && currentLink.getStartOffset() <= caretPosition && currentLink.getEndOffset() >= caretPosition)
         return currentLink
      currentLink = MMTPlugin.getCurrentID(buffer, caretPosition) match {
         case None => null
         case Some((line, begin, end, id)) => 
            val asset = SideKickParsedData.getParsedData(jEdit.getActiveView).getAssetAtOffset(caretPosition).asInstanceOf[MMTAsset]
            asset.getScope match {
               case None => null
               case Some(objects.OMMOD(p)) =>
                  val e = controller.get(p ? id)
                  MMTPlugin.getSourceRef(e) match {
                     case None => null
                     case Some(r) => new MyHyperlink(begin, end, line, p ? id, r)
                  }
            }
      }
      currentLink
   }
}

