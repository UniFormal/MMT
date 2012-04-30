package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import parser._

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
   private def log(msg : => String) {controller.report("jedit-link", msg)}
   private var currentLink : Hyperlink = null
   def getHyperlink(buffer: Buffer, caretPosition: Int) : Hyperlink = try {
      // return a previously found link if it is still applicable
      if (currentLink != null && currentLink.getStartOffset() <= caretPosition && currentLink.getEndOffset() >= caretPosition)
         return currentLink
      currentLink = MMTPlugin.getCurrentID(buffer, caretPosition) match {
         case None => null
         case Some((line, begin, end, id)) =>
            log(id)
            val asset = SideKickParsedData.getParsedData(jEdit.getActiveView).getAssetAtOffset(caretPosition).asInstanceOf[MMTAsset]
            asset.getScope match {
               case None => null
               case Some(home) => libraries.Names.resolve(home, id)(controller.globalLookup) match {
                  case None => null
                  case Some(elem) => MMTPlugin.getSourceRef(elem) match {
                    case None => null
                    case Some(ref) =>
                       log(elem.path.toString)
                       new MyHyperlink(begin, end, line, elem.path, ref)
                  }
               }
            }
      }
      currentLink
   } catch {
     case e: Error => controller.report(e); null
     case e => log(e.getMessage); null
   }
}
