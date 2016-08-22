package info.kwarc.mmt.jedit

import gatchan.jedit.hyperlinks._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.source
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.utils._
import org.gjt.sp.jedit._
import sidekick._

//TODO it should be enough to compute the SourceRef at click time
/** implements the action for clicking on a hyperlink provided by [[MMTHyperlinkSource]] */
class MMTHyperlink(start: Int, end: Int, startLine: Int, path: Path, ref: Option[SourceRef])
   extends AbstractHyperlink(start, end, startLine, path.toPath) {
   def click(view: View) {
      ref foreach {r => MMTHyperlink.navigateTo(view, r)}
   }
}

object MMTHyperlink {
   /** extracts physical source references from a structural element */
   def elemToSourceRef(controller: frontend.Controller, elem: StructuralElement) : Option[SourceRef] = {
       val ref = SourceRef.get(elem)
       // we may have a ref now, but it's only useful if it's a file:URI
       ref flatMap {r =>
          val c = r.container
          if (c.scheme == Some("file")) Some(r)
          else {
             //resolve logical document id in an archive
             controller.backend.resolveLogical(c) map {
               case (archive, path) => r.copy(container = FileURI(archive / source / path))
             }
          }
       }
   }
   /** navigates to a SourceRef in a jedit view */
   def navigateTo(view: View, ref: SourceRef) {
      ref match {
        case SourceRef(FileURI(file), region) =>
           var buffer = jEdit.getBuffer(file.toString)
           if (buffer == null) {
              buffer = jEdit.openFile(view, file.toString)
              io.VFSManager.waitForRequests // wait until buffer is open
           }
           val editPane = view.goToBuffer(buffer)
           editPane.getTextArea.moveCaretPosition(region.start.offset)
        case _ =>
      }
   }
}

/** uses [[SourceRef]]s to provide hyperlinks in MMT documents */
class MMTHyperlinkSource extends HyperlinkSource {
   val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
   val controller = mmt.controller
   private def log(msg : => String) {controller.report("jedit-link", msg)}
   private var currentLink : Hyperlink = null
   def getHyperlink(buffer: Buffer, caretPosition: Int) : Hyperlink = try {
      // return a previously found link if it is still applicable
      if (currentLink != null && currentLink.getStartOffset() <= caretPosition && currentLink.getEndOffset() >= caretPosition)
         return currentLink
      val asset = SideKickParsedData.getParsedData(jEdit.getActiveView).getAssetAtOffset(caretPosition) match {
        case a: MMTAsset => a
        case _ => return null
      }
      currentLink = MMTPlugin.getCurrentID(buffer, caretPosition) match {
         case None => null
         case Some((line, begin, end, id)) =>
            log(id)
            val elemOpt: Option[StructuralElement] = asset match {
               case a: MMTObjAsset =>
                  a.obj match {
                    case t: objects.Term =>
                      a.pragmatic.head flatMap {h =>
                         controller.globalLookup.getO(h)
                      }
                    case _ => None
                  }
               case a: MMTURIAsset =>
                 controller.globalLookup.getO(a.path)
               case _ => None
                  //asset.getScope flatMap {home => libraries.Names.resolve(home, id)(controller.localLookup)}
            }
            val linkOpt = elemOpt map {elem =>
               val ref = MMTHyperlink.elemToSourceRef(controller,elem)
               log(ref.map(_.toString).getOrElse("no file reference"))
               new MMTHyperlink(begin, end, line, elem.path, ref)
            }
            linkOpt.getOrElse(null)
      }
      currentLink
   } catch {
     case e: Error => controller.report(e); null
     case e: java.lang.Exception => log(e.getMessage); null
   }
}
