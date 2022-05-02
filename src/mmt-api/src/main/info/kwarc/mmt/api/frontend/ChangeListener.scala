package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.utils.FilePath

/**
 * A ChangeListener is an Extension that is called on every Constant
 */
trait ChangeListener extends Extension {
   /** called after adding the element */
   def onAdd(c: StructuralElement) {}
   /** called after deleting the element
    *  @param old the now-deleted element
    */
   def onDelete(old: StructuralElement) {}
   /** called after updating the element
    *  @param newElem the element after its update
    *  @param oldElem the element after its update
    *  defaults to onDelete + onAdd
    */
   def onUpdate(oldElem: StructuralElement, newElem: StructuralElement) {
      onDelete(oldElem)
      onAdd(newElem)
   }
   /** called after clearing the Constant */
   def onClear {}
   /** called after checking the element */
   def onCheck(c: StructuralElement) {}
   /** called when navigating to an element */
   def onNavigate(p: Path) {}
   /** called when navigating to a physical location */
   def onNavigateSource(r: parser.SourceRef) {}
   /** called when a new archive is added */
   def onArchiveOpen(a: Archive) {}
   /** called when an archive is removed */
   def onArchiveClose(a: Archive) {}
   /** called when a file was built */
   def onFileBuilt(a: Archive, target: TraversingBuildTarget, path: FilePath, res: BuildResult) {}
}

/**
 * Convenience class to notify a set of [[ChangeListener]]s
 */
class Notify(listeners: List[ChangeListener], report: Report) {
   private def tryAll(f: ChangeListener => Unit): Unit = {
     listeners.foreach {l =>
        try {f(l)}
        catch {
          case e: Error =>
             val ee = l.LocalError("error while responding to change").setCausedBy(e)
             report(ee)
           case e: Exception =>
             val ee = l.LocalError("error while responding to change").setCausedBy(e)
             report(ee)
         }
     }
   }

   def onAdd(c: StructuralElement) {
      tryAll(_.onAdd(c))
   }

   def onDelete(c: StructuralElement) {
      tryAll(_.onDelete(c))
   }

   def onUpdate(oldElem: StructuralElement, newElem: StructuralElement) {
      tryAll(_.onUpdate(oldElem, newElem))
   }

   def onClear {
      tryAll(_.onClear)
   }

   def onCheck(c: StructuralElement) {
      tryAll(_.onCheck(c))
   }

   def onNavigate(p: Path) {
      tryAll(_.onNavigate(p))
   }

   def onNavigateSource(r: parser.SourceRef) {
      tryAll(_.onNavigateSource(r))
   }

   def onArchiveOpen(a: archives.Archive) {
      tryAll(_.onArchiveOpen(a))
   }

   def onArchiveClose(a: archives.Archive) {
      tryAll(_.onArchiveClose(a))
   }

   def onFileBuilt(a: Archive, t: TraversingBuildTarget, p: FilePath, res: BuildResult) {
      tryAll(_.onFileBuilt(a, t, p, res))
   }
}
