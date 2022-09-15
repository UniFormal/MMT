package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.utils.FilePath

/**
 * A ChangeListener is an Extension that is called on every Constant
 */
trait ChangeListener extends Extension {
   /** called after adding the element */
   def onAdd(c: StructuralElement): Unit = {}
   /** called after deleting the element
    *  @param old the now-deleted element
    */
   def onDelete(old: StructuralElement): Unit = {}
   /** called after updating the element
    *  @param newElem the element after its update
    *  @param oldElem the element after its update
    *  defaults to onDelete + onAdd
    */
   def onUpdate(oldElem: StructuralElement, newElem: StructuralElement): Unit = {
      onDelete(oldElem)
      onAdd(newElem)
   }
   /** called after clearing the Constant */
   def onClear: Unit = {}
   /** called after checking the element */
   def onCheck(c: StructuralElement): Unit = {}
   /** called when navigating to an element */
   def onNavigate(p: Path): Unit = {}
   /** called when navigating to a physical location */
   def onNavigateSource(r: parser.SourceRef): Unit = {}
   /** called when a new archive is added */
   def onArchiveOpen(a: Archive): Unit = {}
   /** called when an archive is removed */
   def onArchiveClose(a: Archive): Unit = {}
   /** called when a file was built */
   def onFileBuilt(a: Archive, target: TraversingBuildTarget, path: FilePath, res: BuildResult): Unit = {}
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

   def onAdd(c: StructuralElement): Unit = {
      tryAll(_.onAdd(c))
   }

   def onDelete(c: StructuralElement): Unit = {
      tryAll(_.onDelete(c))
   }

   def onUpdate(oldElem: StructuralElement, newElem: StructuralElement): Unit = {
      tryAll(_.onUpdate(oldElem, newElem))
   }

   def onClear: Unit = {
      tryAll(_.onClear)
   }

   def onCheck(c: StructuralElement): Unit = {
      tryAll(_.onCheck(c))
   }

   def onNavigate(p: Path): Unit = {
      tryAll(_.onNavigate(p))
   }

   def onNavigateSource(r: parser.SourceRef): Unit = {
      tryAll(_.onNavigateSource(r))
   }

   def onArchiveOpen(a: archives.Archive): Unit = {
      tryAll(_.onArchiveOpen(a))
   }

   def onArchiveClose(a: archives.Archive): Unit = {
      tryAll(_.onArchiveClose(a))
   }

   def onFileBuilt(a: Archive, t: TraversingBuildTarget, p: FilePath, res: BuildResult): Unit = {
      tryAll(_.onFileBuilt(a, t, p, res))
   }
}
