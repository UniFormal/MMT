package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import archives._

/**
 * A ChangeListener is an Extension that is called on every Constant
 */
trait ChangeListener extends Extension {
   /** called after adding the element */
   def onAdd(c: ContentElement) {}
   /** called after deleting the element
    *  @param old the now-deleted element
    */
   def onDelete(old: ContentElement) {}
   /** called after updating the element
    *  @param newElem the element after its update
    */
   def onUpdate(newElem: ContentElement) {}
   /** called after clearing the Constant */
   def onClear {}
   /** called after checking the element */
   def onCheck(c: ContentElement) {}
   /** called when navigating to an element */
   def onNavigate(p: Path) {}
   /** called when a new archive is added */
   def onArchiveOpen(a: Archive) {}
   /** called when an archive is removed */
   def onArchiveClose(a: Archive) {}
   /** called when a file was built */
   def onFileBuilt(a: Archive, target: TraversingBuildTarget, path: List[String]) {}
}

/**
 * Convenience class to notify a set of [[ChangeListener]]s
 */
class Notify(listeners: List[ChangeListener], report: Report) {
   private def tryAll(f: ChangeListener => Unit) {
     listeners.foreach {l =>
        try {f(l)}
        catch {
          case e: Error =>
             val ee = l.LocalError("change listener caused error").setCausedBy(e)
             report(ee)
           case e: Exception =>
             val ee = l.LocalError("change listener caused error").setCausedBy(e)
             report(ee)
         }
     }
   }
   def onAdd(c: ContentElement)          {tryAll(_.onAdd(c))}
   def onDelete(c: ContentElement)       {tryAll(_.onDelete(c))}
   def onUpdate(newElem: ContentElement) {tryAll(_.onUpdate(newElem))}
   def onClear                           {tryAll(_.onClear)}
   def onCheck(c: ContentElement)        {tryAll(_.onCheck(c))}
   def onNavigate(p: Path)               {tryAll(_.onNavigate(p))}
   def onArchiveOpen(a: archives.Archive) {tryAll(_.onArchiveOpen(a))}
   def onArchiveClose(a: archives.Archive) {tryAll(_.onArchiveClose(a))}
   def onFileBuilt(a: Archive, t: TraversingBuildTarget, p: List[String]) {tryAll(_.onFileBuilt(a,t,p))}
}