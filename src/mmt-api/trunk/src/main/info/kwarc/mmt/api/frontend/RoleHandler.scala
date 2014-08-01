package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._

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
   def onNewArchive(a: archives.Archive) {}
}

/**
 * Convenience class to notify a set of [[ChangeListener]]s
 */
class Notify(listeners: List[ChangeListener]) {
   def onAdd(c: ContentElement)          {listeners.foreach(_.onAdd(c))}
   def onDelete(c: ContentElement)       {listeners.foreach(_.onDelete(c))}
   def onUpdate(newElem: ContentElement) {listeners.foreach(_.onUpdate(newElem))}
   def onClear                           {listeners.foreach(_.onClear)}
   def onCheck(c: ContentElement)        {listeners.foreach(_.onCheck(c))}
   def onNavigate(p: Path)               {listeners.foreach(_.onNavigate(p))}
   def onNewArchive(a: archives.Archive) {listeners.foreach(_.onNewArchive(a))}
}