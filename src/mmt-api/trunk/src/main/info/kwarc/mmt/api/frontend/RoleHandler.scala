package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._

/**
 * A ChangeListener is an Extension that is called on every Constant
 */
abstract class ChangeListener extends Extension {
   /** called after adding the element */
   def onAdd(c: ContentElement) {}
   /** called after deleting the element */
   def onDelete(path: Path) {}
   /** called after clearing the Constant */
   def onClear {}
   /** called after checking the element */
   def onCheck(c: ContentElement) {}
   /** called when navigating to an element */
   def onNavigate(p: Path) {}
}