package info.kwarc.mmt.api.modules
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries._

/**
 * A Module represents an MMT module.<p>
 * 
 * @param doc the {@link info.kwarc.mmt.api.names.Path} of the parent document
 * @param name the name of the module
 */
abstract class Module(val parent : DPath, val name : LocalPath) extends ContentElement {
   def path = parent ? name
}