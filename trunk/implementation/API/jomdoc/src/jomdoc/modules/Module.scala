package jomdoc.modules
import jomdoc._
import jomdoc.libraries._

/**
 * A Module represents an MMT module.<p>
 * 
 * @param doc the {@link jomdoc.names.Path} of the parent document
 * @param name the name of the module
 */
abstract class Module(val parent : DPath, val name : LocalPath) extends ContentElement {
   def path = parent ? name
}