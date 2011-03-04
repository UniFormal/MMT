package jomdoc.symbols
import jomdoc._
import jomdoc.modules._
import jomdoc.libraries._

/**
 * Statement unifies MMT symbols, MMT assignments, and MMT imports. These are the children of modules.
 * 
 * @param parent the {@link jomdoc.names.Path} of the parent theory or link, respectively
 */
abstract class Statement(val parent : MPath) extends ContentElement

/**
 * Declaration unifies MMT symbols and MMT assignments. These are the named statements.
 * 
 * @param parent the {@link jomdoc.names.Path} of the parent theory or link, respectively
 * @param name the name of the symbol that is declared or instantiated, respectively
 */
abstract class Declaration (parent : MPath, val name : LocalPath) extends Statement(parent) {
   def path = parent ? name
}