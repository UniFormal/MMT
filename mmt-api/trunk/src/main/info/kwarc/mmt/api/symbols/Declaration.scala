package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.libraries._

/**
 * Statement unifies MMT symbols, MMT assignments, and MMT imports. These are the children of modules.
 * 
 * @param parent the {@link info.kwarc.mmt.api.names.Path} of the parent theory or link, respectively
 */
abstract class Declaration extends ContentElement {
   val home : ModuleObj
   val parent = home % LNEmpty()
}

/**
 * Declaration unifies MMT symbols and MMT assignments. These are the named statements.
 * 
 * @param parent the {@link info.kwarc.mmt.api.names.Path} of the parent theory or link, respectively
 * @param name the name of the symbol that is declared or instantiated, respectively
 */
abstract class NamedDeclaration extends Declaration {
   val name : LocalName
   def path = GlobalName(home, name)
}