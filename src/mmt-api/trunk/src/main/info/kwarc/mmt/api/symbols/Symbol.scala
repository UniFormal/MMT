package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._

/**
 * A Symbol represents an MMT symbol.<p>
 * 
 * @param parent the {@link info.kwarc.mmt.api.names.Path} of the parent theory
 * @param name the name of the symbol
 */
abstract class Symbol extends NamedDeclaration {
   val parent : TheoryObj
}

/**
 * An Assignment represents an MMT assignment.<p>
 * 
 * @param parent the {@link info.kwarc.mmt.api.names.Path} of the parent link
 * @param name the name of the instantiated symbol
 */
abstract class Assignment extends NamedDeclaration {
   val parent : Morph
   val target : Obj
}
