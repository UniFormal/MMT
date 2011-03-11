package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules._

/**
 * A Symbol represents an MMT symbol.<p>
 * 
 * @param parent the {@link info.kwarc.mmt.api.names.Path} of the parent theory
 * @param name the name of the symbol
 */
abstract class Symbol(parent : MPath, name : LocalPath) extends Declaration(parent, name) {
}
