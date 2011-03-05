package info.kwarc.mmt.api.symbols
import jomdoc._
import jomdoc.modules._

/**
 * A Symbol represents an MMT symbol.<p>
 * 
 * @param parent the {@link jomdoc.names.Path} of the parent theory
 * @param name the name of the symbol
 */
abstract class Symbol(parent : MPath, name : LocalPath) extends Declaration(parent, name) {
}
