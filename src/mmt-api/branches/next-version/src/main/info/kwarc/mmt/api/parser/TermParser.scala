package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import objects._

/** a TermParser parses Term's. Instances are maintained by the ExtensionManager and retrieved and called by the structural parser. */
abstract class TermParser {
   def applicable(format: String) : Boolean
   def apply(term: String, scope: Term): Term
}

/** A default parser that parses any string into an OMSemiFormal object. */
object DefaultParser extends TermParser {
   def applicable(format: String) = true
   def apply(term: String, scope: Term): Term = OMSemiFormal(Text(scope.toMPath.toPath, term))
}