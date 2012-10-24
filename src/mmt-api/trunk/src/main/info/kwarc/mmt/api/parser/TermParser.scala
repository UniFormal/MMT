package info.kwarc.mmt.api.parser
import info.kwarc.mmt.api._
import objects._

/** a ParsingUnit represents a term that is to be parsed
 * @param component the URI of the Term, if any; may be null
 * @param scope the theory against which to parse
 * @param context the context against which to parse
 * @param term the term to be parse
 * */
case class ParsingUnit(component: CPath, scope: Term, context: Context, term: String)

/** a TermParser parses Term's. Instances are maintained by the ExtensionManager and retrieved and called by the structural parser. */
abstract class TermParser {
   def apply(pu: ParsingUnit): Term
}

/** A default parser that parses any string into an OMSemiFormal object. */
object DefaultParser extends TermParser {
   def apply(pu: ParsingUnit): Term = OMSemiFormal(Text(pu.scope.toMPath.toPath, pu.term))
}

/**
 * Generic Error Class
 */
object ParsingError {
  def apply(msg : String, sr : SourceRegion) = SourceError("object-parser", SourceRef(null, sr), msg) //TODO container missing
//("Parsing Error: " + msg + " at (" + start + ","+ end + ")") {
  //def stackTrace : String = getStackTrace.map(x => x.toString).mkString("","\n","")
}
