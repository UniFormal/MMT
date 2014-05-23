package info.kwarc.mmt.api.metadata
import info.kwarc.mmt.api._

/** convenience methods for marking generated objects (without a specific origin)
 *  
 *  This is used for, e.g., VarDecl whose type was inferred, and generated notations.
 */
object Generated extends Tagger(utils.mmt.mmtsymbol("generated"))
