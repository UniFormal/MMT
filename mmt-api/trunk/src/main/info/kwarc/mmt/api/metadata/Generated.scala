package info.kwarc.mmt.api.metadata
import info.kwarc.mmt.api._

/** convenience methods for marking generated objects (without a specific origin)
 *  
 *  This is used for, e.g., generated notations.
 */
object Generated extends Tagger(utils.mmt.mmtsymbol("generated"))

/** a variable binding introduced by abstracting over free variables */
object TagAbstracted extends Tagger(utils.mmt.mmtsymbol("abstracted"))
/** a variable binding where the type was inferred */
object TagInferredType extends Tagger(utils.mmt.mmtsymbol("inferred-type"))
/** a variable binding introduced by eta-expansion */
object TagEtaExpanded extends Tagger(utils.mmt.mmtsymbol("eta-expanded"))
