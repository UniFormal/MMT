package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import objects._

/** a mutable wrapper around a Term together with status information */
class TermDimension {
   /** the Term */
   var term: Option[Term] = None
   /** true if the Term is dirty, i.e., has to be recomputed */
   var dirty: Boolean = false
}

/** TermContainer acts as the interface between the structural and the object level
 * 
 * Elements like [[info.kwarc.mmt.api.symbols.Constant]] that have a [[info.kwarc.mmt.api.objects.Term]] as a component
 * will not declare a term directly but a TermContainer.
 * 
 * TermContainer keeps track of different syntactic representations of the same semantic term.
 * It also stores additional status information.
 * 
 * The representations are read < parsed < analyzed.
 * Setting a representation marks the higher representations as dirty.
 */
class TermContainer {
   private var _read     : Option[String] = None
   private val _parsed   = new TermDimension
   private val _analyzed = new TermDimension
   /** the unparsed string representation */
   def read = _read
   /** the parsed representation without further analysis */
   def parsed = _parsed.term
   /** the analyzed representation after type and argument reconstruction */
   def analyzed = _analyzed.term
   /** setter for the unparsed string representation */
   def read_=(s: String) {
      _read           = Some(s)
      if (_parsed.term.isDefined)   _parsed.dirty   = true
      if (_analyzed.term.isDefined) _analyzed.dirty = true
   }
   /** setter for the parsed representation without further analysis */
   def parsed_=(t: Term) {
      _parsed.term    = Some(t)
      _parsed.dirty   = false
      if (_analyzed.term.isDefined) _analyzed.dirty = true
   }
   /** setter for the analyzed representation */
   def analyzed_=(t: Term) {
      _analyzed.term  = Some(t)
      _analyzed.dirty = false
   }
   /** getter for the best available representation: analyzed or parsed */
   def get: Option[Term] = _analyzed.term orElse _parsed.term
}

/** helper object */
object TermContainer {
   /** factory for a string incarnation */
   def apply(s: String) = {
      val tc = new TermContainer
      tc.read = s
      tc
   }
   /** factory for a parsed incarnation */
   def apply(t: Term) = {
      val tc = new TermContainer
      tc.parsed = t
      tc
   }
   /** factory for an optional parsed incarnation */
   def apply(tOpt: Option[Term]) = {
      val tc = new TermContainer
      tOpt foreach {t => tc.parsed = t}
      tc
   }
}