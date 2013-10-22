package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import objects._

/** a mutable wrapper around a Term together with status information */
class TermDimension {
   /** the Term */
   var term: Option[Term] = None
   /** true if the Term is dirty, i.e., has to be recomputed */
   var dirty: Boolean = false
   /** the time of the last change */
   var time : Long = System.currentTimeMillis
   /** delete all data */
   def delete {
      term = None
      dirty = false
   }
   override def toString = term.toString + (if (dirty) " (dirty)" else "")
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
class TermContainer extends ComponentContainer {
   private var _read     : Option[String] = None
   private val _parsed   = new TermDimension
   private val _analyzed = new TermDimension
   override def toString = "read: " + _read.toString + "\nparsed: " + _parsed.toString + "\nanalyzed: " + _analyzed.toString
   /** the unparsed string representation */
   def read = _read
   /** the parsed representation without further analysis */
   def parsed = _parsed.term
   /** the analyzed representation after type and argument reconstruction */
   def analyzed = _analyzed.term
   /** setter for the unparsed string representation */
   def read_=(s: Option[String]) {
      val changed = s != _read
      if (changed) {
         _read           = s
         _parsed.dirty   = true
         _analyzed.dirty = true
      }
   }
   def read_=(s: String) {read_=(Some(s))}
   /** setter for the parsed representation without further analysis */
   def parsed_=(t: Option[Term]) {
      val changed = t != _parsed.term
      if (changed) {
         _parsed.term    = t
         _parsed.time    = System.currentTimeMillis
         _analyzed.dirty = true
      }
      _parsed.dirty = false
   }
   def parsed_=(t: Term) {parsed_=(Some(t))}
   /** setter for the analyzed representation */
   def analyzed_=(t: Option[Term]) {
      val changed = t != _analyzed.term
      if (changed) {
         _analyzed.term = t
         _analyzed.time = System.currentTimeMillis
      }
      _analyzed.dirty = false
   }
   def analyzed_=(t: Term) {analyzed_=(Some(t))}
   /** true if the term must still be (re)parsed */
   def isParsedDirty = ! _parsed.dirty
   /** time of the last change */
   def lastChangeParsed = _parsed.time
   /** true if the term must be (re)analyzed */
   def isAnalyzedDirty =  _analyzed.dirty
   /** marks this term for reanalyzation */
   def setAnalyzedDirty {_analyzed.dirty = true}
   /** time of the last change */
   def lastChangeAnalyzed = _analyzed.time
   /** getter for the best available representation: analyzed or parsed */
   def get: Option[Term] = _analyzed.term orElse _parsed.term
   /** true if any dimension is present, i.e., if the component is present */
   def isDefined = _read.isDefined || get.isDefined
   /** copies over the components of another TermContainer
    *  dependent dimensions that are not part of tc become dirty
    */
   def update(tc: ComponentContainer) {tc match {
      case tc: TermContainer =>
         read = tc.read
         if (!(tc.parsed.isEmpty && tc.read.isDefined)) // keep old, now dirty, value if tc is unparsed
            parsed = tc.parsed
         if (!(tc.analyzed.isEmpty && tc.parsed.isDefined)) // keep old, now dirty, value if tc is unanalyzed
            analyzed = tc.analyzed
      case _ => throw ImplementationError("not a TermContainer")
   }}
   /** delete this component */
   def delete {
      _read = None
      List(_parsed,_analyzed).foreach {_.delete}
      dependsOn.clear
   }
   /** stores the set of components that analysis depended on */
   lazy val dependsOn = new scala.collection.mutable.HashSet[CPath]
}

/** helper object */
object TermContainer {
   /** factory for a string incarnation */
   def apply(s: String) = {
      val tc = new TermContainer
      tc.read = s
      tc
   }
   /** convenience factory */
   def apply(t: Term): TermContainer = apply(Some(t))
   /** factory for an optionally given Term
    *  @param tOpt the term; treated as parsed or analyzed depending on AbstractObjectParser.isOnlyParsed
    */
   //TODO remove this - force users to decide whether a term is analyzed/parsed
   //currently this is just a workaround
   def apply(tOpt: Option[Term]): TermContainer = {
      val tc = new TermContainer
      tOpt foreach {t => if (parser.AbstractObjectParser.isOnlyParsed(t))
         tc.parsed = t
      else
         tc.analyzed = t
      }      
      tc
   }
}