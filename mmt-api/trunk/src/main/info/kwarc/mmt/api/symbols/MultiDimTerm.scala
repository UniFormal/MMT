package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import objects._

/** a mutable wrapper around a Term together with status information */
class TermDimension {
   /** the Term */
   var term: Option[Term] = None
   /** true if the Term is dirty, i.e., has to be recomputed */
   var dirty: Boolean = false
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
class TermContainer {
   /**
    * the API may deactivate a TermContainer instead of deleting it to permit reusing it later
    * 
    * invariant: API client code may assume that this flag is never set   
    */
   private[api] var inactive : Boolean = false
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
   def read_=(s: String) {
      val changed = Some(s) != _read
      if (changed) {
         _read           = Some(s)
         _parsed.dirty   = true
         _analyzed.dirty = true
      }
   }
   /** setter for the parsed representation without further analysis */
   def parsed_=(t: Term) {
      val changed = Some(t) != _parsed.term
      if (changed) {
         _parsed.term    = Some(t)
         _parsed.dirty   = false
         _analyzed.dirty = true
      }
   }
   /** setter for the analyzed representation */
   def analyzed_=(t: Term) {
      _analyzed.term  = Some(t)
      _analyzed.dirty = false
   }
   /** true if the term must still be (re)parsed */
   def isParsedDirty = ! _parsed.dirty
   /** true if the term must be (re)analyzed */
   def isAnalyzedDirty =  _analyzed.dirty
   /** marks this term for reanalyzation */
   def setAnalyzedDirty {_analyzed.dirty = true}
   /** getter for the best available representation: analyzed or parsed */
   def get: Option[Term] = _analyzed.term orElse _parsed.term
   /** true if any dimension is present, i.e., if the component is present */
   def isDefined = _read.isDefined || get.isDefined
   /** copies over the components of another TermContainer
    *  dependent dimensions that are not part of tc become dirty
    */
   def update(tc: TermContainer) {
      tc.read foreach {r =>
         read = r
         inactive = false
      }
      tc.parsed foreach {t =>
         parsed = t
         inactive = false
      }
      tc.analyzed foreach {p =>
         analyzed = p
         inactive = false
      }
   }
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