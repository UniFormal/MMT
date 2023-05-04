package info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils.MMT_TODO
import objects._

/** a mutable wrapper around a value together with status information */
class ObjDimension[T] {
   /** the Term */
   var obj: Option[T] = None
   /** getter, but only if not dirty */
   def termIfNotDirty = if (dirty) None else obj
   /** true if dirty, i.e., has to be recomputed */
   var dirty: Boolean = false
   /** the time of the last change */
   var time : Long = System.currentTimeMillis
   /** delete all data */
   def delete: Unit = {
      obj = None
      dirty = false
      time = System.currentTimeMillis
   }
   override def toString = obj.toString + (if (dirty) " (dirty)" else "")
}

/** TermContainer acts as the interface between the structural and the object level
 *
 * Elements like [[info.kwarc.mmt.api.symbols.Constant]] that have a [[info.kwarc.mmt.api.objects.Term]] as a component
 * will not declare a term directly but a TermContainer.
 *
 * TermContainer keeps track of different syntactic representations of the same semantic term.
 * It also stores additional status information.
 *
 * The representations are read < parsed < analyzed < normalized.
 * Setting a representation marks the higher representations as dirty.
 *
 * @tparam T the type of objects stored; the type bound is not actually needed, but it helps putting sharper bound on some return types
 */
trait ObjContainer[T <: Obj] extends AbstractObjectContainer {
   private var _read       : Option[String] = None
   private val _parsed     = new ObjDimension[T]
   private val _analyzed   = new ObjDimension[T]
   private val _normalized = new ObjDimension[T]
   override def toString = "read: " + _read.toString + "\nparsed: " + _parsed.toString + "\nanalyzed: " + _analyzed.toString +"\nnormalized: " + _normalized.toString
   /** the unparsed string representation */
   def read = _read
   /** the parsed representation without further analysis (if non-dirty) */
   def parsed = _parsed.termIfNotDirty
   /** the analyzed representation after type and argument reconstruction (if non-dirty) */
   def analyzed = _analyzed.termIfNotDirty
   /** the normalized representation after type and argument reconstruction (if non-dirty)
    *
    *  This is not always computed, and even if it is, it is not returned by default by the get method.
    *  It is intended for optimizations where the normalized form of a Term would otherwise have to be recomputed.
    */
   def normalized = _normalized.termIfNotDirty
   /** sets the normalized representation (if not already set) using a normalization function
    *  Because normalization is need-based, this should be called, if possible, before accessing the normalized representation. */ 
   def normalize(normFun: T => T): Unit = {
     normalized match {
       case Some(_) =>
       case None => normalized = analyzed map normFun
     }
   }
   /** setter for the unparsed string representation */
   def read_=(s: Option[String]): Boolean = {
      val changed = s != _read
      if (changed) {
         _read           = s
         _parsed.dirty   = true
         _analyzed.dirty = true
         _normalized.dirty = true
      }
      changed
   }
   def read_=(s: String): Unit = {read_=(Some(s))}
   /** setter for the parsed representation without further analysis */
   def parsed_=(t: Option[T]): Boolean = {
      val changed = t != _parsed.obj
      /*
       * TODO assume old-parsed (op), old-analyzed (oa), new-parsed (np), need new-analyzed (na)
       * metadata of oa points to metadata of op
       * np contains new metadata (e.g., for source references) even if op == np
       * if op != np, we compute na anyway, no problem
       * if op == np, na := oa except that metadata of np must be integrated into metadata of op
       */
      _parsed.obj = t
      if (changed) {
         _parsed.time    = System.currentTimeMillis
         _analyzed.dirty = true
         _normalized.dirty = true
      }
      _parsed.dirty = false
      changed
   }
   def parsed_=(t: T): Unit = {parsed_=(Some(t))}
   /** setter for the analyzed representation */
   def analyzed_=(t: Option[T]): Boolean = {
      val changed = t != _analyzed.obj
      _analyzed.obj = t  // set this even if equal in order to get the metadata of the new term
      if (changed) {
         _analyzed.time = System.currentTimeMillis
         _normalized.dirty = true
      }
      _analyzed.dirty = false
      changed
   }
   def analyzed_=(t: T): Boolean = {
      analyzed_=(Some(t))
   }
   /** setter for the normalized representation */
   def normalized_=(t: Option[T]): Boolean = {
      val changed = t != _normalized.obj
      _normalized.obj = t  // set this even if equal in order to get the metadata of the new term
      if (changed) {
         _normalized.time = System.currentTimeMillis
      }
      _normalized.dirty = false
      changed
   }
   def getAnalyzedIfFullyChecked: Option[T]
   /** true if the term must still be (re)parsed */
   def isParsedDirty = ! _parsed.dirty
   /** time of the last change */
   def lastChangeParsed = _parsed.time
   /** true if the term must be (re)analyzed */
   def isAnalyzedDirty =  _analyzed.dirty
   /** marks this term for re-analysis */
   def setAnalyzedDirty: Unit = {_analyzed.dirty = true}
   /** time of the last change */
   def lastChangeAnalyzed = _analyzed.time

   /** getter for the best available non-dirty representation: analyzed or parsed */
   def get: Option[T] = analyzed orElse parsed
   /** true if any dimension is present, i.e., if the component is present */
   def isDefined = _read.isDefined || get.isDefined
   /** stores the set of components that analysis depended on */
   lazy val dependsOn = new scala.collection.mutable.HashSet[CPath]
   /** delete this component */
   def delete(): Unit = {
      _read = None
      List(_parsed,_analyzed,_normalized).foreach {_.delete}
      dependsOn.clear()
   }
   /** clears the contents of this component and sets it to a new value */
   def set(t: T): Unit = {
     delete()
     analyzed = Some(t)
   }

   // auxiliary methods that cannot be implemented generically in Scala
   type ThisType <: ObjContainer[T]
   /** creates a new empty container for the same type */
   protected def newEmpty: ThisType
   /** checks if a container stores objects of the same type */
   protected def hasSameType(oc: ObjContainer[_]): Boolean

   /** copies over the non-dirty components of another TermContainer
    *  dependent dimensions that are not part of tc become dirty
    */
   def update(tc: ComponentContainer) = {tc match {
      case tc: ObjContainer[T] if hasSameType(tc) =>
         var changed = (read = tc.read)
         if (changed || tc.parsed.isDefined)
            changed | {changed = true; (parsed = tc.parsed)}
         if (changed || tc.analyzed.isDefined)
            changed | {changed = true; (analyzed = tc.analyzed)}
         if (changed || tc.normalized.isDefined)
            changed |= {changed = true; (normalized = tc.normalized)}
         changed
      case _ => throw ImplementationError("not a TermContainer")
   }}

   /** creates a deep copy of this container, dirty parts are dropped */
   def copy: ThisType = {
     val tc = newEmpty
     tc.read = read
     tc.parsed = parsed
     tc.analyzed = analyzed
     tc.normalized = normalized
     tc
   }
   def merge(that: ThisType): ThisType = {
     if (that.isDefined) that else this.copy
   }
}

/** container for mutable terms */
class TermContainer extends ObjContainer[Term] with AbstractTermContainer {
   type ThisType = TermContainer
   protected def newEmpty = new TermContainer
   protected def hasSameType(oc: ObjContainer[_]) = oc.isInstanceOf[TermContainer]

   /** applies a function to the contained Term and returns a new TermContainer */
   def map(f: Term => Term) = TermContainer(get map f)

   /** returns the analyzed term if it has been successfully and completely checked */
   def getAnalyzedIfFullyChecked = {
     analyzed flatMap {t =>
       val pr = parser.ParseResult.fromTerm(t)
       if (pr.unknown.isEmpty && pr.free.isEmpty)
         Some(pr.term)
       else
         None
     }
   }
   /** true if the analyzed part is dirty or a previous check did not succeed */
   def checkNeeded = getAnalyzedIfFullyChecked.isDefined
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
   @deprecated("MMT_TODO: remove this - force users to decide whether a term is analyzed/parsed", since="forever")
   def apply(tOpt: Option[Term]): TermContainer = {
      val tc = new TermContainer
      tOpt foreach {t => if (parser.ObjectParser.isOnlyParsed(t))
         tc.parsed = t
      else
         tc.analyzed = t
      }
      tc
   }

   def asParsed(term: Option[Term]): TermContainer = {
      val tc = new TermContainer
      tc.parsed = term
      tc
   }
   def asParsed(term: Term): TermContainer = asParsed(Some(term))


   def asAnalyzed(term: Option[Term]): TermContainer = {
      val tc = new TermContainer
      tc.analyzed = term
      tc
   }
   def asAnalyzed(term: Term): TermContainer = asAnalyzed(Some(term))

   def empty(): TermContainer = new TermContainer
}

/** container for mutable contexts */
// contrary to a TermContainer, a ContextContainer stores unknown and free variables separately
class ContextContainer extends ObjContainer[Context] {
   var unknowns = Context.empty
   var free = Context.empty
   def getAnalyzedIfFullyChecked = {
     if (unknowns.isEmpty && free.isEmpty)
       analyzed
     else
       None
   }

   type ThisType = ContextContainer
   protected def newEmpty = new ContextContainer
   protected def hasSameType(oc: ObjContainer[_]) = oc.isInstanceOf[ContextContainer]

   /** applies a function to the contained Context and returns a new ContextContainer */
   def map(f: Context => Context) = {
     get.map(c => ContextContainer(f(c))).getOrElse(newEmpty)
   }
}

object ContextContainer {
   /** convenience factory for an analyzed context */
   def apply(c: Context): ContextContainer = {
     val cc = new ContextContainer
     cc.analyzed = c
     cc
   }
   def apply(s: String): ContextContainer = {
      val tc = new ContextContainer
      tc.read = s
      tc
   }

}
