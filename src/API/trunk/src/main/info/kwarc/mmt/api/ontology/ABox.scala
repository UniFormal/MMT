package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.utils.MyList.fromList
import info.kwarc.mmt.api.objects._
import scala.collection.mutable.{HashSet,HashMap}
/**
 * Objects of type Query are expressions of a simple query language over an MMT ABox.
 * The semantics of a query is that it denotes a binary relation between MMT paths.
 */
sealed abstract class Query {
   def |(q: Query) = Choice(this, q)
   def *(q: Query) = Sequence(this, q)
   /** the same query but for the inverse relation */
   def unary_- : Query
}
/** base case: a primitive binary relation */
case class ToObject(dep : Binary) extends Query {
   def unary_- = ToSubject(dep)
   override def toString = "+ " + dep
}
/** base case: the inverse of a primitive binary relation */
case class ToSubject(dep : Binary) extends Query {
   def unary_- = ToObject(dep)
   override def toString = "- " + dep
}
/** the transitive closure of a relation */
case class Transitive(q : Query) extends Query {
   def unary_- = Transitive(- q)
   override def toString = "(" + q + ")*"
}
/** the union of a list of relations */
case class Choice(qs : Query*) extends Query {
   def unary_- = Choice(qs.map(- _) : _*)
   override def toString = qs.mkString(""," | ", "")
}
/** the composition of a list of relations */
case class Sequence(qs : Query*) extends Query {
   def unary_- = Sequence(qs.reverse.map(- _) : _*)
   override def toString = qs.mkString(""," ; ", "")
}
/** the reflexive relation */
case object Reflexive extends Query {
   def unary_- = this
   override def toString = "Id"
}
/** the reflexive relation restricted to a unary predicate */
case class HasType(tp : Unary) extends Query {
   def unary_- = this
   override def toString = ":" + tp
}

object Query {
    /** S has a structure declaration with domain O, defined as an abbreviation */
	def HasStructureFrom = Sequence(- HasCodomain, HasType(IsStructure), + HasDomain)
    /** Disjunction of all binary relations */
	def AnyDep = Choice(+HasOccurrenceOfInType, +HasOccurrenceOfInDefinition, +HasOccurrenceOfInTarget,
   +IsAliasFor, +HasMeta, +HasOccurrenceOfInImport, +HasDomain, +HasCodomain)
}

/**
 * An ABoxStore stores the abox of the loaded documents with respect to the MMT ontology.
 * Triples (subject, binary, object) are hashed
 * such that for each two components the set of possible completions can be retrieved efficiently.
 */
class ABoxStore(report : frontend.Report) {
   private val types = new HashMap[Path, Unary]
   private val subjects = new HashMapToSet[(Binary,Path), Path]
   private val objects = new HashMapToSet[(Path,Binary), Path]
   private val dependencies = new HashMapToSet[(Path,Path), Binary]
   private def log(msg : => String) = report("abox", msg)
   /** retrieves all Individual declarations */
   def getInds : Iterator[Individual] = types.map({case (p,t) => Individual(p,t)}).iterator
   /** retrieves all Relation declarations */
   def getDeps : Iterator[Relation] = dependencies.pairs.map({case ((p,q), d) => Relation(d,p,q)})
   /** adds a declaration */
   def +=(d : ABoxDecl) {
      log(d.toString)
      d match {
        case Relation(dep, subj, obj) =>                  
           subjects += ((dep, obj), subj)
           objects += ((subj, dep), obj)
           dependencies += ((subj, obj), dep)
        case Individual(p, tp) => types(p) = tp
      }
   }
   /**
    * Executes a query.
    * @param start an MMT path
    * @param q the query to be executed
    * @param add a continuation called on every element in the result set (duplicate calls possible)
    */
   def query(start : Path, q : Query, add : Path => Unit) {q match {
      case ToObject(d) => objects(start, d).foreach(add)   //all paths related to start via d 
      case ToSubject(d) => subjects(d, start).foreach(add) //all paths inversely related to start via d
      //only start itself
      case Reflexive => add(start)
      //the set of paths related to start via arbitrarily many q-steps (depth-first, children before parent)
      case Transitive(q) =>
         var added = HashSet.empty[Path]
         def step(p : Path) {
            if (! added.contains(p)) {
               added += p
               val next = query(p, q)
               next.foreach(step)
               add(p) //add parent only after children
            }
         }
         val next = query(start, q)
         next.foreach(step)
      //the set of paths related to start by any of the relations in qs (in the order listed)
      case Choice(qs @ _*) => qs.foreach(q => query(start, q, add))
      //the set of paths related to start by making steps according to qs
      case Sequence(qs @ _*) => qs.toList match {
         case Nil => add(start)
         case hd :: tl => query(start, hd).foreach(p => query(p, Sequence(tl : _*), add))
      }
      //only start itself iff it has the right type
      case HasType(tp) => if (types.getOrElse(start, null) == tp) add(start)
   }}
   
   /**
    * Special case of query(_,_,_) that stores the result set as a list.
    * Elements are ordered as they are found, duplicates are removed.   
    */
   def query(start : Path, q : Query) : List[Path] = {
      var result : List[Path] = Nil
      query(start, q, (p : Path) => result ::= p)
      result.distinct
   }
      
   /**
    * Returns the set of theories a theory depends on
    */
   def theoryClosure(p : MPath) : List[MPath] = {
      val q = Transitive(+HasMeta | +HasOccurrenceOfInImport | Query.HasStructureFrom | Reflexive)
      val l = query(p, q)
      //for well-formed MMT, l can only contain MPaths
      l.mapPartial {case p : MPath => Some(p) case _ => None}
   }
   /**
    * Returns the set of MMT paths an object depends on
    * If <code>splittable</code> is empty, this is simply the theory closure of the (smallest possible) home theory of o.
    * But theories listed in <code>splittable</code> will not be in the output.
    * Rather, the smallest set of their components that <code>o</code> depends on is contained in the output.
    * This permits to extract from a large monolithic theory the minimal required subtheory.
    * Example application: Assume a theory L for a logic, an L-theory T with lots of definitions and theorems,
    * and a conjecture C. Then objectClosure(C, List(T)) will return only those parts in T that are (indirectly)
    * mentioned in C. This permits to compute a significantly smaller theory when presenting C to an ATP system.
    * Note that L is not splittable, thus L itself and thus all axioms and connectives given in L are returned
    * even if they are not mentioned by C.
    * @param o the MMT object
    * @param splittable a list of theories that may be split.
    * @return the list of MMT paths needed to make the object well-formed
    */
   def objectClosure(o : Obj, splittable : List[MPath]) : HashSet[Path] = {
      null //TODO
   }
   /** deletes all declarations */
   def clear {
      dependencies.clear
      subjects.clear
      objects.clear
      types.clear
   }
}