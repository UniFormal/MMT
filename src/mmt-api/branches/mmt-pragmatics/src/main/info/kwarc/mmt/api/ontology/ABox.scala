package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.utils.MyList.fromList
import info.kwarc.mmt.api.objects._
import scala.collection.mutable.{HashSet,HashMap}

/**
 * An ABoxStore stores the abox of the loaded elements with respect to the MMT ontology.
 * Triples (subject, binary, object) are hashed three ways so that for any two components
 * the set of third components can be retrieved efficiently.
 */
class RelStore(report : frontend.Report) {
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
   def +=(d : RelationalElement) {
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
    * There is no result set; instead, a continuation is passed that can be used to build the result set;
    * this permits, e.g., to keep track of the order in which results were found.
    * @param start the MMTURI to which the results are related
    * @param q the query to be executed; the way in which results are related to the start
    * @param add a continuation called on every element in the result set (in topological order, duplicate calls possible)
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
    * This is currently not implemented.
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