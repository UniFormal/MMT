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
   private val individuals = new HashMapToSet[Unary, Path]
   private val types = new HashMap[Path,Unary]
   private val subjects = new HashMapToSet[(Binary,Path), Path]
   private val objects = new HashMapToSet[(Path,Binary), Path]
   private val dependencies = new HashMapToSet[(Path,Path), Binary]
   private def log(msg : => String) = report("abox", msg)
   /** retrieves all Individual declarations */
   def getInds : Iterator[Individual] = individuals.pairs map {case (t,p) => Individual(p,t)}
   /** retrieves all individual of a certain type */
   def getInds(tp: Unary) : Iterator[Path] = individuals(tp).iterator
   /** retrieves type of an Individual */
   def getType(p: Path) : Option[Unary] = types.get(p)
   /** retrieves all Relation declarations */
   def getDeps : Iterator[Relation] = dependencies.pairs map {case ((p,q), d) => Relation(d,p,q)}

   def getObjects(d : Binary) = subjects.keys.filter(_._1 == d).map(_._2).toSet
   def getSubjects(d : Binary) = objects.keys.filter(_._2 == d).map(_._1).toSet

   /** adds a declaration */
   def +=(d : RelationalElement) {
      log(d.toString)
      d match {
        case Relation(dep, subj, obj) =>                  
           subjects += ((dep, obj), subj)
           objects += ((subj, dep), obj)           
           dependencies += ((subj, obj), dep)
        case Individual(p, tp) =>
           types(p) = tp
           individuals += (tp, p)
      }
   }
   
   /** deletes a path from the relation library */
   def deleteSubj(subj : Path) {
     // currently implementation is quite ugly, restructuring might be useful
     val sd = subjects.flatMap(x => x._2.flatMap(y => if (y == subj) List((x._1,y)) else Nil))
     sd.map(p => subjects -= (p._1,p._2))
     val od = objects.flatMap(x => x._2.flatMap(y => if (x._1._1 == subj) List((x._1,y)) else Nil))
     od.map(p => objects -= (p._1,p._2))
     val dd = dependencies.flatMap(x => x._2.flatMap(y => if (x._1._1 == subj) List((x._1,y)) else Nil))
     dd.map(p => dependencies -= (p._1,p._2))
     
     
     types -= subj
     val id = individuals.flatMap(x => x._2.flatMap(y => if (y == subj) List((x._1,y)) else Nil))
     id.map(p => individuals -= (p._1,p._2))     
   }


  /*

   /** renames a path from the relation library */
   def renameSubj(old : Path, nw : Path) {
	 // currently implementation is quite ugly, restructuring might me useful
     val sd = subjects.flatMap(x => x._2.flatMap(y => if (y == old) List((x._1,y)) else Nil))
     sd.map(p => subjects -= (p._1,p._2))
     sd.map(p => subjects += (p._1,nw))
     val od = objects.flatMap(x => x._2.flatMap(y => if (x._1._1 == old) List((x._1,y)) else Nil))
     od.map(p => objects -= (p._1,p._2))
     od.map(p => objects += ((nw,p._1._2),p._2))
     
     val dd = dependencies.flatMap(x => x._2.flatMap(y => if (x._1._1 == old) List((x._1,y)) else Nil))
     dd.map(p => dependencies -= (p._1,p._2))
     dd.map(p => dependencies += ((nw,p._1._2),p._2))
     
     types(nw) = types(old)
     types -= old
     val id = individuals.flatMap(x => x._2.flatMap(y => if (y == old) List((x._1,y)) else Nil))
     id.map(p => individuals -= (p._1,p._2))     
     id.map(p => individuals += (p._1,nw))     
     
   }

   */

   
   def queryList(start : Path, q : RelationExp) : List[Path] = {
      var ps : List[Path] = Nil
      query(start, q) {p => ps ::= p}
      ps
   }
   /**
    * Executes a relational query from a fixed start path.
    * There is no result set; instead, a continuation is passed that can be used to build the result set;
    * this permits, e.g., to keep track of the order in which results were found.
    * @param start the MMTURI to which the results are related
    * @param q the query to be executed; the way in which results are related to the start
    * @param add a continuation called on every element in the result set (in topological order, duplicate calls possible)
    */
   def query(start : Path, q : RelationExp)(implicit add : Path => Unit) {q match {
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
               val next = query(p, q)(step)
               add(p) //add parent only after children
            }
         }
         val next = query(start, q)(step)
      //the set of paths related to start by any of the relations in qs (in the order listed)
      case Choice(qs @ _*) => qs.foreach(q => query(start, q))
      //the set of paths related to start by making steps according to qs
      case Sequence(qs @ _*) => qs.toList match {
         case Nil => add(start)
         case hd :: tl => query(start, hd) {p => query(p, Sequence(tl : _*))}
      }
      //only start itself iff it has the right type
      case HasType(tp) => if (hasType(start, tp)) add(start)
   }}
   def hasType(p: Path, tp: Unary) : Boolean = individuals(tp) contains p
   
   /**
    * Returns the set of theories a theory depends on
    */
   def theoryClosure(p : MPath) : List[MPath] = {
      val q = Transitive(+HasMeta | +Includes | +DependsOn | Reflexive)
      var l : List[MPath] = Nil
      query(p, q) {
         case p : MPath => l ::= p
         case _ =>
      }
      l
   }
   /** deletes all declarations */
   def clear {
      dependencies.clear
      subjects.clear
      objects.clear
      individuals.clear
      types.clear
   }
}

object RelationalElementReader {
   def read(f: File, base: Path, as: RelStore) {
      File.ReadLineWise(f) {line => as += parse(line, base)}
   }
   def parse(s: String, base: Path) : RelationalElement = {
      s.split(" ").toList match {
         case List(tp, ind) => Individual(Path.parse(ind, base), Unary.parse(tp))
         case List(rel, subj, obj) => Relation(Binary.parse(rel), Path.parse(subj, base), Path.parse(obj, base))
         case _ => throw ParseError("not a valid relational element: " + s)
      }
   }
}