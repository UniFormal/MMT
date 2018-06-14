package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.utils.MyList.fromList
import info.kwarc.mmt.api.objects._
import scala.collection.mutable.{HashSet,HashMap}

/**
 * An ABoxStore stores the abox of the loaded elements with respect to the MMT ontology.
 *
 * Triples (subject, binary, object) are hashed three ways so that for any two components
 * the set of third components can be retrieved efficiently.
 *
 * Use [[TheoryGraph]] for theory graph-level querying
 */
class RelStore(report : frontend.Report) {
   private val individuals = new HashMapToSet[Unary, Path]
   private val types = new HashMap[Path,Unary]
   private val subjects = new HashMapToSet[(Binary,Path), Path]
   private val objects = new HashMapToSet[(Path,Binary), Path]
   private val dependencies = new HashMapToSet[(Path,Path), Binary]

   override def toString = "Individuals: "+individuals.map(i => "\n - "+i.toString)+"\n Theories:"+
     individuals(IsTheory).map(i => "\n - "+i.toString)

   private def log(msg : => String) = report("abox", msg)
   /** retrieves all Individual declarations */
   def getInds : Iterator[Individual] = individuals.pairs map {case (t,p) => Individual(p,t)}
   /** retrieves all individual of a certain type */
   def getInds(tp: Unary) : Iterator[Path] = individuals(tp).iterator
   /** retrieves type of an individual */
   def getType(p: Path) : Option[Unary] = types.get(p)
   /** checks if an individual has a given type */
   def hasType(p: Path, tp: Unary) : Boolean = individuals(tp) contains p
   /** retrieves all Relation declarations */
   def getDeps : Iterator[Relation] = dependencies.pairs map {case ((p,q), d) => Relation(d,p,q)}
   /** tests if there is a relation holds between two individuals */
   def hasDep(from: Path, to: Path, bin: Binary) = dependencies((from,to)) contains bin

   //def getObjects(d : Binary) = subjects.keys.filter(_._1 == d).map(_._2).toSet
   //def getSubjects(d : Binary) = objects.keys.filter(_._2 == d).map(_._1).toSet

   /** adds a RelationalElement */
   def +=(d : RelationalElement) {
      synchronized {
         log(d.toString)
         d match {
           case Relation(dep, subj, obj) =>
              subjects += ((dep, obj), subj)
              objects += ((subj, dep), obj)
              dependencies += ((subj, obj), dep)
           case Individual(p, tp) =>
              types(p) = tp
              individuals += (tp, p)
              /*
              p.ancestors match {
                 case `p` :: tail =>
                    tail.foldLeft(p)((q1,q2) => {objects += ((q2,Declares),q1) ; q2}) // seems to be necessary to add theories to their namespaces
                 case _ =>
              } */
         }
      }
   }

   /** deletes all RelationalElements with a given subject */
   def deleteSubject(subj : Path) {
      synchronized {
        types -= subj
        individuals.values.foreach {v => v -= subj}
        subjects.values.foreach {v => v -= subj}
        objects.keys.foreach {k =>
           if (k._1 == subj)
              objects -= k
        }
        dependencies.keys.foreach {k =>
           if (k._1 == subj)
              dependencies -= k
        }
      }
   }

   def queryList(start : Path, q : RelationExp) : List[Path] = {
      var ps : List[Path] = Nil
      query(start, q) {p => ps ::= p}
      ps
   }
   def querySet(start : Path, q : RelationExp) : HashSet[Path] = {
      var ps = new HashSet[Path]()
      query(start, q) {p => ps += p}
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
      case Transitive(qn) =>
         var added = HashSet.empty[Path]
         def step(p : Path) {
            if (! added.contains(p)) {
               //println("Added path "+p.toString()+" as a path related to the starting path "+start.toString()+" with search query "+q.toString())
               added += p
               val next = query(p, qn)(step)
               add(p) //add parent only after children
            }
         }
         val next = query(start, qn)(step)
      //the set of paths related to start by any of the relations in qs (in the order listed)
      case Choice(qs @ _*) => qs.foreach(q => query(start, q))
      //the set of paths related to start by making steps according to qs
      case Sequence(qs @ _*) => qs.toList match {
         case Nil => add(start)
         case hd :: tl => query(start, hd) {p => query(p, Sequence(tl : _*))}
      }
      //only start itself iff it has the right type
      case HasType(mustOpt,mustNot) =>
        val tpO = getType(start)
        val inMust = mustOpt match {
          case None => true
          case Some(must) => tpO.map {tp => must contains tp}.getOrElse(false)
        }
        lazy val notInMustNot = tpO.map(tp => ! (mustNot contains tp)).getOrElse(true)
        if (inMust && notInMustNot)
          add(start)
   }}

  def makeStatisticsFor(p:Path, q:RelationExp, prefix:String) = {
    val ds=querySet(p, q)
    val dsG = ds.toList.groupBy(x => getType(x)).toList flatMap {
      case (Some(t),l:List[Path]) => (List((prefix + t.toString,l.size)))
      case (None, _) => Nil
    }
    Statistics(dsG)
  }
   
  /*def makeImplicitDeclarationStatistics(p:Path, q:RelationExp, prefix:String) : List[(String, Int)] = {
    val holoThs = querySet(p, Transitive(ToSubject(+HasMeta | +Includes | +DependsOn | Reflexive)))
    val ds = querySet(p, q)
    val dsG = ds.toList.groupBy(x => getType(x)).toList flatMap {
      case (Some(t),l:List[Path]) => (List((prefix + "Defined Implicitly" + t.toString,l.size)))
      case (None, _) => Nil
    }
    dsG
  }*/
  
  def makeStatistics(p: Path) = {
    val decl = Transitive(ToObject(Declares))
    val align = Sequence(ToObject(Declares) | Transitive(ToObject(IsAlignedWith)))
    val morph = Transitive(+HasMeta | +Includes | +DependsOn | Reflexive)
    var dsG = makeStatisticsFor(p, decl, "")
    dsG += makeStatisticsFor(p, align, "Alignments of ")
    dsG += ("Induced theory morphisms", getTheoryMorphisms(p).size)
    dsG
  }

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
      this.synchronized {
         dependencies.clear
         subjects.clear
         objects.clear
         individuals.clear
         types.clear
      }
   }
	
	def getTheoryMorphisms(p : Path) = {
		val q = Transitive(+HasMeta | +Includes | +IsImplicitly)
		val q2 = q * Declares * HasType(IsConstant)
		querySet(p, q) //(individuals(IsTheory) map {th => querySet(th, q)}).flatten
	}	
}

case class Statistics(entries: List[(String,Int)]) {
  def +(that: Statistics): Statistics = {
    Statistics(entries ::: that.entries)
  }
  def +(s: String, n: Int): Statistics = {
    this + Statistics(List((s,n)))
  }
}
