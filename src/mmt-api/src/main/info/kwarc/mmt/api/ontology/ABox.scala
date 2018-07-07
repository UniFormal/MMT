package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.utils.MyList.fromList
import info.kwarc.mmt.api.objects._
import scala.collection.mutable.{HashSet,HashMap}

import scala.util.control.Exception.Catch

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

   //var test : List[Individual] = Nil
   
   /** adds a RelationalElement */
   def +=(d : RelationalElement) {
      synchronized {
         d match {
           case Relation(dep, subj, obj) =>
              subjects += ((dep, obj), subj)
              objects += ((subj, dep), obj)
              dependencies += ((subj, obj), dep)
           case ind @ Individual(p, tp) =>
              types(p) = tp
              individuals += (tp, p)
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

   /**
    * Discriminates the different sorts of constants into types untyped constant, data constructor, datatype constructor, 
    * rule, judgement constructor, high universe (kind level constructors and higher) and typed constant (fallback),
    * @param s the type of the constant
    * @param p the path of the constant
    * @note precondition: Every constant is annotated as having exactly one of the types untyped constant, data constructor,
    * datatype constructor, rule, judgement constructor, high universe
    */
   def mapConstant(s:Unary, p:Path) : (StatType, Path) = {
     if (s == IsConstant) {
       try {
         val desiredTypes : List[Unary] = List(IsDataConstructor, IsDatatypeConstructor, IsRule, IsJudgementConstructor, IsHighUniverse, IsUntypedConstant)
         val un = desiredTypes find {tp => hasType(p, tp)}
         val entry = un getOrElse {
           IsConstant
         }
         var entries : List[(StatType, Path)] = Nil
         entry match {
           case IsDatatypeConstructor => entries::=(DatatypeConstructorEntry(), p)
           case IsDataConstructor => entries::=(DataConstructorEntry(), p)
           case IsRule => entries::=(RuleEntry(), p)
           case IsJudgementConstructor => entries::=(JudgementConstructorEntry(), p)
           case IsHighUniverse => entries::=(HighUniverseEntry(), p)
           case IsUntypedConstant => entries::=(UntypedConstantEntry(), p)
           case IsConstant => 
           case _ => log("Found unexpected Individual: "+p.toPath)
         }
         if (entries.length > 0) {
           if (entries.length >1)
             log ("clashing relational data for: "+p.toPath+": "+entries.toString())
           entries.head
         } else {
           //log ("Failed to identify the type of declaration of "+p.toPath+" (most likely an error building the archive). ")
           //log("Please update the relational files. ")
           (TypedConstantEntry(),p)
         }
       } catch {
         case t: Throwable => 
           //t.printStackTrace()
           log("Maltyped constant at: "+p.toString())
           (MaltypedConstantEntry(), p)
       }
     }
     else {
       s match {
         case IsTheory => (TheoryEntry(), p)
         case IsDocument => (DocumentEntry(), p)
         case IsView => (ViewEntry(), p)
         case IsStructure => (StructureEntry(), p)
         case IsPattern => (PatternEntry(), p)
         case _ => (AnyOtherEntry(), p)//(new StatType(s.toString),p)
       }
     }
   }
   
   /**
    * Discriminates the a list of constants into the types untyped constant, data constructor, datatype constructor, 
    * rule, judgement constructor, high universe and typed constant (fallback),
    * @param p a tuple containing the unary "type" of the individual and the list of constants of that type
    */
   def mapConstants(p:(Option[Unary], List[Path])) : List[(StatType, Path)]= {
     p match {
       case (Some(t),x::l) => mapConstant(t, x)::mapConstants((Some(t),l))
       case (Some(p), Nil) => Nil
       case (None, _) => Nil
     }
   }
   
  /**
    * Make a statistic for the given query and prepend the prefix to the descriptions of the found declarations
    * @param p the path of the document of theory to make the statistic for
    * @param q the query
    */ 
  def makeStatisticsFor(p:Path, q:RelationExp, prefix:Option[StatPrefixEntry]) = {
    val pre = prefix.getOrElse(NoPrefix())
    val ds=querySet(p, q)
    val dsGl = ds.toList.groupBy(x => getType(x)).toList flatMap {x => mapConstants(x)}
    val bla : List[(StatType,List[(StatType,Path)])] = dsGl.groupBy({case (s,x)=>s}).toList
    val dsG : List[StatisticEntry] = bla flatMap {
      case (s:StatType,l) => (List(StatisticEntry(s,l.size)))
    }
    Statistics(List((pre, dsG)))
  }
   
  /**
    * Make a statistic for the document or theory at the given path
    * @param p the path of the document or theory
    */
  def makeStatistics(q: Path) = {
    if (q.toString() == "http://docs.omdoc.org/examples")
    {}
    val decl = Transitive(+Declares)
    val subtheory = (decl | Reflexive) * HasType(IsTheory)
    val align = decl * Transitive(+IsAlignedWith)
    // val subtheory = Transitive(+Declares *HasType(IsTheory) | Reflexive)
    val expMorph = decl * Transitive(+HasViewFrom)
    val morph = decl * Transitive(+HasMeta | +Includes | +IsImplicitly | +HasViewFrom)
    val expinduced = expMorph * decl * HasType(IsConstant)
    val induced = subtheory * morph * decl * HasType(IsConstant)
    var dsG = makeStatisticsFor(q, decl, None)
    dsG += makeStatisticsFor(q, align, Some(AlignmentPrefix()))
    val (exMorph, anyMorph) = (querySet(q, expMorph).size, querySet(q, morph).size)
    if (exMorph > 0)
      dsG += (ExplicitMorphismEntry(), exMorph, None)
    if (anyMorph > 0)
      dsG += (AnyMorphismEntry(), anyMorph, None)
    dsG += makeStatisticsFor(q, expinduced, Some(ExplicitMorphismPrefix()))
    dsG += makeStatisticsFor(q, align, Some(AnyMorphismPrefix()))
    dsG
  }
  
  def statDescription : JSONArray = {
    val prefsClasses = List(NoPrefix(), ExplicitMorphismPrefix(), AnyMorphismPrefix(), AlignmentPrefix())
    val statClasses = List(TheoryEntry(), DocumentEntry(), UntypedConstantEntry(), TypedConstantEntry(), MaltypedConstantEntry(), 
        StructureEntry(), PatternEntry(), JudgementConstructorEntry(), DataConstructorEntry(), DatatypeConstructorEntry(), 
        RuleEntry(), ViewEntry(), HighUniverseEntry(), ExplicitMorphismEntry(), AnyMorphismEntry())
    val prefs = JSONArray.fromList(prefsClasses map(_.toJSON))
    val entries = JSONArray.fromList(statClasses map(_.toJSON))
    JSONArray.fromList(List(JSONObject("sorts of relations to the declaration" -> prefs), JSONObject("types of declarations" -> entries)))
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
}

//The type of a statistics for a theory, document or archive
case class Statistics(entries: List[(StatPrefixEntry, List[StatisticEntry])]) {
  def +(s: StatisticEntry, pre:StatPrefixEntry): Statistics = {
    val entriesRes = (!(entries.map(_._1.getKey) contains pre.getKey)) match {
      case true => 
        (pre, List(s))::entries
      case false =>
        val ent = entries map {
          case (p, stats) if (p.getKey == pre.getKey) =>
            if (stats map(_.stat) contains s.stat) {
              val statsRes = stats map {
                case e if (e.stat.getKey != s.stat.getKey) => e
                case e if (e.stat.getKey == s.stat.getKey) => StatisticEntry(e.stat, e.n+s.n)
              }
              (p, statsRes)
            } else
              (p, s::stats)
          case e => e
        }
      ent
    }
    Statistics(entriesRes)
  }
  def +(that: (StatPrefixEntry, List[StatisticEntry])) : Statistics = {
    var stat : Statistics = this
    def f(p:(StatisticEntry, StatPrefixEntry)) : Unit = stat = stat+(p._1, p._2)
    that._2 map {s => (s, that._1)} foreach f
    stat
  }
  
  def +(that: Statistics): Statistics = {
    var stat = this
    def f(e:(StatPrefixEntry, List[StatisticEntry])) = stat = stat + e
    that.entries foreach f
    stat
  }
  def +(s: StatType, n: Int, pre:Option[StatPrefixEntry]): Statistics = {
    this + (StatisticEntry(s, n), pre.getOrElse(NoPrefix()))
  }
  def empty = Statistics(Nil)
  /**
    * Returns a JSON representation of the statistics
    */
  def toJSON : JSONArray = {
    val jstats = entries map {case (pre, substat) => JSONObject(pre.getKey -> JSONArray.fromList(substat.map(_.toJSON)))}
    JSONArray.fromList(jstats)
  }
}

sealed class StatPrefixEntry(key:String, teaser:String, description:String) {
  def getKey = key
  def getTeaser = teaser
  def getDescription = description
  def ==(other : StatType) = this.getKey == other.getKey
  def toJSON = JSONArray.fromList((List(JSONObject("key"->JSONString(key)), JSONObject("teaser"->JSONString(teaser)), JSONObject("description"->JSONString(description)))))
}
case class NoPrefix() extends StatPrefixEntry("decl", "Declared declaration", 
    "Declarations declared (possibly via multiple hops) by the given theory/document/archive")
case class ExplicitMorphismPrefix() extends StatPrefixEntry("exp_mor", "Induced declaration by explicit morphisms (views)", 
    "Declarations induced via (possibly multiple concatenated) explicit theory morphisms (aka. views)")
case class AnyMorphismPrefix() extends StatPrefixEntry("any_mor", "Induced declaration by any morphisms", 
    "Declarations induced via any (possibly multiple concatenated) theory morphisms (views, includes, metas and implicit morphisms)")
case class AlignmentPrefix() extends StatPrefixEntry("align", "Alignment", 
    "Declarations aligned (possibly via multiple intermediate alignments) with declaration declared by the given theory/document/archive")


//The types of the entries of the statistics to be generated
case class StatisticEntry(stat : StatType, n:Int) {
  def +(other : StatisticEntry) : List[StatisticEntry] = {
    if (stat.getKey == other.getStat.getKey)
      List(StatisticEntry(stat, n+other.getN))
    else 
      List(this, other)
  }
  def :+(stats : List[StatisticEntry]) : List[StatisticEntry] = {
    if (stats map(_.getStat) contains stat) {
      stats map {
        case x if (x.getStat.getKey == stat.getKey) => StatisticEntry(stat, n+x.getN)
        case y => y
      }
    } else 
      this::stats
  }
  def getStat = {stat}
  def getN = {n}
  def toJSON : JSONObject = JSONObject(stat.getKey -> JSONInt(n))
}

sealed class StatType(key:String, teaser:String, description: String) {
  def getKey = {key}
  def getTeaser = {teaser}
  def getDescription = {description}
  def ==(other : StatType) = {this.getKey == other.getKey}
  def toJSON = JSONArray.fromList((List(JSONObject("key"->JSONString(key)), JSONObject("teaser"->JSONString(teaser)), JSONObject("description"->JSONString(description)))))
}

case class TheoryEntry() extends StatType("theo", "theory", "theory")
case class DocumentEntry() extends StatType("doc", "document", "document")
case class UntypedConstantEntry() extends StatType("unty_con", "untyped constant", "declaration of untyped constant")
case class TypedConstantEntry() extends StatType("ty_con", "typed constant", "declaration of typed constant")
case class MaltypedConstantEntry() extends StatType("mal_con", "maltyped constant", "maltyped (type can't be infered) constant declaration")
case class StructureEntry() extends StatType("struc", "structure", "structure")
case class PatternEntry() extends StatType("pat", "pattern", "pattern")
case class JudgementConstructorEntry() extends StatType("judg", "judgement constructor", "judgement constructor")
case class DataConstructorEntry() extends StatType("data", "data constructor", "data constructor")
case class DatatypeConstructorEntry() extends StatType("type", "datatype constructor", "datatype constructor")
case class RuleEntry() extends StatType("rule", "rule", "rule declarations")
case class ViewEntry() extends StatType("view", "view", "views in the theory")
case class HighUniverseEntry() extends StatType("high", "high universe", "constructors returning kinds or object in even higher typing universes")
case class ExplicitMorphismEntry() extends StatType("exp_mor", "explicit theory morphism", "the transitive closure of all theory morphisms into the theory")
case class AnyMorphismEntry() extends StatType("any_mor", "any theory morphism", "the transitive closure of all theory morphisms into the theory")
case class AnyOtherEntry() extends StatType("other", "other", "other")