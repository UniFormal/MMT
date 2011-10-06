package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import utils._
import objects._
import libraries._
import scala.collection.mutable.{HashSet}

/** wrapper type for the result of a query */
sealed abstract class QueryResult {
   def toNode : scala.xml.Node
}
/** result of an element query */
case class ElemResult(s: BaseType) extends QueryResult {
   def toNode : scala.xml.Node = s match {
      case p: Path => <result path={p.toPath}/>
      case o: Obj => <result xmlns:om="http://www.openmath.org/OpenMath">{o.toNode}</result>
   }
}
/** result of a set query */
case class ESetResult(h: HashSet[BaseType]) extends QueryResult {
   def toNode : scala.xml.Node =
      <results>{h map {x => ElemResult(x).toNode}}</results>
}

/** evaluates a query expression to a query result */
class Evaluator(rs: RelStore, lup: Lookup) {
   private val lff = new lf.LFF(frontend.NullReport)
   
   /** evaluation of a query
    *  the result is typed according to the type of the query
    *  if the query is ill-formed, this will throw an exception  
    */
   def evaluate(q: Query) : QueryResult = Query.infer(q)(Nil) match {
      case Elem(_) => ElemResult(evaluateElem(q)(Nil))
      case ESet(_) => ESetResult(evaluateESet(q)(Nil))
   }
   /** pre: Query.infer(e) = Elem(_) */
   private def evaluateElem(q: Query)(implicit context: List[BaseType]) : BaseType = evaluateESet(q).head
   /** pre: Query.infer(e) = Elem(PathType) */
   private def evaluateElemPath(e: Query)(implicit context: List[BaseType]) = evaluateElem(e).asInstanceOf[Path]
   /** pre: Query.infer(e) = Elem(ObjectType) */
   private def evaluateElemObj(e: Query)(implicit context: List[BaseType]) = evaluateElem(e).asInstanceOf[Obj]
   private def singleton(b: BaseType) = {val res = new HashSet[BaseType]; res += b; res} 
   /** sets are evaluated to hash sets of the respective type
    *  pre: Query.infer(e) succeeds 
    */
   private def evaluateESet(q: Query)(implicit context: List[BaseType]) : HashSet[BaseType] = q match {
      case Bound(i) => singleton(context(i))
      case ThePath(p) => singleton(p)
      case TheObject(o) => singleton(o)
      case SubObject(of, pos) => singleton(evaluateElemObj(of).subobject(pos))
      case Component(of, comp) =>
         val res = new HashSet[BaseType]
         evaluateESet(of) foreach {p => 
            lup.get(p.asInstanceOf[Path]).contComponents(comp) match {
               case obj : Obj => res += obj
               case _ => throw GetError("component exists but does not indicate an object: " + comp)
            }
         }
         res
      case InferedType(of) =>
         val res = new HashSet[BaseType]
         evaluateESet(of) foreach {
            case t: Term => res += lff.infer(t, Context())(lup)
            case o => throw GetError("object exists but is not a term: " + o)
         }
         res
      case ThePaths(es @ _*) =>
         val res = new HashSet[BaseType]
         es foreach {res += _}
         res
      case TheObjects(es @ _*) =>
         val res = new HashSet[BaseType]
         es foreach {res += _}
         res
      case Singleton(e) => singleton(evaluateElem(e))
      case AllThatAre(c) =>
         val res = new HashSet[BaseType]
         rs.getInds(c) foreach {res += _}
         res
      case Union(s,t) => evaluateESet(s) union evaluateESet(t)
      case BigUnion(dom,s) =>
         val res = new HashSet[BaseType]
            evaluateESet(dom) foreach {
            e => evaluateESet(s)(e::context) foreach {x => res += x}
         }
         res
      case Comprehension(dom, pred) => evaluateESet(dom) filter {e => evaluate(pred)(e :: context)}
      case Unifies(wth) => new HashSet[BaseType] //TODO empty for now
      case Related(to, by) =>
         val res = new HashSet[BaseType]
         rs.query(evaluateElemPath(to), by)(res += _)
         res
      case Closure(of) =>
         evaluateESet(of) match { 
            case p : MPath =>
               val res = new HashSet[BaseType]
               rs.theoryClosure(p) foreach {res += _}
               res
            case p => throw GetError("must be a module path " + p)
         }
   }
   /** propositions are evaluated to booleans */
   def evaluate(p: Prop)(implicit context: List[BaseType]) : Boolean = p match {
      case IsA(e,c) => rs.getInds(c) contains evaluateElemPath(e)
      case PrefixOf(short,long) => evaluateElemPath(short) <= evaluateElemPath(long)
      case IsIn(e,t) => evaluateESet(t) contains evaluateElem(e)
      case IsEmpty(s) => evaluateESet(s).isEmpty
      case Equal(e,f) => evaluateElem(e) == evaluateElem(f)
      case Not(q) => ! evaluate(q)
      case And(q,r) => evaluate(q) && evaluate(r)
      case Forall(dom, sc) => evaluateESet(dom) forall {e => evaluate(sc)(e::context)}
   }
}