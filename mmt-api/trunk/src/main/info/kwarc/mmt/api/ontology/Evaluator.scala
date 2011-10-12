package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import utils._
import frontend._
import objects._
import libraries._
import scala.collection.mutable.{HashSet}

/** wrapper type for the result of a query */
sealed abstract class QueryResult {
   def toNode : scala.xml.Node
}
/** result of an element query */
case class ElemResult(l: List[BaseType]) extends QueryResult {
   def toNode : scala.xml.Node = <result>{l map {
      case p: Path => <uri path={p.toPath}/>
      case o: Obj => <object xmlns:om="http://www.openmath.org/OpenMath">{o.toNode}</object>
   }}</result>
}
/** result of a set query */
case class ESetResult(h : HashSet[List[BaseType]]) extends QueryResult {
   def toNode : scala.xml.Node =
      <results>{h map {x => ElemResult(x).toNode}}</results>
}

/** evaluates a query expression to a query result */
class Evaluator(controller: Controller) {
   private val rs = controller.depstore
   private val extman = controller.extman
   private val lup = controller.globalLookup
   
   /** evaluation of a query
    *  the result is typed according to the type of the query
    *  if the query is ill-formed, this will throw an exception  
    */
   def evaluate(q: Query) : QueryResult = Query.infer(q)(Nil) match {
      case Elem(_) => ElemResult(evaluateElem(q)(Nil))
      case ESet(_) => ESetResult(evaluateESet(q)(Nil))
   }
   /** pre: Query.infer(e) = Elem(_) */
   private def evaluateElem(q: Query)(implicit context: List[BaseType]) : List[BaseType] = evaluateESet(q).head
   /** pre: Query.infer(e) = Elem(List(b)) */
   private def evaluateElemBase(q: Query)(implicit context: List[BaseType]) : BaseType = evaluateElem(q).head
   /** pre: Query.infer(e) = Elem(PathType) */
   private def evaluateElemPath(e: Query)(implicit context: List[BaseType]) = evaluateElemBase(e).asInstanceOf[Path]
   /** pre: Query.infer(e) = Elem(ObjectType) */
   private def evaluateElemObj(e: Query)(implicit context: List[BaseType]) = evaluateElemBase(e).asInstanceOf[Obj]

   private class RHashSet extends HashSet[List[BaseType]] {
      def +=(b: BaseType) {this += List(b)}
   }
   private def empty = new RHashSet 
   private def singleton(b: BaseType) = {val res = empty; res += b; res}

   /** sets are evaluated to hash sets of the respective type
    *  pre: Query.infer(e) succeeds 
    */
   private def evaluateESet(q: Query)(implicit context: List[BaseType]) : HashSet[List[BaseType]] = q match {
      case Bound(i) => singleton(context(i-1))
      case ThePath(p) => singleton(p)
      case TheObject(o) => singleton(o)
      case SubObject(of, pos) => singleton(evaluateElemObj(of).subobject(pos))
      case Component(of, comp) =>
         val res = empty
         evaluateESet(of) foreach {p => 
            lup.get(p.asInstanceOf[Path]).contComponents(comp) match {
               case obj : Obj => res += List(obj)
               case _ => throw GetError("component exists but does not indicate an object: " + comp)
            }
         }
         res
      case InferedType(of) =>
         val res = empty
         //TODO: find an applicable foundation
         val found = extman.getFoundation(null).getOrElse(throw GetError("no applicable type inference engine defined"))
         evaluateESet(of) foreach {
            case t: Term => res += List(found.infer(t, Context())(lup))
            case o => throw GetError("object exists but is not a term: " + o)
         }
         res
      case ThePaths(es @ _*) =>
         val res = empty
         es foreach {res += _}
         res
      case TheObjects(es @ _*) =>
         val res = empty
         es foreach {res += _}
         res
      case Singleton(e) =>
         val res = empty
         res += evaluateElem(e)
         res
      case AllThatAre(c) =>
         val res = empty
         rs.getInds(c) foreach {res += _}
         res
      case Union(s,t) => evaluateESet(s) union evaluateESet(t)
      case BigUnion(dom,s) =>
         val res = empty
            evaluateESet(dom) foreach {
            e => evaluateESet(s)(e:::context) foreach {x => res += x}
         }
         res
      case Comprehension(dom, pred) => evaluateESet(dom) filter {e => evaluate(pred)(e ::: context)}
      case Unifies(wth) => empty //TODO empty for now
      case Related(to, by) =>
         val res = empty
         rs.query(evaluateElemPath(to), by)(res += _)
         res
      case Closure(of) =>
         evaluateESet(of) match { 
            case p : MPath =>
               val res = empty
               rs.theoryClosure(p) foreach {res += _}
               res
            case p => throw GetError("must be a module path " + p)
         }
      case Tuple(l) =>
         val res = empty
         res += l map {q => evaluateElemBase(q)}
         res
      case Projection(q,i) =>
        val t = evaluateElem(q)
        val res = empty
        res += t(i)
        res
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
      case Forall(dom, sc) => evaluateESet(dom) forall {e => evaluate(sc)(e:::context)}
   }
}