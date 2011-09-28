package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import utils._
import objects._
import libraries._
import scala.collection.mutable.{HashSet}

/** evaluates a query expression */
class Evaluator(rs: RelStore, lup: Lookup) {
   val lff = new lf.LFF(frontend.NullReport)
   /** elements are evaluated to values of the respective type */
   def evaluate[S <: BaseType](e: Elem[S])(implicit context: List[BaseType]) : S = e match {
      case Bound(i) => context(i) match {
         case s: S => s
         case _ => throw ImplementationError("variable has wrong type")
      }
      case TheElem(e) => e
      case Component(of, comp) => lup.get(evaluate(of)).contComponents(comp) match {
         case obj : Obj => obj
         case _ => throw GetError("component exists but does not indicate an object: " + comp)
      }
      case SubObject(of, pos) => evaluate(of).subobject(pos)
      case InferedType(of) => evaluate(of) match {
         case t: Term => lff.infer(t, Context())(lup)
      }
   }
   /** sets are evaluated to hash sets of the respective type */
   def evaluate[S <: BaseType](set: ESet[S])(implicit context: List[BaseType]) : HashSet[S] = set match {
      case TheSet(es @ _*) =>
         val res = new HashSet[S]
         es foreach {res += _}
         res
      case Singleton(e) =>
         val res = new HashSet[S]
         res += evaluate(e)
         res
      case AllThatAre(c) =>
         val res = new HashSet[Path]
         rs.getInds(c) foreach {res += _}
         res
      case Union(s,t) => evaluate(s) union evaluate(t)
      case BigUnion(dom,s) =>
         val res = new HashSet[S]
         evaluate(dom) foreach {
            e => evaluate(s)(e::context) foreach {x => res += x}
         }
         res
      case Comprehension(dom, pred) => evaluate(dom) filter {e => evaluate(pred)(e :: context)}
      case Unifies(wth) => new HashSet[S] //TODO empty for now
      case Related(to, by) =>
         val res = new HashSet[S]
         rs.query(evaluate(to), by)(res += _)
         res
      case Closure(of) =>
         evaluate(of) match { 
            case p : MPath =>
               val res = new HashSet[Path]
               rs.theoryClosure(p) foreach {res += _}
               res
            case p => throw GetError("must be a module path " + p)
         }
   }
   /** propositions are evaluated to booleans */
   def evaluate(p: Prop)(implicit context: List[BaseType]) : Boolean = p match {
      case IsA(e,c) => rs.getInds(c) contains evaluate(e)
      case PrefixOf(short,long) => evaluate(short) <= evaluate(long)
      case IsIn(e,t) => evaluate(t) contains evaluate(e)
      case IsEmpty(s) => evaluate(s).isEmpty
      case Equal(e,f) => evaluate(e) == evaluate(f)
      case Not(q) => ! evaluate(q)
      case And(q,r) => evaluate(q) && evaluate(r)
      case Forall(dom, sc) => evaluate(dom) forall {e => evaluate(sc)(e::context)}
   }
}