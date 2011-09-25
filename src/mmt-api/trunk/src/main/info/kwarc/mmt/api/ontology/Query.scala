package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import utils._
import utils.MyList.fromList
import objects._
import libraries._
import scala.collection.mutable.{HashSet,HashMap}

trait BaseType

abstract class RProduct[A <: BaseType, B <: BaseType] extends BaseType {
   def asPair : (A,B)
}
case class RPair[A <: BaseType, B <: BaseType](first: A, second: B) extends RProduct[A,B] {
   def asPair : (A,B) = (first,second)
}

sealed abstract class Exp[S <: BaseType]
sealed abstract class Result[S <: BaseType]

case class AllOfType(tp: Unary) extends Result[Path]

case class Bound[S <: BaseType](index: Int) extends Exp[S]

case class ByURI(p: Path) extends Exp[Path]
case class ComponentOf(of: Exp[Path], component: String) extends Exp[Obj]
case class SubObject(of: Exp[Obj], position: Position) extends Exp[Obj]

case class Related(to: Exp[Path], by: Query) extends Result[Path]
case class Union[T <: BaseType](left: Result[T], right: Result[T]) extends Result[T]
case class Comprehension[S <: BaseType](domain: Result[S], pred : Prop) extends Result[S]
case class BigUnion[S <: BaseType, T <: BaseType](domain: Result[S], of: Result[T]) extends Result[T]

case class UnifiesWith(query: Obj) extends Result[RProduct[Path,Obj]]

sealed abstract class Prop
case class IsIn[S <: BaseType](elem: Exp[S], tp: Result[S]) extends Prop
case class IsEmpty[S <: BaseType](r: Result[S]) extends Prop
case class IsOfType(e: Exp[Path], t: Unary) extends Prop
case class PrefixOf(short: Exp[Path], long: Exp[Path]) extends Prop
case class Equal[S <: BaseType](left: Exp[S], right: Exp[S]) extends Prop 
case class And(left: Prop, right: Prop) extends Prop
case class Forall[S <: BaseType](domain: Result[S], scope: Prop) extends Prop

object Engine {
   def evaluate[S <: BaseType](e: Exp[S])(implicit lup: Lookup, context: List[BaseType]) : S = e match {
      case Bound(i) => context(i) match {
         case s: S => s
         case _ => throw ImplementationError("variable has wrong type")
      }
      case SubObject(of, pos) => pos.indices.foldLeft(evaluate(of))({
         case (o, i) => o.components(i) match {case s: Obj => s case _ => throw GetError("illegal position")}
      })
   }
   def evaluate[S <: BaseType](q: Result[S])(implicit lup: Lookup, context: List[BaseType]) : HashSet[S] = q match {
      case Comprehension(dom, pred) => evaluate(dom) filter {e => evaluate(pred)(lup, e :: context)}
      
   }
   def evaluate(p: Prop)(implicit lup: Lookup, context: List[BaseType]) : Boolean = p match {
      case IsIn(e,t) => evaluate(t) contains evaluate(e)
      case PrefixOf(short,long) => evaluate(short) <= evaluate(long)
      case Equal(e,f) => evaluate(e) == evaluate(f)
      case And(q,r) => evaluate(q) && evaluate(r)
   }
}
/**
 * Objects of type Query are expressions of a simple query language over an MMT ABox.
 * The semantics of a query is that it denotes a binary relation between MMT paths.
 */
sealed abstract class Query {
   /** syntactic sugar for queries: Choice(this, q) */
   def |(q: Query) = Choice(this, q)
   /** syntactic sugar for queries: Sequence(this, q) */
   def *(q: Query) = Sequence(this, q)
   /** the inverse/dual of this query */
   def unary_- : Query
}
/** base case: an atomic binary relation */
case class ToObject(dep : Binary) extends Query {
   def unary_- = ToSubject(dep)
   override def toString = "+ " + dep
}
/** base case: the inverse of an atomic binary relation */
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

/** the reflexive relation restricted to a unary predicate
 *  This permits the restriction of a result set to elements of a certain type.
 */
case class HasType(tp : Unary) extends Query {
   def unary_- = this
   override def toString = ":" + tp
}

/** The semantics of a Concept is a set of individuals */
abstract class Concept
/** The set of individuals listed */ 
case class OneOf(paths : Path*) extends Concept
/** The set of individuals of a certain type */
case class OfType(tp: Unary) extends Concept
/** The image of a concept under a relation, i.e., the set of individuals that are in the given relation with any of the given individuals. */ 
case class Relatives(of: Concept, by: Query) extends Concept


/** helper object for queries */
object Query {
    /** S has a structure declaration with domain O, defined as an abbreviation */
   def HasStructureFrom = Sequence(- HasCodomain, HasType(IsStructure), + HasDomain)
   def Imports = Choice(+ Includes, Query.HasStructureFrom)
    /** Disjunction of all binary relations, helpful for dependency closures */
   def AnyDep = Choice(Binary.all.map(+ _) : _*)
}