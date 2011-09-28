package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import objects._
import utils._
import scala.xml.Node

/** types of the query language; atomic types are paths and objects */
trait BaseType

/** type formation for product types */ 
abstract class RProduct[A <: BaseType, B <: BaseType] extends BaseType {
   def asPair : (A,B)
}
/** introductory form for product types (pairs) */
case class RPair[A <: BaseType, B <: BaseType](first: A, second: B) extends RProduct[A,B] {
   def asPair : (A,B) = (first,second)
}

/** elements of a type */
sealed abstract class Elem[S <: BaseType]
/** set elements of a type */
sealed abstract class ESet[S <: BaseType]
/** propositions */
sealed abstract class Prop

/** auxiliary trait to formalize which constructors introduce bound variables */
trait Binder

/** a bound variable; we use de-Bruijn indices starting from 0 to refer to bound variables */
case class Bound[S <: BaseType](index: Int) extends Elem[S]

/** constants */
case class TheElem[A <: BaseType](a: A) extends Elem[A]
/** component of a declaration with a certain Path */
case class Component(of: Elem[Path], component: String) extends Elem[Obj]
/** subobject of an object at a certain position */
case class SubObject(of: Elem[Obj], position: Position) extends Elem[Obj]
/** type of an object */
case class InferedType(of: Elem[Obj]) extends Elem[Obj]
/** the set of all elements related to a certain path by a certain relation */
case class Related(to: Elem[Path], by: RelationExp) extends ESet[Path]

/** finite sets */
case class TheSet[A <: BaseType](as: A*) extends ESet[A]
/** singleton sets */
case class Singleton[A <: BaseType](e: Elem[A]) extends ESet[A]
/** the set of all elements of a certain concept in the MMT ontology */ 
case class AllThatAre(tp: Unary) extends ESet[Path]
/** unification query; returns all objects in the database that unify with a certain object and the respective substitutions */
case class Unifies(wth: Obj) extends ESet[RProduct[Path,Obj]]
/** dependency closure of a path */
case class Closure(of: Elem[Path]) extends ESet[Path]
/** union of sets */
case class Union[T <: BaseType](left: ESet[T], right: ESet[T]) extends ESet[T]
/** union of a collection of sets indexed by a set */
case class BigUnion[S <: BaseType, T <: BaseType](domain: ESet[S], of: ESet[T]) extends ESet[T] with Binder
/** comprehension subset of a set */
case class Comprehension[S <: BaseType](domain: ESet[S], pred : Prop) extends ESet[S] with Binder

/** typing relation between a path and a concept of the MMT ontology */
case class IsA(e: Elem[Path], t: Unary) extends Prop
/** ancestor relation between paths */
case class PrefixOf(short: Elem[Path], long: Elem[Path]) extends Prop
/** element relation between elements and sets */
case class IsIn[S <: BaseType](elem: Elem[S], tp: ESet[S]) extends Prop
/** emptiness of a set */
case class IsEmpty[S <: BaseType](r: ESet[S]) extends Prop
/** equality of elements */
case class Equal[S <: BaseType](left: Elem[S], right: Elem[S]) extends Prop 
/** negation */
case class Not(arg: Prop) extends Prop
/** conjunction */
case class And(left: Prop, right: Prop) extends Prop
/** universal quantification over a set */
case class Forall[S <: BaseType](domain: ESet[S], scope: Prop) extends Prop with Binder

/**
 * binary relations between paths, i.e., relation in the MMT ontology
 */
sealed abstract class RelationExp {
   /** syntactic sugar for queries: Choice(this, q) */
   def |(q: RelationExp) = Choice(this, q)
   /** syntactic sugar for queries: Sequence(this, q) */
   def *(q: RelationExp) = Sequence(this, q)
   /** the inverse/dual of this query */
   def unary_- : RelationExp
}
/** base case: an atomic binary relation */
case class ToObject(dep : Binary) extends RelationExp {
   def unary_- = ToSubject(dep)
   override def toString = "+ " + dep
}
/** base case: the inverse of an atomic binary relation */
case class ToSubject(dep : Binary) extends RelationExp {
   def unary_- = ToObject(dep)
   override def toString = "- " + dep
}
/** the transitive closure of a relation */
case class Transitive(q : RelationExp) extends RelationExp {
   def unary_- = Transitive(- q)
   override def toString = "(" + q + ")*"
}
/** the union of a list of relations */
case class Choice(qs : RelationExp*) extends RelationExp {
   def unary_- = Choice(qs.map(- _) : _*)
   override def toString = qs.mkString(""," | ", "")
}
/** the composition of a list of relations */
case class Sequence(qs : RelationExp*) extends RelationExp {
   def unary_- = Sequence(qs.reverse.map(- _) : _*)
   override def toString = qs.mkString(""," ; ", "")
}
/** the reflexive relation */
case object Reflexive extends RelationExp {
   def unary_- = this
   override def toString = "Id"
}

/** the reflexive relation restricted to a unary predicate
 *  This permits the restriction of a result set to elements of a certain type.
 */
case class HasType(tp : Unary) extends RelationExp {
   def unary_- = this
   override def toString = ":" + tp
}

/** helper object for queries */
object RelationExp {
   def parse(n: Node) : RelationExp = n match {
      case <sequence>{r @ _*}</sequence> => Sequence(r map parse : _*)
      case <choice>{r @ _*}</choice> => Choice(r map parse : _*)
      case <transitive>{r}</transitive> => Transitive(parse(r))
      case <reflexive/> => Reflexive
      case <inverse>{r}</inverse> => - parse(r)
      case <toobject/> => + Binary.parse(xml.attr(n, "relation"))
      case <tosubject/> => - Binary.parse(xml.attr(n, "relation"))
      case n => throw ParseError("illegal relation expression: " + n)
   }
    /** S has a structure declaration with domain O, defined as an abbreviation */
   def HasStructureFrom = Sequence(- HasCodomain, HasType(IsStructure), + HasDomain)
   def Imports = Choice(+ Includes, RelationExp.HasStructureFrom)
    /** Disjunction of all binary relations, helpful for dependency closures */
   def AnyDep = Choice(Binary.all.map(+ _) : _*)
}

object Prop {
   def parse(n: Node) : Prop = n match {
      case <equal>{e}{f}</equal> => xml.attr(n, "type") match {
         case "path" => Equal(Elem.parsePath(e), Elem.parsePath(f))
         case "object" => Equal(Elem.parseObj(e), Elem.parseObj(f))
      }
      case <and>{f}{g}</and> => And(parse(f), parse(g))
      case <not>{f}</not> => Not(parse(f))
      case <forall>{d}{f}</forall> => xml.attr(n, "type") match {
         case "path" => Forall(ESet.parsePath(d), parse(f))
         case "object" => Forall(ESet.parseObj(d), parse(f))
      }
      //TODO missing cases
      case n => throw ParseError("illegal proposition: " + n)
   }
}

object Elem {
   def parsePath(n: Node) : Elem[Path] = n match {
      case <individual/> => TheElem(Path.parse(xml.attr(n, "uri")))
      case n => parseGeneric[Path](n)
   }
   def parseObj(n: Node) : Elem[Obj] = n match {
      case <component>{o}</component> => Component(parsePath(o), xml.attr(n, "index"))
      case <subobject>{o}</subobject> => SubObject(parseObj(o), Position.parse(xml.attr(n, "position")))
      case <type>{o}</type> => InferedType(parseObj(o))
      case n => parseGeneric[Obj](n)
   }
   def parseGeneric[A <: BaseType](n: Node) : Elem[A] = n match {
      case <var/> =>
         val s = xml.attr(n, "index")
         val i = try {s.toInt} catch {case _ => throw ParseError("illegal variable index: " + s)}
         Bound[A](i)
      case n => throw ParseError("illegal element expression: " + n)
   }
}

object ESet {
   def parsePath(n: Node) : ESet[Path] = n match {
      case <related>{to}{by}</related> => Related(Elem.parsePath(to), RelationExp.parse(by))
      case <closure>{of}</closure> => Closure(Elem.parsePath(of))
      case n => parseGeneric[Path](n)
   }
   def parseObj(n: Node) : ESet[Obj] = parseGeneric[Obj](n)
   def parseGeneric[A <: BaseType](n: Node) : ESet[A] = n match {
      case <singleton>{e}</singleton> => Singleton(Elem.parseGeneric[A](e))
      case <union>{l}{r}</union> => Union(parseGeneric[A](l), parseGeneric[A](r))
      case <bigunion>{d}{s}</bigunion> =>
         val dom = xml.attr(n, "type") match {
            case "path" => ESet.parsePath(d)
            case "object" => ESet.parseObj(d)
         }
         BigUnion(dom, parseGeneric[A](s))
      case <comprehension>{d}{f}</comprehension> =>
         Comprehension(ESet.parseGeneric[A](d), Prop.parse(f))
      //TODO missing cases
      case n => throw ParseError("illegal set expression: " + n)
   }
}

/*
<type><subobject position="2_2"><component index="type"><individual uri="http://latin.omdoc.org/logics/syntax?CONJ?and"/></component></subobject></type>
<related><individual uri="http://latin.omdoc.org/logics/syntax?FOL"/><sequence><transitive><toobject relation="Includes"/></transitive><toobject relation="Declares"/></sequence></related>
*/