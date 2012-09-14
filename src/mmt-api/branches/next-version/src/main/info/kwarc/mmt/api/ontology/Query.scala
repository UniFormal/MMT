package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import objects._
import utils._
import scala.xml.Node

/** base types, used in the formation of QueryType */
sealed abstract class QueryBaseType
/** queries regarding the BaseType Path */
case object PathType extends QueryBaseType
/** queries regarding the BaseType Obj */
case object ObjType extends QueryBaseType
/** queries returning arbitrary XML */
case object XMLType extends QueryBaseType
/** queries returning arbitrary strings */
case object StringType extends QueryBaseType

/** the types of the query language */
sealed abstract class QueryType
/** type of query that returns an element of a base type */
case class Elem(tp: List[QueryBaseType]) extends QueryType
/** type oa a query that returns a set of values of a base type */
case class ESet(tp: List[QueryBaseType]) extends QueryType

object Elem {
   def apply(b: QueryBaseType) : QueryType = Elem(List(b))
}
object ESet {
   def apply(b: QueryBaseType) : QueryType  = ESet(List(b)) 
}
object Elem1 {
   def unapply(t: QueryType) : Option[QueryBaseType] = t match {
      case Elem(List(b)) => Some(b)
      case _ => None
   }
}
object ESet1 {
   def unapply(t: QueryType) : Option[QueryBaseType] = t match {
      case ESet(List(b)) => Some(b)
      case _ => None
   }
}

/** propositions */
sealed abstract class Prop

/** the expressions of the query language */
sealed abstract class Query

/** a bound variable; we use de-Bruijn indices starting from 1 to refer to bound variables
 *  if a variable of tuple type is bound, each component is added separately, the first one has index 1
 */
case class Bound(index: Int) extends Query
/** constant path */
case class ThePath(a: Path) extends Query
/** constant object */
case class TheObject(a: Obj) extends Query
/** component of a declaration with a certain Path */
case class Component(of: Query, component: String) extends Query
/** subobject of an object at a certain position */
case class SubObject(of: Query, position: Position) extends Query
/** type of an object */
case class InferedType(of: Query, found: MPath) extends Query
/** the set of all elements related to a certain path by a certain relation */
case class Related(to: Query, by: RelationExp) extends Query

/** finite set of paths */
case class ThePaths(as: Path*) extends Query
/** finite set of objects */
case class TheObjects(as: Obj*) extends Query
/** singleton sets */
case class Singleton(e: Query) extends Query
/** the set of all elements of a certain concept in the MMT ontology */ 
case class AllThatAre(tp: Unary) extends Query
/** unification query; returns all objects in the database that unify with a certain object and the respective substitutions */
case class Unifies(wth: Obj) extends Query
/** dependency closure of a path */
case class Closure(of: Query) extends Query
/** union of sets */
case class Union(left: Query, right: Query) extends Query
/** union of a collection of sets indexed by a set */
case class BigUnion(domain: Query, of: Query) extends Query
/** comprehension subset of a set */
case class Comprehension(domain: Query, pred : Prop) extends Query
/** tupling */
case class Tuple(components: List[Query]) extends Query
/** projectiong */
case class Projection(of: Query, index: Int) extends Query

/** the XML object arising from rendering an object */
case class Present(content: Query, style: MPath) extends Query

/** typing relation between a path and a concept of the MMT ontology */
case class IsA(e: Query, t: Unary) extends Prop
/** ancestor relation between paths */
case class PrefixOf(short: Query, long: Query) extends Prop
/** element relation between elements and sets */
case class IsIn(elem: Query, tp: Query) extends Prop
/** emptiness of a set */
case class IsEmpty(r: Query) extends Prop
/** equality of elements */
case class Equal(left: Query, right: Query) extends Prop 
/** negation */
case class Not(arg: Prop) extends Prop
/** conjunction */
case class And(left: Prop, right: Prop) extends Prop
/** universal quantification over a set */
case class Forall(domain: Query, scope: Prop) extends Prop


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

/** helper object for relation expressions */
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

/** helper object for query expressions */
object Query {
   /** checks a query against a query type
    *  throws an exception if the query is ill-formed
    */
   def check(q: Query, tp : QueryType)(implicit context: List[QueryBaseType]) {
      val it = infer(q)
      if (it != tp) {
         throw ParseError("illegal query: " + q + "\nexpected: " + tp + "\nfound: " + it)
      }
   }
   /** infers the type of a query
    *  throws an exception if the query is ill-formed
    */
   def infer(q: Query)(implicit context: List[QueryBaseType]) : QueryType = q match {
      case Bound(i) => try {Elem(context(i-1))} catch {case _ => throw ParseError("illegal variable index: " + i)}
      case ThePath(_) => Elem(PathType)
      case TheObject(_) => Elem(ObjType)
      case Component(of, _) =>
         infer(of) match {
            case Elem1(PathType) => Elem(ObjType)
            case ESet1(PathType) => ESet(ObjType)
            case _ => throw ParseError("illegal query: " + q)
         }
      case SubObject(of,_) =>
         check(of, Elem(ObjType))
         Elem(ObjType)
      case InferedType(of, p) =>
         infer(of) match {
            case Elem1(ObjType) => Elem(ObjType)
            case ESet1(ObjType) => ESet(ObjType)
            case _ => throw ParseError("illegal query: " + q)
         }
      case Related(to,_) =>
         infer(to) match {
            case Elem1(PathType) => ESet(PathType)
            case ESet1(PathType) => ESet(PathType)
            case t => throw ParseError("ill-typed query: " + to + " expected path or set of paths, found " + t)
         }
      case ThePaths(_*) => ESet(PathType)
      case TheObjects(_*) => ESet(ObjType)
      case Singleton(it) =>
         infer(it) match {
            case Elem(t) => ESet(t)
            case _ => throw ParseError("expected element query, found set query " + q)
         }
      case AllThatAre(_) => ESet(PathType)
      case Unifies(_) => ESet(ObjType)
      case Closure(of) =>
         check(of, Elem(PathType))
         ESet(PathType)
      case Union(l,r) =>
         (infer(l), infer(l)) match {
            case (ESet(s), ESet(t)) if s == t => ESet(s)
            case (Elem(s), Elem(t)) if s == t => ESet(s)
            case (ESet(s), Elem(t)) if s == t => ESet(s)
            case (Elem(s), ESet(t)) if s == t => ESet(s)
            case _ => throw ParseError("expected set queries of equal type " + q)
         }
      case BigUnion(d,of) => infer(d) match {
            case ESet(s) => infer(of)(s ::: context) match {
               case ESet(t) => ESet(t)
               case _ => throw ParseError("illegal query " + q)
            }
            case _ => throw ParseError("illegal query " + q)
         }
      case Comprehension(d, p) =>
         infer(d) match {
            case ESet(t) => 
               Prop.check(p)(t ::: context)
               ESet(t)
            case _ => throw ParseError("illegal query " + q)
         }
      case Tuple(qs) =>
         val ts = qs map infer
         val bts = ts map {
           case Elem1(b) => b
           case _ => throw ParseError("illegal query " + q)
         }
         Elem(bts)
      case Projection(q, i) => infer(q) match {
         case Elem(s) if 0 <= i && i < s.length => Elem(s(i-1))
         case _ => throw ParseError("illegal query " + q)
      }
      case Present(c,s) => infer(c) match {
         case Elem1(PathType) => Elem(XMLType)
         case Elem1(ObjType) => Elem(XMLType)
         case ESet1(PathType) => ESet(XMLType)
         case ESet1(ObjType) => ESet(XMLType)
         case _ => throw ParseError("illegal query " + q)
      }
   }
   /** parses a query; infer must be called to sure well-formedness */
   def parse(n: Node) : Query = n match {
      case <individual/> => ThePath(Path.parse(xml.attr(n, "uri")))
      case <concept/> => AllThatAre(Unary.parse(xml.attr(n, "name")))
      case <component>{o}</component> => Component(parse(o), xml.attr(n, "index"))
      case <subobject>{o}</subobject> => SubObject(parse(o), Position.parse(xml.attr(n, "position")))
      case <type>{o}</type> => InferedType(parse(o), Path.parseM(xml.attr(n, "meta"), utils.mmt.mmtbase))
      case <var/> =>
         val s = xml.attr(n, "index")
         val i = try {s.toInt} catch {case _ => throw ParseError("illegal variable index: " + s)}
         Bound(i)
      case <related>{to}{by}</related> => Related(parse(to), RelationExp.parse(by))
      case <closure>{of}</closure> => Closure(parse(of))
      case <singleton>{e}</singleton> => Singleton(parse(e))
      case <union>{l}{r}</union> => Union(parse(l), parse(r))
      case <bigunion>{d}{s}</bigunion> =>
         val dom = xml.attr(n, "type") match {
            case "path" => parse(d)
            case "object" => parse(d)
         }
         BigUnion(dom, parse(s))
      case <comprehension>{d}{f}</comprehension> =>
         Comprehension(parse(d), Prop.parse(f))
      case <present>{o}</present> => Present(parse(o), Path.parseM(xml.attr(n, "style"), utils.mmt.mmtbase))
      //TODO missing cases
      case n => throw ParseError("illegal query expression: " + n)
   }
}

/** helper object for relation expressions */
object Prop {
   /** checks a proposition
    *  throws an exception if the proposition is ill-formed
    */
   def check(p: Prop)(implicit context: List[QueryBaseType]) {p match {
      case IsA(e,_) => Query.infer(e) match {
         case Elem(_) =>
         case _ => throw ParseError("illegal proposition " + p)
      }
      case PrefixOf(s, l) =>
         Query.check(s, Elem(PathType))
         Query.check(l, Elem(PathType))
     case IsIn(elem, tp) =>
         (Query.infer(elem), Query.infer(tp)) match {
            case (Elem(s), ESet(t)) if s == t =>
            case _ => throw ParseError("illegal proposition " + p)
         }
     case IsEmpty(r) => Query.infer(r) match {
        case ESet(_) =>
        case _ => throw ParseError("illegal proposition " + p)
     }
     case Equal(left, right) => (Query.infer(left), Query.infer(right)) match {
        case (Elem(s), Elem(t)) if s == t => 
        case _ => throw ParseError("illegal proposition " + p)
     }
     case Not(arg) => check(arg)
     case And(left, right) => check(left); check(right)
     case Forall(domain: Query, scope: Prop) => Query.infer(domain) match {
        case ESet(t) => check(scope)(t ::: context)
        case _ => throw ParseError("illegal proposition " + p)
     }
   }}
   
   /** parses a propositions; check must be called to ensure well-formedness */
   def parse(n: Node) : Prop = n match {
      case <equal>{e}{f}</equal> => Equal(Query.parse(e), Query.parse(f))
      case <isin>{e}{f}</isin> => IsIn(Query.parse(e), Query.parse(f))
      case <isa>{e}</isa> => IsA(Query.parse(e), Unary.parse(xml.attr(n, "concept")))
      case <isempty>{e}</isempty> => IsEmpty(Query.parse(e))
      case <and>{f}{g}</and> => And(parse(f), parse(g))
      case <not>{f}</not> => Not(parse(f))
      case <forall>{d}{f}</forall> => Forall(Query.parse(d), parse(f))
      //TODO missing cases
      case n => throw ParseError("illegal proposition: " + n)
   }
}

/*
<type meta="http://cds.omdoc.org/foundations/lf/lf.omdoc?lf"><subobject position="2_2"><component index="type"><individual uri="http://latin.omdoc.org/logics/syntax?CONJ?and"/></component></subobject></type>
<related><individual uri="http://latin.omdoc.org/logics/syntax?FOL"/><sequence><transitive><toobject relation="Includes"/></transitive><toobject relation="Declares"/></sequence></related>
*/