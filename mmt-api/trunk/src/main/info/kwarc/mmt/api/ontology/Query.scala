package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.utils.MyList.fromList
import info.kwarc.mmt.api.objects._
import scala.collection.mutable.{HashSet,HashMap}
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
   def Imports = Choice(+ HasOccurrenceOfInImport, Query.HasStructureFrom)
    /** Disjunction of all binary relations, helpful for dependency closures */
   def AnyDep = Choice(+HasOccurrenceOfInType, +HasOccurrenceOfInDefinition, +HasOccurrenceOfInTarget,
   +IsAliasFor, +HasMeta, +HasOccurrenceOfInImport, +HasDomain, +HasCodomain)
}