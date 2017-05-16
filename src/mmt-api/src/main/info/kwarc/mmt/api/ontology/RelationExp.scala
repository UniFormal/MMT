package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api.ParseError
import info.kwarc.mmt.api.objects.{OMA, OMS, Term}
import info.kwarc.mmt.api.utils.xml

import scala.xml.Node

/**
  * binary relations between paths, i.e., relation in the MMT ontology
  *
  * The semantics of a [[RelationExp]] is a binary relation on [[Path]]s with the usual operations for the calculus of binary endorelations.
  * For example,
  * {{{
  * val relstore:RelStore
  * val doc: DPath
  * val deps = relstore.querySet(doc, +Declares * HasType(IsTheory) * (Imports | Reflexive) * -Declares)
  * }}}
  * could be used as a dependency relation between documents.
  */
sealed abstract class RelationExp {
  /** union with another relation */
  def |(q: RelationExp) = Choice(this, q)

  /** composition with another relation */
  def *(q: RelationExp) = Sequence(this, q)

  /** reflexive-transitive closure */
  def ^* = Reflexive | Transitive(this)

  /** transitive closure */
  def ^+ = Transitive(this)

  /** inverse/dual */
  def unary_- : RelationExp
}

/** base case: an atomic binary relation */
case class ToObject(dep: Binary) extends RelationExp {
  def unary_- = ToSubject(dep)

  override def toString = "+ " + dep
}

/** base case: the inverse of an atomic binary relation */
case class ToSubject(dep: Binary) extends RelationExp {
  def unary_- = ToObject(dep)

  override def toString = "- " + dep
}

/** the transitive closure of a relation */
case class Transitive(q: RelationExp) extends RelationExp {
  def unary_- = Transitive(-q)

  override def toString = "(" + q + ")*"
}

/** the symmetric closure of a relation */
object Symmetric {
  def apply(q: RelationExp) = Choice(q, -q)
}

/** the union of a list of relations */
case class Choice(qs: RelationExp*) extends RelationExp {
  def unary_- = Choice(qs.map(-_): _*)

  override def toString = qs.mkString("", " | ", "")
}

/** the composition of a list of relations */
case class Sequence(qs: RelationExp*) extends RelationExp {
  def unary_- = Sequence(qs.reverse.map(-_): _*)

  override def toString = qs.mkString("", " ; ", "")
}

/** the reflexive relation */
case object Reflexive extends RelationExp {
  def unary_- = this

  override def toString = "Id"
}

/** the reflexive relation restricted to a set of paths
  * This permits the restriction of a result set to, e.g., elements of a certain type.
  */
case class HasType(mustHave: Option[List[Unary]], mustNotHave: List[Unary]) extends RelationExp {
  def unary_- = this

  override def toString = ":" + mustHave.mkString(",") + " \\ " + mustNotHave.mkString(",")
}

object HasType {
  def apply(us: Unary*): RelationExp = HasType(Some(us.toList), Nil)
}

/** helper object for relation expressions */
object RelationExp {

  /** S has a structure declaration with domain O, defined as an abbreviation */
  def HasStructureFrom = Sequence(-HasCodomain, HasType(IsStructure), +HasDomain)

  def Imports = Choice(+Includes, RelationExp.HasStructureFrom)

  def TheoryUses = Choice(+HasMeta, Imports)

  def ViewTheories = HasType(IsView) * (+HasDomain | +HasCodomain)

  def Deps = TheoryUses | ViewTheories

  /** Disjunction of all binary relations, helpful for dependency closures */
  def AnyDep(implicit relManager: RelationalManager) = Choice(relManager.allBinary.map(+_).toSeq: _*)

  /**
    * Parses a RelationExpression
    * @param n Node to parse
    * @param relManager Relational Manager to use
    * @return
    */
  def parse(n: Node)(implicit relManager: RelationalManager): RelationExp = n match {
    case <sequence>{r@_*}</sequence> =>
      Sequence(r map parse: _*)

    case <choice>{r@_*}</choice> =>
      Choice(r map parse: _*)

    case <transitive>{r}</transitive> =>
      Transitive(parse(r))

    case <reflexive/> => Reflexive

    case <inverse>{r}</inverse> => -parse(r)

    case <toobject/> =>
      +relManager.parseBinary(xml.attr(n, "relation"))
    case <tosubject/> =>
      -relManager.parseBinary(xml.attr(n, "relation"))

    case _ => throw ParseError("illegal relation expression: " + n)
  }

  def parse(t : Term) : RelationExp = t match {
    case OMA(OMS(QMTRelationExp.ToObject), st :: Nil) =>
      ToObject(Binary.parse(st))

    case OMA(OMS(QMTRelationExp.ToSubject), st :: Nil) =>
      ToSubject(Binary.parse(st))

    case OMA(OMS(QMTRelationExp.Transitive), st :: Nil) =>
      Transitive(parse(st))

    case OMA(OMS(QMTRelationExp.Symmetric), st :: Nil) =>
      Symmetric(parse(st))

    case OMA(OMS(QMTRelationExp.Choice), lst) =>
      Choice(lst.map(parse) :_*)

    case OMA(OMS(QMTRelationExp.Sequence), lst) =>
      Sequence(lst.map(parse) :_*)

    case OMS(QMTRelationExp.Reflexive) =>
      Reflexive

    // TODO: Parse HasType
    case _ => throw new Exception(s"Expected a relational expression, got instead: $t")
  }

}

/*
<type meta="http://cds.omdoc.org/foundations/lf/lf.omdoc?lf"><subobject position="2_2"><component index="type"><individual uri="http://latin.omdoc.org/logics/syntax?CONJ?and"/></component></subobject></type>
<related><individual uri="http://latin.omdoc.org/logics/syntax?FOL"/><sequence><transitive><toobject relation="Includes"/></transitive><toobject relation="Declares"/></sequence></related>
*/
