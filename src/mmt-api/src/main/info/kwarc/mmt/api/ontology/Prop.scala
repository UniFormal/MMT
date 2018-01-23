package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api.{LocalName, ParseError}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.xml

import scala.xml.Node

/** propositions */
sealed abstract class Prop

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

/** disjunction */
object Or {
  def apply(left : Prop, right: Prop) : Prop = Not(And(Not(left), Not(right)))
  def unapply(p : Prop) : Option[(Prop, Prop)] = p match {
    case Not(And(Not(l), Not(r))) => Some((l, r))
    case _ => None
  }
}

/** universal quantification over a set */
case class Forall(domain: Query, varname: LocalName, scope: Prop) extends Prop

/** existential quantification over a set */
object Exists {
  def apply(domain: Query, varname: LocalName, scope: Prop) : Prop = Not(Forall(domain, varname, Not(scope)))
  def unapply(p : Prop) : Option[(Query, LocalName, Prop)] = p match {
    case Not(Forall(domain, varname, Not(scope))) => Some((domain, varname, scope))
    case _ => None
  }
}

/** judgement that holds for a single element */
case class Holds(about: Query, j: QueryJudgement) extends Prop

object Prop {

  /**
    * parses a Term representing a Proposition
    * @param t Term to parse
    * @param queryFunctions List of [[QueryFunctionExtension]] to use
    * @param relManager RelationalManager to use
    * @return
    */
  def parse(t: Term)(implicit queryFunctions: List[QueryFunctionExtension], relManager: RelationalManager) : Prop = t match {
    case OMA(OMID(QMTProp.IsA), q :: concept :: Nil) =>
        IsA(Query.parse(q), Unary.parse(q))

    case OMA(OMID(QMTProp.PrefixOf), l :: r :: Nil) =>
      PrefixOf(Query.parse(l), Query.parse(r))

    case OMA(OMID(QMTProp.IsIn), e :: f :: Nil) =>
      IsIn(Query.parse(e), Query.parse(f))

    case OMA(OMID(QMTProp.IsEmpty), e :: Nil) =>
      IsEmpty(Query.parse(e))

    case OMA(OMID(QMTProp.Equal), l :: r :: Nil) =>
      Equal(Query.parse(l), Query.parse(r))

    case OMA(OMID(QMTProp.And), f :: g :: Nil) =>
      And(parse(f), parse(g))

    case OMA(OMID(QMTProp.Or), f :: g :: Nil) =>
      Or(parse(f), parse(g))

    case OMA(OMID(QMTProp.Not), f :: Nil) =>
      Not(parse(f))

    case OMBINDC( OMID(QMTProp.Forall), Context(VarDecl(name, _, _, _, _)), List(d, f)) =>
      Forall(Query.parse(d), name, parse(f))

    case OMBINDC( OMID(QMTProp.Forall), Context(VarDecl(name, _, _, _, _)), List(d, f)) =>
      Exists(Query.parse(d), name, parse(f))

    case OMA(OMID(QMTProp.Holds), a :: j :: Nil) =>
      Holds(Query.parse(a), QueryJudgement.parse(j))
  }

  /**
    * Parses an XML node representing a Proposition into an actual Proposition
    * @param n Node to parse
    * @param queryFunctions List of [[QueryFunctionExtension]] to use
    * @param relManager RelationalManager to use
    * @return
    */
  def parse(n: Node)(implicit queryFunctions: List[QueryFunctionExtension], relManager: RelationalManager): Prop = n match {
    case <isa>{e}</isa> =>
      IsA(Query.parse(e), relManager.parseUnary(xml.attr(n, "concept")))

    case <prefixof>{s}{l}</prefixof> =>
      PrefixOf(Query.parse(s), Query.parse(l))

    case <isin>{e}{f}</isin> =>
      IsIn(Query.parse(e), Query.parse(f))

    case <isempty>{e}</isempty> =>
      IsEmpty(Query.parse(e))

    case <equal>{l}{r}</equal> =>
      Equal(Query.parse(l), Query.parse(r))

    case <and>{f}{g}</and> =>
      And(parse(f), parse(g))

    case <or>{f}{g}</or> =>
      Or(parse(f), parse(g))

    case <not>{f}</not> =>
      Not(parse(f))

    case <forall>{d}{f}</forall> =>
      Forall(Query.parse(d), xml.attrL(n, "name"), parse(f))

    case <exists>{d}{f}</exists> =>
      Exists(Query.parse(d), xml.attrL(n, "name"), parse(f))

    case <holds>{a}{j}</holds> =>
      Holds(Query.parse(a), QueryJudgement.parse(j))

    case _ => throw ParseError("illegal proposition: " + n)
  }
}
