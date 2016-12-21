package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._ 
import info.kwarc.mmt.api.{ContentPath, GlobalName, MPath}
import info.kwarc.mmt.api.objects.{OMMOD, OMS, Term}
import info.kwarc.mmt.api.utils._

import scala.collection.immutable.List

sealed abstract class Reference

sealed abstract class URIReference extends Reference

case class LogicalReference(mmturi: ContentPath) extends URIReference {
  override def toString = mmturi.toPath
}

case class PhysicalReference(url: URI) extends URIReference {
  override def toString = url.toString
}

case class ConceptReference(con : String) extends Reference {
  override def toString = con

  override def equals(o: scala.Any): Boolean = o match {
    case cr : ConceptReference if cr.con.toLowerCase == con.toLowerCase => true
    case _ => false
  }

  override def hashCode(): Int = con.toLowerCase().hashCode()
}

sealed abstract class Alignment {
  val from: Reference
  val to: Reference
  var props: List[(String, String)] = Nil

  def ->(that: Alignment): Alignment

  // def toJSON: (JSONString, JSONObject)

  var isGenerated = false

  def reverse: Alignment
}

sealed abstract class URIAlignment extends Alignment {
  val from: URIReference
  val to: URIReference
}

object ConceptPair {
  def apply(from : String, to : String) : ConceptPair = ConceptPair(ConceptReference(from),ConceptReference(to))
}
case class ConceptPair(from : ConceptReference, to : ConceptReference) extends Alignment {
  def  ->(that : Alignment): Alignment = {
    val ret = that match {
      case ConceptPair(fr, t) => ConceptPair(from, t)
      case ca : ConceptAlignment => ConceptAlignment(from,ca.ref)
      case _ => throw ImplementationError("missing case") //TODO
    }
    ret.isGenerated = true
    ret
  }
  def reverse = {
    val ret = ConceptPair(to,from)
    ret.isGenerated = true
    ret
  }

  override def toString = "< " + from + " | " + to + " >"
}

object ConceptAlignment{
  def apply(ref : Reference, con : String) : ConceptAlignment = ConceptAlignment(ref,ConceptReference(con))
}
case class ConceptAlignment(from: Reference, to : Reference) extends Alignment {
  require((from,to) match {
    case (ConceptReference(_),u : URIReference) => true
    case (u : URIReference,ConceptReference(_)) => true
    case _ => false
  })
  val (concept,ref,right) = (from,to) match {
    case (ConceptReference(s),r: URIReference) => (s,r,false)
    case (r: URIReference,ConceptReference(s)) => (s,r,true)
  }
  def ->(that: Alignment): Alignment = {
    require (to == that.from)
    val ret = that match {
      case ca @ ConceptAlignment(ffr,tto) =>
        if (right) InformalAlignment(ref,tto.asInstanceOf[URIReference]) else ConceptPair(concept,ca.concept)
      case cp @ ConceptPair(ffr,tto) if right => ConceptAlignment(from,tto)
      case a : FormalAlignment if !right => ConceptAlignment(ConceptReference(concept),a.to)
      case a : InformalAlignment if !right => ConceptAlignment(ConceptReference(concept),a.to)
      case _ => throw new Exception("incompatible alignments can't be composed: " + this + " and " + that)
    }
    ret.isGenerated = true
    ret
  }

  override def toString = "| " + concept + " | " + ref.toString + " |"
  def reverse = {
    val ret = ConceptAlignment(to,from)
    ret.isGenerated = true
    ret
  }
}

sealed abstract class FormalAlignment extends URIAlignment {
  val from: LogicalReference
  val to: LogicalReference
  val invertible: Boolean

  def toTerm : Term = to match {
    case LogicalReference(t: GlobalName) ⇒ OMS(t)
    case LogicalReference(t: MPath)      ⇒ OMMOD(t)
  }
}

case class SimpleAlignment(from: LogicalReference, to: LogicalReference, invertible: Boolean) extends FormalAlignment {

  def toJSON = (JSONString("Simple"), JSONObject(List(
    (JSONString("from"), JSONString(from.toString)),
    (JSONString("to"), JSONString(to.toString))
  )))

  def reverse = {
    val ret = if (invertible) SimpleAlignment(to, from, true) else InformalAlignment(to, from)
    ret.isGenerated = true
    ret
  }

  def ->(that: Alignment): Alignment = {
    require(to == that.from)
    val ret = that match {
      case ConceptAlignment(tfrom,tto) => ConceptAlignment(from,tto)
      case SimpleAlignment(tfrom, tto, inv) =>
        SimpleAlignment(from, tto, invertible && inv)
      case InformalAlignment(tfrom, tto) =>
        InformalAlignment(from,tto)
      case ArgumentAlignment(tfrom,tto,inv,arguments) =>
        ArgumentAlignment(from,tto,invertible && inv,arguments)
      case _ => throw ImplementationError("missing case") //TODO
    }
    ret.isGenerated = true
    ret
  }

  override def toString = from.toString + " " + to.toString +
    " direction=" + (if (invertible) """"both"""" else """"forward"""") +
    props.filter(x ⇒ x._1 != "direction").map(p ⇒ " " + p._1 + "=" + """"""" + p._2 + """"""").mkString("")
}

case class ArgumentAlignment(from: LogicalReference, to: LogicalReference, invertible: Boolean,
                             arguments: List[(Int, Int)]) extends FormalAlignment {
  def toJSON = (JSONString("Argument"), JSONObject(List(
    (JSONString("from"), JSONString(from.toString)),
    (JSONString("to"), JSONString(to.toString)),
    (JSONString("args"), JSONArray(arguments.map(p ⇒ JSONArray.fromList(List(JSONInt(p._1), JSONInt(p._2))))))
  )))

  def reverse = {
    val ret = if (invertible) ArgumentAlignment(to, from, true, arguments.map(p ⇒ (p._2, p._1))) else
      InformalAlignment(to, from)
    ret.isGenerated = true
    ret
  }

  override def toString = from.toString + " " + to.toString +
    " direction=" + (if (invertible) """"both"""" else """"forward"""") +
    " " + """arguments="""" + arguments.map(p ⇒ "(" + p._1 + "," + p._2 + ")").mkString("") +
    """"""" +
    props.filter(x ⇒ !(List("direction", "arguments") contains x._1)).map(p ⇒ " " + p._1 + "=" + """"""" + p._2 + """"""").mkString("")

  private def combine(args: List[(Int,Int)]) : List[(Int,Int)] = arguments.map(p => {
    val p2 = args.find(q => p._2==q._1)
    p2.map(q => (p._1,q._2))
  }) collect {
    case Some((a,b)) => (a,b)
  }

  def ->(that: Alignment): Alignment = {
    require(to == that.from)
    val ret = that match {
      case SimpleAlignment(tfrom, tto, inv) =>
        ArgumentAlignment(from, tto, invertible && inv,arguments)
      case InformalAlignment(tfrom, tto) =>
        InformalAlignment(from,tto)
      case ArgumentAlignment(tfrom,tto,inv,args2) =>
        ArgumentAlignment(from,tto,invertible && inv,combine(args2))
      case _ => throw ImplementationError("missing case") //TODO      
    }
    ret.isGenerated = true
    ret
  }
}

case class InformalAlignment(from: URIReference, to: URIReference) extends URIAlignment {
  def toJSON = (JSONString("Informal"), JSONObject(List(
    (JSONString("from"), JSONString(from.toString)),
    (JSONString("to"), JSONString(to.toString))
  )))

  def reverse = {
    val ret = InformalAlignment(to, from)
    ret.isGenerated = true
    ret
  }

  override def toString = from.toString + " " + to.toString +
    props.map(p ⇒ " " + p._1 + "=" + """"""" + p._2 + """"""").mkString("")

  def ->(that: Alignment): Alignment = {
    require(to == that.from)
    val ret = that match {
      case ConceptAlignment(_,tto) => ConceptAlignment(from,tto)
      case _ => InformalAlignment(from,that.to.asInstanceOf[URIReference])
    }
    ret.isGenerated = true
    ret
  }
}

object SimpleAlignment {
  def apply(from: ContentPath, to: ContentPath, invertible: Boolean, props: List[(String, String)] = Nil): SimpleAlignment = {
    val ret = SimpleAlignment(LogicalReference(from), LogicalReference(to), invertible)
    ret.props = props
    ret
  }
}
object ArgumentAlignment {
  def apply(from: ContentPath, to: ContentPath, invertible: Boolean, args: List[(Int, Int)], props: List[(String, String)] = Nil): ArgumentAlignment = {
    val ret = ArgumentAlignment(LogicalReference(from), LogicalReference(to), invertible, args)
    ret.props = props
    ret
  }
}
object InformalAlignment {
  def apply(from: ContentPath, to: URI, props: List[(String, String)] = Nil): InformalAlignment = {
    val ret = InformalAlignment(LogicalReference(from), PhysicalReference(to))
    ret.props = props
    ret
  }
}