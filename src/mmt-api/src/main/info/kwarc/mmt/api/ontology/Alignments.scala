package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import objects._
import utils._

/** any named object that can be subject to alignment */
sealed abstract class Reference
/** objects that have URIs */
sealed abstract class URIReference extends Reference
/** MMT objects */
case class LogicalReference(mmturi: ContentPath) extends URIReference {
  override def toString = mmturi.toPath
}
/** external objects */
case class PhysicalReference(url: URI) extends URIReference {
  override def toString = url.toString
}
/** general concepts */
case class ConceptReference(con: String) extends Reference with CanonicalForm[ConceptReference] {
  def canonical = copy(con = con.toLowerCase)
  override def toString = con

  override def hashCode(): Int = con.toLowerCase.hashCode
  override def equals(that: Any): Boolean = that match {
    case ConceptReference(ocon) => ocon.toLowerCase == con.toLowerCase
    case _ => false
  }
}

sealed abstract class Alignment {
  val from: Reference
  val to: Reference
  var props: List[(String, String)] = Nil

  /** the composition (diagram-order) of this with that (not necessarily inducing a translation) */
  def ->(that: Alignment): Alignment

  var isGenerated = false

  /** the reverse of this alignment (not necessarily inducing a translation) */
  def reverse: Alignment
}

sealed abstract class URIAlignment extends Alignment {
  val from: URIReference
  val to: URIReference
}

case class ConceptPair(from: ConceptReference, to: ConceptReference) extends Alignment {
  def ->(that: Alignment): Alignment = {
    val ret = that match {
      case ConceptPair(fr, t) => ConceptPair(from, t)
      case ca: ConceptAlignment => ConceptAlignment(from,ca.ref)
      case _ => throw ImplementationError("not concatenatable")
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
object ConceptPair {
  def apply(from: String, to: String): ConceptPair = ConceptPair(ConceptReference(from),ConceptReference(to))
}

object ConceptAlignment{
  def apply(ref: Reference, con: String): ConceptAlignment = ConceptAlignment(ref,ConceptReference(con))
}
case class ConceptAlignment private (from: Reference, to: Reference) extends Alignment {
  // FR I made the constructor private because it is undocumented that not all arguments are legal
  require((from,to) match {
    case (ConceptReference(_),u: URIReference) => true
    case (u: URIReference,ConceptReference(_)) => true
    case _ => false
  })
  val (concept,ref,right) = (from,to) match {
    case (ConceptReference(s),r: URIReference) => (s,r,false)
    case (r: URIReference,ConceptReference(s)) => (s,r,true)
    case _ => throw ImplementationError("should be impossible due to assertion above")
  }
  def ->(that: Alignment): Alignment = {
    require (to == that.from)
    val ret = that match {
      case ca @ ConceptAlignment(ffr,tto) =>
        if (right) InformalAlignment(ref,tto.asInstanceOf[URIReference]) else ConceptPair(concept,ca.concept)
      case cp @ ConceptPair(ffr,tto) if right => ConceptAlignment(from,tto)
      case a: FormalAlignment if !right => ConceptAlignment(ConceptReference(concept),a.to)
      case a: InformalAlignment if !right => ConceptAlignment(ConceptReference(concept),a.to)
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
  def invertible: Boolean = false

  def toTerm: Term = to match {
    case LogicalReference(t: GlobalName) => OMS(t)
    case LogicalReference(t: MPath)      => OMMOD(t)
  }
  
  def ->(that: Alignment): Alignment = {
    val a = that match {
      case fa: FormalAlignment => AlignmentConcatenation(this,fa)
      case ua: URIAlignment => InformalAlignment(this.from, ua.to)
      case ca: ConceptAlignment => ConceptAlignment(this.from, ca.to)
      case cp: ConceptPair => throw ImplementationError("this would violate concatenatability")
    }
    a.isGenerated = true
    a
  }
  
  /** if this is invertible, the reverse must be a formal alignment as well */
  def reverse: Alignment = {
    val a = InformalAlignment(to, from)
    a.isGenerated = true
    a
  }
}

case class AlignmentConcatenation(first: FormalAlignment, second: FormalAlignment) extends FormalAlignment {
  isGenerated = true
  val from = first.from
  val to = second.to
  override def invertible = first.invertible && second.invertible
  override def reverse = second.reverse -> first.reverse
}

case class SimpleAlignment(from: LogicalReference, to: LogicalReference, override val invertible: Boolean) extends FormalAlignment {

  def toJSON = (JSONString("Simple"), JSONObject(List(
    (JSONString("from"), JSONString(from.toString)),
    (JSONString("to"), JSONString(to.toString))
  )))

  override def reverse = {
    val ret = if (invertible) SimpleAlignment(to, from, true) else InformalAlignment(to, from)
    ret.isGenerated = true
    ret.props = props
    ret
  }

  override def ->(that: Alignment): Alignment = {
    require(to == that.from)
    val ret: Alignment = that match {
      case ConceptAlignment(tfrom,tto) => ConceptAlignment(from,tto)
      case SimpleAlignment(tfrom, tto, inv) =>
        SimpleAlignment(from, tto, invertible && inv)
      case InformalAlignment(tfrom, tto) =>
        InformalAlignment(from,tto)
      case ArgumentAlignment(tfrom,tto,inv,arguments) =>
        ArgumentAlignment(from,tto,invertible && inv,arguments)
      case _ => super.->(that) 
    }
    ret.isGenerated = true
    ret
  }

  override def toString = from.toString + " " + to.toString +
    " direction=" + (if (invertible) """"both"""" else """"forward"""") +
    props.filter(x => x._1 != "direction").map(p => " " + p._1 + "=" + """"""" + p._2 + """"""").mkString("")
}

case class ArgumentAlignment(from: LogicalReference, to: LogicalReference, override val invertible: Boolean,
                             arguments: List[(Int, Int)]) extends FormalAlignment {
  def toJSON = (JSONString("Argument"), JSONObject(List(
    (JSONString("from"), JSONString(from.toString)),
    (JSONString("to"), JSONString(to.toString)),
    (JSONString("args"), JSONArray(arguments.map(p => JSONArray(JSONInt(p._1), JSONInt(p._2))):_*))
  )))

  override def reverse = {
    val ret = if (invertible) ArgumentAlignment(to, from, true, arguments.map(p => (p._2, p._1))) else
      InformalAlignment(to, from)
    ret.isGenerated = true
    ret
  }

  override def toString = from.toString + " " + to.toString +
    " direction=" + (if (invertible) """"both"""" else """"forward"""") +
    " " + """arguments="""" + arguments.map(p => "(" + p._1 + "," + p._2 + ")").mkString("") +
    """"""" +
    props.filter(x => !(List("direction", "arguments") contains x._1)).map(p => " " + p._1 + "=" + """"""" + p._2 + """"""").mkString("")

  private def combine(args: List[(Int,Int)]): List[(Int,Int)] = arguments.map(p => {
    val p2 = args.find(q => p._2==q._1)
    p2.map(q => (p._1,q._2))
  }) collect {
    case Some((a,b)) => (a,b)
  }

  override def ->(that: Alignment): Alignment = {
    require(to == that.from)
    val ret = that match {
      case SimpleAlignment(tfrom, tto, inv) =>
        ArgumentAlignment(from, tto, invertible && inv,arguments)
      case InformalAlignment(tfrom, tto) =>
        InformalAlignment(from,tto)
      case ArgumentAlignment(tfrom,tto,inv,args2) =>
        ArgumentAlignment(from,tto,invertible && inv,combine(args2))
      case _ => super.->(that)
    }
    ret.isGenerated = true
    ret
  }
}

/** aligns a global function with a method that is accessed via projection on the first argument */
// TODO the dot operator should be obtained generically rather than be part of the alignment
case class DereferenceAlignment(fromP: ContentPath, toP: ContentPath, dotOperator: GlobalName) extends FormalAlignment {
  val from = LogicalReference(fromP)
  val to = LogicalReference(toP)
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
    props.map(p => " " + p._1 + "=" + """"""" + p._2 + """"""").mkString("")

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
