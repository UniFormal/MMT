package info.kwarc.mmt.odk

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom.{StandardString, StandardInt, SemanticType, RealizedType}
import info.kwarc.mmt.api.utils.{JSONArray, JSONString, JSONInt, JSON}
import info.kwarc.mmt.api.{LocalName, frontend, NamespaceMap, Path}
import info.kwarc.mmt.lf.{ApplySpine, Apply}

/**
  * Created by raupi on 20.01.16.
  */
class Plugin extends frontend.Plugin {
  val theory = Path.parseM("http://www.opendreamkit.org/?TypeSystem",NamespaceMap.empty)
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    // content enhancers
    controller.extman.addExtension(new LMFDB.Plugin)
  }
}

object Typesystem {
  val path = Path.parseM("http://www.opendreamkit.org/?TypeSystem",NamespaceMap.empty)
  def theory(implicit controller: Controller) = controller.get(Typesystem.path).asInstanceOf[DeclaredTheory]
}

trait Codec {
  val symbol : OMID
  val tm : Term
  type univ
  def apply(u:univ) : Term
  def fromJSON(json:JSON) : Term
}

abstract class RealCodec extends Codec {
  val tp : RealizedType
  def apply(u:univ) : OMLIT
  def fromJSON(json:JSON) : OMLIT
}

object TMIntRT extends RealizedType(
  Apply(OMS(Typesystem.path ? LocalName("tm")),OMS(Typesystem.path ? LocalName("int"))),
  StandardInt)

object TMInt extends RealCodec {
  val symbol = OMS(Typesystem.path ? LocalName("int"))
  val tm = Apply(OMS(Typesystem.path ? LocalName("tm")),symbol)
  val tp = TMIntRT
  type univ = BigInt
  def apply(u:univ) = tp.apply(u)
  def fromString(s:String) = BigInt(s)
  def fromJSON(json:JSON) = json match {
    case i:JSONInt => apply(BigInt(i.value))
    case i:JSONString => apply(BigInt(i.value))
    case _ => throw new Exception("Error: Not a JSON Int or String!")
  }
}

object TMStringRT extends RealizedType(
  Apply(OMS(Typesystem.path ? LocalName("tm")),OMS(Typesystem.path ? LocalName("string"))),
  StandardString)

object TMString extends RealCodec {
  val symbol = OMS(Typesystem.path ? LocalName("string"))
  val tm = Apply(OMS(Typesystem.path ? LocalName("tm")),symbol)
  val tp = TMStringRT
  type univ = String
  def apply(u:univ) = tp.apply(u)
  def fromString(s:String) = s
  def fromJSON(json:JSON) = json match {
    case i:JSONString => apply(i.value)
    case _ => throw new Exception("Error: Not a JSON String!")
  }
}

case class TMList(c:RealCodec) extends Codec {
  val symbol = OMS(Typesystem.path ? LocalName("list"))
  val tm = OMA(OMS(Typesystem.path ? LocalName("tm")),List(OMA(symbol,List(c.symbol))))
  val nil = OMS(Typesystem.path ? LocalName("nil"))
  val cons = OMS(Typesystem.path ? LocalName("cons"))
  type univ = List[c.univ]
  def apply(s:List[c.univ]) : Term = if (s.isEmpty) Apply(nil,c.symbol)
    else ApplySpine(cons,c.symbol,c.apply(s.head),apply(s.tail))
  def fromJSON(json:JSON) = json match {
    case i:JSONArray => apply(i.values.toList.map(c.fromJSON).map(_.value.asInstanceOf[c.univ]))//.map(_.value))
    case _ => throw new Exception("Error: Not a JSON Array of type " + c.tp.synType)
  }
}