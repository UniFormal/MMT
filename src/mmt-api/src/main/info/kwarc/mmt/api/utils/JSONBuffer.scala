package info.kwarc.mmt.api.utils

import info.kwarc.mmt.api.GeneralError

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * A convenience class to construct JSON Objects
  */
class JSONObjectBuffer {
  private val buffer = new ArrayBuffer[(String, JSON)]

  def add[T](key: String, value: T)(implicit converter: JSONConverter[T]): Unit = {
    buffer += ((key, converter.toJSON(value)))
  }

  def result(): JSONObject = {
    JSONObject(buffer.result() :_*)
  }
}

/**
  * A convenienve class to deconstruct JSON Objects
  * @param obj
  */
class JSONObjectParser(obj: JSONObject){
  lazy private val buffer: mutable.HashMap[String, JSON] = {
    val b = new mutable.HashMap[String, JSON]()
    obj.map.foreach(kv => b.+=((kv._1.value, kv._2)))
    b
  }

  def take[T](key: String)(implicit converter: JSONConverter[T]) : T = {
    converter.fromJSON(buffer.remove(key).getOrElse(throw NOSuchKey(key)))
  }

  implicit def rest: Map[String, JSON] = buffer.toMap
}
case class NOSuchKey(key: String) extends Exception(s"No such key: $key")


/**
  * A convenience class to construct JSON Arrays
  */
class JSONListBuffer {
  private val buffer = new ArrayBuffer[JSON]

  def +=[T](value: T)(implicit converter: JSONConverter[T]): Unit = {
    buffer += converter.toJSON(value)
  }

  def result(): JSONArray = {
    JSONArray(buffer.result(): _*)
  }
}

/**
  * A convenience class to parse JSON Arrays
  * @param a
  */
class JSONListParser(a: JSONArray) {
  private var buffer = a.values.toList

  def take[T](implicit converter: JSONConverter[T]) : T = {
    val head = buffer.headOption.getOrElse(throw EmptyList)
    buffer = buffer.tail
    converter.fromJSON(head)
  }

  implicit def rest: List[JSON] = buffer
}
case object EmptyList extends Exception(s"JSON List is empty and does not contain any more elements")


/** A class to quickly convert objects between Scala and JSON */
trait JSONConverter[T] {
  def toJSON(obj: T) : JSON

  def fromJSON(j: JSON): T = fromJSONOption(j).getOrElse(throw ConverterNotApplicable(j))
  def fromJSONOption(j: JSON) : Option[T]
}
case class ConverterNotApplicable(json: JSON) extends Exception(s"Not applicable: $json")



object JSONConverter {

  def toJSON[T](obj: T)(implicit converter: JSONConverter[T]) : JSON = converter.toJSON(obj)
  def fromJSON[T](json: JSON)(implicit converter: JSONConverter[T]): T = converter.fromJSON(json)
  def fromJSONOption[T](json: JSON)(implicit converter: JSONConverter[T]): Option[T] = converter.fromJSONOption(json)

  implicit object convertJSON extends JSONConverter[JSON] {
    def toJSON(j: JSON): JSON = j
    def fromJSONOption(j: JSON): Option[JSON] = Some(j)
  }

  implicit object convertIn extends JSONConverter[Int] {
    def toJSON(i: Int) = JSONInt(i)
    def fromJSONOption(j : JSON): Option[Int] = j match {
      case JSONInt(x) => Some(x)
      case _ => None
    }
  }

  implicit object convertDouble extends JSONConverter[Double] {
    def toJSON(d: Double) = JSONFloat(d)
    def fromJSONOption(j : JSON): Option[Double] = j match {
      case JSONFloat(x) => Some(x)
      case _ => None
    }
  }

  implicit object convertBoolean extends JSONConverter[Boolean] {
    def toJSON(b: Boolean) = JSONBoolean(b)
    def fromJSONOption(j: JSON): Option[Boolean] = j match {
      case JSONBoolean(x) => Some(x)
      case _ => None
    }
  }

  implicit object convertString extends JSONConverter[String] {
    def toJSON(s: String) = JSONString(s)
    def fromJSONOption(j: JSON): Option[String] = j match {
      case JSONString(x) => Some(x)
      case _ => None
    }
  }

  implicit def OptC[T](implicit member: JSONConverter[T]) = new  JSONConverter[Option[T]] {
    def toJSON(opt: Option[T]): JSON = opt.map(m => member.toJSON(m)).getOrElse(JSONNull)
    def fromJSONOption(j: JSON): Option[Option[T]] = j match {
      case JSONNull => Some(None)
      case x@_ => member.fromJSONOption(x).map(y => Some(y))
    }
  }

  implicit def ListC[T](implicit member: JSONConverter[T]) = new JSONConverter[List[T]] {
    def toJSON(seq: List[T]) = JSONArray(seq.map(m => member.toJSON(m)):_*)
    def fromJSONOption(j: JSON): Option[List[T]] = j match {
      case JSONArray(x @ _*) =>
        val y = x.map(member.fromJSONOption).toList
        if(y.forall(_.nonEmpty)){
          Some(y.map(_.get))
        } else {
          None
        }
      case _ => None
    }
  }
}
