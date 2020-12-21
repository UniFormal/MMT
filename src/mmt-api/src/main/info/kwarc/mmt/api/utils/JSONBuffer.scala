package info.kwarc.mmt.api.utils

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * This file implements mutable objects for constructing and destruction JSON objects
  */

/**
  * A convenience class to construct JSON Objects
  */
class JSONObjectBuffer {
  private val buffer = new ArrayBuffer[(String, JSON)]

  /** adds a value to this JSONObject buffer */
  def add(key: String, value: JSON): Unit = {
    buffer += ((key,value))
  }
  /** convenience method to add a value unless None */
  def addO(key: String, value: Option[JSON]): Unit = {
    value.foreach(add(key,_))
  }

  /** convenience method to add a bunch of values at once */
  def apply(parts: (String, JSON)*): JSONObjectBuffer = {
    parts.foreach({ case (s, j) => add(s, j)})
    this
  }

  def result(): JSONObject = {
    JSONObject.apply(buffer.toSeq :_*)
  }
}

object JSONObjectBuffer {
  def apply(parts: (String, JSON)*): JSONObjectBuffer = {
    val buffer = new JSONObjectBuffer
    buffer(parts: _*)
  }
}

/**
  * A convenience class to deconstruct JSON Objects
  * @param obj
  */
class JSONObjectParser(obj: JSONObject){
  lazy private val buffer: mutable.HashMap[String, JSON] = {
    val b = new mutable.HashMap[String, JSON]()
    obj.map.foreach(kv => b.+=((kv._1.value, kv._2)))
    b
  }

  /** takes a value from this JSONObjectParser of type T */
  def take[T <: JSON](key: String): T = {
    buffer.remove(key).getOrElse(throw NOSuchKey(key)).asInstanceOf[T]
  }

  /** convenience method to take a value from this JSOnObjectParser if it exists */
  def takeO[T <: JSON](key: String) : Option[T] = {
    buffer.remove(key).map(_.asInstanceOf[T])
  }

  implicit def rest: Map[String, JSON] = buffer.toMap
}
case class NOSuchKey(key: String) extends Exception(s"No such key: $key")


/**
  * A convenience class to construct JSON Arrays
  */
class JSONListBuffer {
  private val buffer = new ArrayBuffer[JSON]

  def add(value: JSON): Unit = {
    buffer += value
  }

  def result(): JSONArray = {
    JSONArray.apply(buffer.toSeq:_*)
  }
}

/**
  * A convenience class to parse JSON Arrays
  * @param a
  */
class JSONListParser(a: JSONArray) {
  private var buffer = a.values.toList

  def take[T <: JSON] : T = {
    val head = buffer.headOption.getOrElse(throw EmptyList)
    buffer = buffer.tail
    head.asInstanceOf[T]
  }

  def rest: List[JSON] = buffer
}
case object EmptyList extends Exception(s"JSON List is empty and does not contain any more elements")