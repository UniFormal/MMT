package info.kwarc.mmt.api.utils

import scala.xml._

/** objects that can be serialized as XML */
trait ScalaTo {
  def toXML: Node
  def toJSON: JSON
  /** identity by default; override to add lazy serialization */
  def lazily(implicit store: LazySerializationStore): ScalaTo = this
}

/** implicit conversions to add XML serializations for typical types */
object ScalaTo {
  /** labeled node in a serialization syntax tree */
  def node(label: String, children: List[ScalaTo]) = new ScalaTo {
    def toXML = Elem(null, label, Null, TopScope, children.isEmpty, children.map(_.toXML):_*)
    def toJSON = JSONArray(JSONString(label), JSONArray(children.map(_.toJSON):_*))
  }
  
  def value(label: String, v: String) = new ScalaTo {
    def toXML = Elem(null, label, Null, TopScope, false, Text(v))
    def toJSON = JSONObject(label -> JSONString(v))
  }
  
  implicit def XInt(i: Int): ScalaTo = value("integer", i.toString)
  implicit def XString(s: String): ScalaTo = value("string", s)
  implicit def XBoolean(b: Boolean): ScalaTo = value("boolean", b.toString)
  implicit def XNode(n: Node): ScalaTo = new ScalaTo {
    def toXML = n
    def toJSON = JSONXML.xmlToJSON(n)
  }
  
  implicit def XPair[A <% ScalaTo, B <% ScalaTo](p: (A,B)) = node("pair", List(p._1, p._2))
  implicit def XList[A <% ScalaTo](l: List[A]): ScalaTo = {
    node("list", l.map(implicitly[A => ScalaTo]))
  }
  implicit def XOption[A <% ScalaTo](o: Option[A]): ScalaTo = node("option", o.toList.map(implicitly[A=>ScalaTo]))
}

/** mix in this trait to get add systematic serialization defined by a list of fields */
trait FieldWiseScalaTo extends ScalaTo {
  def fields: List[Field]
  def scalaTo = ScalaTo.node(getClass.getName, fields.map(_.to))
  def toXML = scalaTo.toXML
  def toJSON = scalaTo.toJSON
  
  override def lazily(implicit store: LazySerializationStore) = {
    val lazyFields = fields.map(f => f.lazily)
    ScalaTo.node(getClass.getName, lazyFields)
  }
}

/** auxiliary class for [[FieldsToXML]] */
case class Field(name: String, value: ScalaTo) {
  def to = ScalaTo.node(name, List(value))
  /** only serializes one level, then lazy continuations */
  def lazily(implicit store: LazySerializationStore) = ScalaTo.node(name, List(store {value}))
}

/** serializable trees */
object FieldWiseScalaTo {
  abstract class Example extends FieldWiseScalaTo {
    val a: String
    val b: List[Example]
    def fields = List(Field("a", a), Field("b", b))
  }
}

/** a class for storing objects that are serialized lazily */ 
class LazySerializationStore(url: String) {
  private var nextId: Int = -1
  private def getNextId = {nextId += 1; nextId}
  
  private val entries = new scala.collection.mutable.HashMap[Int,() => ScalaTo]
  
  /** store a continuation for future serialization and return a reference to it */
  def add[A <% ScalaTo](v: => A): LazySerialization = {
    val id = getNextId
    entries(id) = () => v
    LazySerialization(url, id)
  }
  
  def apply(v: => ScalaTo) = add(v)
  
  /** compute and retrieve a previously stored serialization */
  def get(id: Int): Node = {
    val v = entries.get(id).getOrElse(return <error/>)
    entries -= id
    v().toXML
  }
}

/** an object whose lazy serialization can be retrieved from a URL */
case class LazySerialization(baseURL: String, id: Int) extends ScalaTo {
  val url = baseURL + "?" + id
  def toXML = <lazy url={url}/>
  def toJSON = JSONObject("lazy" -> JSONString(url))
}
