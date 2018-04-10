package info.kwarc.mmt.api.utils

import scala.xml._

/** objects that can be serialized as XML */
trait ScalaToXML {
  def toXML: Node
}

/** implicit conversions to add XML serializations for typical types */
object ScalaToXML {
  /** simplified XML elements */
  def elem(label: String, children: Node*) = Elem(null, label, Null, TopScope, children:_*)

  def value(label: String, v: String) = new ScalaToXML {
    def toXML = elem(label, Text(v))
  }
  
  implicit def XInt(i: Int): ScalaToXML = value("integer", i.toString)
  implicit def XString(s: String): ScalaToXML = value("string", s)
  implicit def XBoolean(b: Boolean): ScalaToXML = value("boolean", b.toString)
  implicit def XNode(n: Node): ScalaToXML = elem("node", n)
  
  implicit class XPair[A <% ScalaToXML, B <% ScalaToXML](p: (A,B)) extends ScalaToXML {
    def toXML = elem("pair", p._1.toXML, p._2.toXML)
  }
  
  implicit class XList[A <% ScalaToXML](l: List[A]) extends ScalaToXML {
    def toXML = elem("list", l.map(_.toXML) :_*)
  }

  implicit class XOption[A <% ScalaToXML](o: Option[A]) extends ScalaToXML {
    def toXML = elem("option", o.toList.map(_.toXML) :_*)
  }
}

/** mix in this trait to get add a systematic XML serialization to a class defined by a list of fields
  *  
  *  @example
  *  {{{
  *  case class Q(i: Int, l: List[Option[(Int,Q)]]) extends FieldsToXML {
  *    def fields = Field("i", i) :: Field("l", l) :: Nil
  *  }
  *  val q = Q(1, List(Some((2,Q(2,List(None))))))
  *  printlln(q.toXML)
  *  }}}
*/
trait FieldsToXML extends ScalaToXML {
  def fields: List[Field[_]]
  def toXML = ScalaToXML.elem(getClass.getName, fields.map(_.toXML) :_*)
}

/** auxiliary class for [[FieldsToXML]] */
case class Field[+A <% ScalaToXML](name: String, value: A) {
  def toXML = ScalaToXML.elem(name, value.toXML)
}

/** serializable trees */ 
case class Tree[+A <% ScalaToXML](label: A, children: Tree[A]) extends FieldsToXML {
  def fields = {
    val f: Field[ScalaToXML] = Field("label", label)  
    f :: Field("children", children) :: Nil
  }
}


/** a class for storing objects that are serialized lazily */ 
class LazySerializationStore(url: String) {
  private var nextId: Int = -1
  private def getNextId = {nextId += 1; nextId}
  
  private val entries = new scala.collection.mutable.HashMap[Int,() => ScalaToXML]
  
  /** store a continuation for future serialization and return a reference to it */
  def add(v: => ScalaToXML): LazySerialization = {
    val id = getNextId
    entries(id) = () => v
    LazySerialization(url, id)
  }
  
  /** compute and retrieve a previously stored serialization */
  def get(id: Int): Node = {
    val v = entries.get(id).getOrElse(return <error/>)
    entries -= id
    v().toXML
  }
}

/** an object whose lazy serialization can be retrieved from a URL */
case class LazySerialization(url: String, id: Int) extends ScalaToXML {
  def toXML = <lazy url={url + "?" + id}/>
}
