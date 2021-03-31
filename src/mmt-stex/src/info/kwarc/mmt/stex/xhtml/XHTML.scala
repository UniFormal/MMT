package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.{File, MMTSystem, XMLEscaping}
import info.kwarc.mmt.api.{LocalName, NamespaceMap, Path}
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource

import java.io.StringReader
import scala.annotation.tailrec
import scala.collection.mutable
import scala.xml._
import scala.xml.parsing.NoBindingFactoryAdapter

object XHTML {
  def text(s : String) = new XHTMLText(s)

  def unescape(s : String) : String = {
    val s1 = XMLEscaping.unapply(s)
    if (s1 == s) s else unescape(s1)
  }

  def makeAttributes(ls : ((String,String),String)*) = ls.foldLeft(scala.xml.Null : MetaData){
    case (e,(("",k),v)) =>
      new UnprefixedAttribute(k,v,e)
    case (e,((p,k),v)) =>
      new PrefixedAttribute(p,k,v,e)
  }

  object Rules {
    implicit val defaultrules : List[PartialFunction[Node,XHTMLNode]] = List(
      {case e : Elem if e.label == "html" => new XHTMLDocument(Some(e))},
      {case e : Elem if e.label == "math" => new XHTMLMath(Some(e))},
      {case e : Elem if e.label == "svg" =>
        val ne = new XHTMLNode(Some(e)) {}
        ne.attributes(("","xmlns")) = "http://www.w3.org/2000/svg"
        ne
      },
      {case e : Elem if List("mrow","mi","mo","msub","msup","mtext","mfrac","mspace","munderover","merror").contains(e.label) => new XMathML(Some(e))}
    )
  }
  private lazy val parserFactory = {
    val ret = new SAXFactoryImpl
    ret.setNamespaceAware(true)
    ret
  }
  private lazy val adapter = new NoBindingFactoryAdapter
  private lazy val parser = parserFactory.newSAXParser()

  def parse(f : File)(implicit rules : List[PartialFunction[Node,XHTMLNode]]) = {
    val ret = adapter.loadXML(new InputSource(File.Reader(f)),parser)
    apply(ret)
  }

  def applyString(s : String)(implicit rules : List[PartialFunction[Node,XHTMLNode]]) = {
    val ret = adapter.loadXML(new InputSource(new StringReader(s)),parser)
    apply(ret)
  }

  def apply(node : Node)(implicit rules : List[PartialFunction[Node,XHTMLNode]]) : List[XHTMLNode] = {
    (rules ::: Rules.defaultrules).find(_.isDefinedAt(node)) match {
      case Some(r) =>
        val ret = r(node)
        node.child.flatMap(apply).foreach(ret.add)
        ret.children.foreach(_.cleanup)
        List(ret)
      case _ => node match {
        case t : scala.xml.Text =>
          val init = t.toString().takeWhile(_.isWhitespace)
          val end = t.toString().reverse.takeWhile(_.isWhitespace).reverse
          val content = t.toString().drop(init.length).dropRight(end.length)
          List(init,content,end).filterNot(_.isEmpty).map(s => new XHTMLText(s))
        case a : Atom[String] =>
          List(new XHTMLText(a.data))
        case e : Elem if (e.label == "mi" || e.label == "mo") && e.child.isEmpty =>
          Nil
        case e : Elem =>
          val ret = new XHTMLNode(Some(node))
          node.child.flatMap(apply).foreach(ret.add)
          ret.children.foreach(_.cleanup)
          List(ret)
        case null =>
          Nil
        case _ =>
          ???
      }
    }
  }
  val empty = scala.xml.Text("\u200E")
}

class XHTMLNode(initial_node : Option[Node] = None) {
  def cleanup : Unit = {}
  def strip : Node = {
    val attrs = attributes.toList.filter{
      case (("","property"),_) => false
      case (("","resource"),_) => false
      case (("","class"),_) => false
      case (("stex","arg"),_) => false
      case _ =>
        true
    }
    Elem(null,_label,XHTML.makeAttributes(attrs:_*),scope,true,children.map(_.strip):_*)
  }
  def inscript : Boolean = _label == "script" || (_parent match {
    case Some(p) => p.inscript
    case _ => false
  })
  protected var _parent : Option[XHTMLNode] = None
  def parent = _parent
  protected def ancestors : List[XHTMLNode] = if (_parent.isDefined) _parent.get :: _parent.get.ancestors else Nil
  def ismath = {
    if (label == "mtext") false
    else ancestors.collectFirst {
      case _ : XHTMLMath => true
      case n : XHTMLNode if n.label == "mtext" => false
    }.getOrElse(false)
  }

  def top : XHTMLNode = _parent match {
    case None => this
    case Some(p) => p.top
  }

  def getHead = top.get("head")().head

  protected var prefix = initial_node.map(_.prefix).getOrElse("")
  protected var _label = initial_node.map(_.label).getOrElse("")
  def label = _label
  val attributes = mutable.Map.empty[(String,String),String]
  protected var scope = initial_node.map(_.scope).getOrElse(null)
  protected var _children : List[XHTMLNode] = Nil
  def children = _children

  private def fill(a : MetaData) : Unit = a match {
    case UnprefixedAttribute(str, value, data) =>
      attributes(("",str)) = value.toString()
      fill(data)
    case PrefixedAttribute(str, str1, value, data) =>
      attributes((str,str1)) = value.toString()
      fill(data)
    case scala.xml.Null =>
    case _ =>
      ???
  }
  initial_node.foreach(n => fill(n.attributes))

  def node : Node = Elem(prefix,_label,XHTML.makeAttributes(attributes.toSeq.reverse:_*),scope,true,children.map(_.node) :_*)

  def add(s : String) : Unit = add(XHTML.text(s))
  def add(n : Node) : Unit = add(XHTML(n)(Nil).head)
  def add(e : XHTMLNode) : Unit = {
    e._parent = Some(this)
    _children = _children ::: List(e)
  }

  def addAfter(e : XHTMLNode,after : XHTMLNode) : Unit = _children.indexOf(after) match {
    case -1 => _children = _children ::: List(e)
      e._parent = Some(this)
    case i => _children = _children.take(i+1) ::: e :: _children.drop(i+1)
      e._parent = Some(this)
  }

  protected def delete(e : XHTMLNode) : Unit = _children = _children.filterNot(_ == e)
  def delete : Unit = _parent.foreach(_.delete(this))

  override def toString: String = node.toString()

  @tailrec
  final def iterate(f : XHTMLNode => Unit) : Unit = {
    f(this)
    children match {
      case a :: _ => a.iterate(f)
      case _ => successor match {
        case Some(s) => s.iterate(f)
        case _ =>
      }
    }
  }

  @tailrec
  final def successor : Option[XHTMLNode] = parent match {
      case Some(p) =>
        val ch = p.children
        ch.drop(ch.indexOf(this)).tail.headOption match {
          case Some(h) => Some(h)
          case None =>
            p.successor
        }
      case _ => None
  }

  def get[A <: XHTMLNode](cls : Class[A]) : List[A] = { // You'd think this would be easier, but apparently it isn't
    val all = get()()
    def classes[B](b : Class[B]) : List[Any] = b :: (if (b.getSuperclass == null) Nil else classes(b.getSuperclass))
    all.collect({
      case a : A if classes(a.getClass) contains cls =>
        a
    })
  }

  def get(_label : String = "")(_attributes : (String,String,String)*) : List[XHTMLNode] = {
    val matches = _label match {
      case "" =>
        _attributes match {
          case Nil =>
            (n : XHTMLNode) => true
          case _ =>
            (n : XHTMLNode) => _attributes.exists(t => n.attributes.get(t._1,t._2).contains(t._3))
        }
      case _ =>
        _attributes match {
          case Nil =>
            (n : XHTMLNode) => n._label == _label
          case _ =>
            (n : XHTMLNode) => n._label == _label && _attributes.exists(t => n.attributes.get(t._1,t._2).contains(t._3))
        }
    }
    get(matches)
  }
  protected def get(matches : XHTMLNode => Boolean) : List[XHTMLNode] = _children.filter(matches) ::: _children.flatMap(_.get(matches))


  private var _id = 0
  def generateId = {
    _id += 1
    "stexelem" + (_id-1)
  }

  def isEmpty : Boolean = _children.isEmpty || _children.forall(_.isEmpty)

  def getTopMath : XHTMLNode = if (!parent.get.ismath) this else parent.get.getTopMath
}

class XHTMLText(init : String) extends XHTMLNode() {
  private var _text = init
  def text = _text

  override def node: scala.xml.Text = scala.xml.Text(text)
  override def strip: scala.xml.Text = node

  override def isEmpty: Boolean = {
    val trimmed = text.replace('\u2061',' ').trim
    trimmed.isEmpty
  }
}

class XHTMLDocument(initial_node : Option[Node] = None) extends XHTMLNode(initial_node) {
  private val doc_prefix = "<!DOCTYPE html>"

  override def node: Node =
    <html xmlns="http://www.w3.org/1999/xhtml"
          xmlns:om="http://www.openmath.org/OpenMath"
          xmlns:stex="http://www.mathhub.info"
          xmlns:ml="http://www.w3.org/1998/Math/MathML"
    >{children.map(_.node)}</html>

  override def toString: String = doc_prefix + super.toString
}