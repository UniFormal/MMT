package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.xml.parsing.{ConstructingParser, NoBindingFactoryAdapter}
import scala.xml.{Elem, Node, NodeSeq, XML}

trait HTMLNode {
  def plain: HTMLPlainNode
  def namespace: String  = plain.namespace

  def state: ParsingState = plain.state
  def label : String = plain.label
  def sourceref : Option[SourceRef] = plain.sourceref
  override def toString: String = plain.toString
  def onAdd : Unit
  def onOpen: Unit
  def copy: HTMLNode
  def plaincopy : HTMLPlainNode = plain.plaincopy
  def isEmpty : Boolean = plain.isEmpty
  def isVisible : Boolean = plain.isVisible
  def children : List[HTMLNode] = plain.children
  def iterate(f: HTMLNode => Unit): Unit = plain.iterate(f)

  def ancestors: List[HTMLNode] = plain._parent match {
    case Some(p) => p :: p.plain.ancestors
    case _ => Nil
  }

    def collectAncestor[A](f: PartialFunction[HTMLNode, A]): Option[A] = plain._parent match {
    case Some(f(a)) => Some(a)
    case Some(e) => e.plain.collectAncestor(f)
    case _ => None
  }

  def replace(n: HTMLNode) = {
    this.plain._parent = n.plain._parent
    this.plain.startswithWS = n.plain.startswithWS
    this.plain._sourceref = n.sourceref
    this.plain.classes = n.plain.classes
    n.plain.attributes.foreach { case ((a, b), c) => this.plain.attributes((a, b)) = c }
    this.plain._state = n.state
    if (state._top contains n) state._top = Some(this)
    this.plain._parent.foreach(p => p.plain._children.splitAt(this.plain._parent.get.plain._children.indexOf(n)) match {
      case (before, _ :: after) =>
        this.plain._parent.get.plain._children = before ::: this :: after
      case _ =>
        state.throwError("???")
    })
  }

  def get(_label: String = "")(_attributes: (String, String, String)*)(cls: String = ""): List[HTMLNode] = {
    val matches = _label match {
      case "" =>
        _attributes match {
          case Nil =>
            cls match {
              case "" => (n: HTMLNode) => true
              case c => (n: HTMLNode) => n.plain.classes.contains(c)
            }
          case _ =>
            (n: HTMLNode) =>
              _attributes.exists(t => n.plain.attributes.get(t._1, t._2).contains(t._3)) && (cls match {
                case "" => true
                case c => n.plain.classes.contains(c)
              })
        }
      case _ =>
        _attributes match {
          case Nil =>
            cls match {
              case "" => (n: HTMLNode) => n.label == _label
              case c => (n: HTMLNode) => n.label == _label && n.plain.classes.contains(c)
            }
          case _ =>
            (n: HTMLNode) =>
              n.label == _label && _attributes.exists(t => n.plain.attributes.get(t._1, t._2).contains(t._3)) && (cls match {
                case "" => true
                case c => n.plain.classes.contains(c)
              })
        }
    }
    get(matches)
  }

  protected def get(matches: HTMLNode => Boolean): List[HTMLNode] = plain._children.filter(matches) ::: plain._children.flatMap(_.plain.get(matches))

  def delete = plain._parent.foreach { p =>
    p.plain._children = p.plain._children.filterNot(_ == this)
  }

  def add(n: Node): HTMLNode = add(n.toString())

  def add(s: String): HTMLNode =
    state.withParent(this) {
      HTMLParser.apply(s)(state)
      plain._children.head
    }

  def add(n: HTMLNode): Unit = {
    n.plain._parent.foreach(p => p.plain._children = p.plain._children.filterNot(_ == n))
    n.plain._parent = Some(this)
    n.plain._state = this.state
    plain._children ::= n
  }

  def addAfter(n: Node, after: HTMLNode): HTMLNode = addAfter(n.toString(), after)

  def addAfter(s: String, after: HTMLNode): HTMLNode = state.withParent(this) {
    HTMLParser.apply(s)(state)
    val c = plain._children.head
    plain._children = plain._children.tail.take(plain._children.indexOf(after) - 1) ::: c :: plain._children.drop(plain._children.indexOf(after))
    c
  }

  def addAfter(n: HTMLNode, after: HTMLNode): Unit = {
    n.plain._parent.foreach(p => p.plain._children = p.plain._children.filterNot(_ == n))
    n.plain._parent = Some(this)
    n.plain._state = this.state
    plain._children = plain._children.take(plain._children.indexOf(after) + 1) ::: n :: plain._children.drop(plain._children.indexOf(after) + 1)
  }

  def addBefore(n: Node, before: HTMLNode): HTMLNode = addBefore(n.toString(), before)

  def addBefore(s: String, before: HTMLNode): HTMLNode = state.withParent(this) {
    HTMLParser.apply(s)(state)
    val c = plain._children.head
    plain._children = plain._children.tail.take(plain._children.indexOf(before)) ::: c :: plain._children.drop(plain._children.indexOf(before) + 1)
    c
  }

  def addABefore(n: HTMLNode, before: HTMLNode): Unit = {
    n.plain._parent.foreach(p => p.plain._children = p.plain._children.filterNot(_ == n))
    n.plain._parent = Some(this)
    n.plain._state = this.state
    plain._children = plain._children.take(plain._children.indexOf(before)) ::: n :: plain._children.drop(plain._children.indexOf(before))
  }
}
abstract class HTMLNodeWrapper(val inner:HTMLNode) extends HTMLNode {
  def plain = inner.plain

  def onOpen = {}
}

class HTMLPlainNode(var _state: ParsingState, override val namespace: String, var _label: String) extends HTMLNode {
  def onOpen = {}
  override def toString: String = state.present(this)
  def plain = this
  override def state = _state
  override def label = _label

  val attributes = mutable.Map.empty[(String, String), String]
  var classes: List[String] = Nil
  private[xhtml] var _parent: Option[HTMLNode] = None
  var startswithWS = false
  var endswithWS = false
  var _sourceref: Option[SourceRef] = None

  def copy: HTMLPlainNode = {
    val ret = new HTMLPlainNode(state, namespace, label)
    ret.classes = classes
    attributes.foreach(e => ret.attributes(e._1) = e._2)
    children.foreach(c => ret.add(c.copy))
    ret
  }

  override def plaincopy: HTMLPlainNode = {
    val ret = new HTMLPlainNode(state, namespace, label)
    ret.classes = classes
    attributes.foreach(e => ret.attributes(e._1) = e._2)
    children.foreach(c => ret.add(c.plaincopy))
    ret
  }

  def parent = _parent

  private[xhtml] var _children: List[HTMLNode] = Nil

  override def children = _children.reverse

  override def isEmpty: Boolean = _children.forall(_.isEmpty)

  override def isVisible: Boolean = {
    val a = attributes.get((HTMLParser.ns_shtml, "visible"))
    (a.contains("true") || a.isEmpty) && _parent.forall(_.isVisible)
  }

  def isMath: Boolean = namespace == HTMLParser.ns_mml

  override def sourceref = _sourceref

  def addAttribute(key: String, value: String) = key.split(':') match {
    case Array(a, b) =>
      val ns = state.namespaces.getOrElse(a, a)
      attributes((ns, b)) = value
    case Array(a) =>
      attributes((namespace, a)) = value
    case _ =>
      state.throwError("???")
  }

  def onAdd = {}

  @tailrec
  override final def iterate(f: HTMLNode => Unit): Unit = {
    f(this)
    children match {
      case a :: _ => a.plain.iterate(f)
      case _ => successor match {
        case Some(s) => s.plain.iterate(f)
        case _ =>
      }
    }
  }

  @tailrec
  final protected def successor: Option[HTMLNode] = parent match {
    case Some(p) =>
      val ch = p.plain.children
      ch.drop(ch.indexWhere(_.plain == this)).tail.headOption match {
        case Some(h) => Some(h.plain)
        case None =>
          p.plain.successor
      }
    case _ => None
  }

  def node : Node = try {
    XML.loadString(state.present(this, forcenamespace = true, dotop = false).trim.replace("&", "&amp;"))
    //XML.loadString(state.present(this, forcenamespace = true,dotop=false).trim.replace("&nbsp;", "&amp;nbsp;"))
  } catch {
    case o: Throwable =>
      println(o.toString)
      throw o
  }

}

class HTMLText(state: ParsingState, val text: String) extends HTMLPlainNode(state, "", "") {
  override def toString() = text //.replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;").replaceAll("\"","&quot;")

  override def isEmpty = toString() == "" || toString() == HTMLParser.empty.toString || toString() == "&#8205;"

  override def copy: HTMLText = {
    new HTMLText(state, text)
  }

  override def node: Node = scala.xml.Text(text)

  override def plaincopy: HTMLText = {
    new HTMLText(state, text)
  }
}

object HTMLNode {
  def apply(state: ParsingState, label: String, xmlns: String = "") = label.split(':') match {
    case Array(l) if xmlns.nonEmpty =>
      new HTMLPlainNode(state, xmlns, l)
    case Array(nsa, l) if state.namespaces.contains(nsa) =>
      new HTMLPlainNode(state, state.namespaces(nsa), l)
    case Array(l) =>
      new HTMLPlainNode(state, state.namespace, l)
    case _ =>
      ???
  }
}
