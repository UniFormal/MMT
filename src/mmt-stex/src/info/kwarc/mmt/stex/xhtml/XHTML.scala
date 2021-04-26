package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.{File, MMTSystem, XMLEscaping}
import info.kwarc.mmt.api.{DPath, ErrorHandler, ErrorThrower, LocalName, NamespaceMap, OpenCloseHandler, Path}
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource

import java.io.StringReader
import scala.annotation.tailrec
import scala.collection.mutable
import scala.xml._
import scala.xml.parsing.NoBindingFactoryAdapter

import info.kwarc.mmt.api.Error

class XHTMLAnnotation(val node : XHTMLNode) {
  val priority = 0
  def init = node.addAnnotation(this)
  def afterPopulating(s : XHTMLParsingState) : Unit = {}
  init
}

class XHTMLParsingState(rules : List[PartialFunction[(XHTMLNode,XHTMLParsingState), Unit]] = Nil, error : ErrorHandler = ErrorThrower) {
  def withErrors[A](f : => A) = try {Some(f)} catch {
    case e : Error =>
      error(e)
      None
  }
  def open(node : XHTMLNode) = {
    rules.foreach(r => withErrors{r.apply(node,this)})
    node match {
      case _ if List("math","mrow","mi","mo","msub","msup","mtext","mfrac","mspace","munderover","merror","mtext","omnum","munder","mstyle").contains(node.label) =>
          new MathMLAnnotation(node)
      case _ if node.label == "mn" =>
        MathMLLiteral(node)
      case _ =>
    }
  }
  def close(node : XHTMLNode) = {
    node.getAnnotations.foreach(r => withErrors(r.afterPopulating(this)))
  }
}

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
/*
  object Rules {
    implicit val defaultrules : List[PartialFunction[(XHTMLNode,Unit]] = List(
      {case e : Elem if e.label == "html" => new XHTMLDocument},
      {case e : Elem if e.label == "math" => new XHTMLMath(Some(e))},
      {case e : Elem if e.label == "svg" =>
        val ne = new XHTMLNode(Some(e)) {}
        ne.attributes(("","xmlns")) = "http://www.w3.org/2000/svg"
        ne
      },
      {case e : Elem if List("mrow","mi","mo","msub","msup","mtext","mfrac","mspace","munderover","merror").contains(e.label) => new XMathML(Some(e))}
    )
  }
 */
  private lazy val parserFactory = {
    val ret = new SAXFactoryImpl
    ret.setNamespaceAware(true)
    ret
  }
  private lazy val adapter = new NoBindingFactoryAdapter
  private lazy val parser = parserFactory.newSAXParser()

  def parse(f : File, state : Option[XHTMLParsingState] = None) = {
    val ret = adapter.loadXML(new InputSource(File.Reader(f)),parser)
    apply(ret,state)
  }

  def applyString(s : String, state : Option[XHTMLParsingState] = None) = {
    val ret = adapter.loadXML(new InputSource(new StringReader(s)),parser)
    apply(ret,state)
  }

  def apply(node : Node, state : Option[XHTMLParsingState] = None) : XHTMLNode = {
    val nstate = state.getOrElse(new XHTMLParsingState(Nil))
    applyI(node)(nstate).head
  }

  private def applyI(node : Node)(implicit state : XHTMLParsingState) : List[XHTMLNode] = state.withErrors{
    val top = node match {
      case e : Elem if e.label == "script" && e.attribute("type").map(_.toString()).contains("application/xml")
        && e.child.length == 1 && e.child.head.isInstanceOf[scala.xml.Text] && e.toString().contains("&lt;") => // Parser doesn't handle scripts; considers them opaque strings
        val start = node.toString().takeWhile(_ != '>') + ">"
        val ret = scala.xml.XML.loadString(start + node.child.map(c => unescape(c.toString())).mkString("") + "</script>")
        return applyI(ret)
      case e : Elem if e.label == "html" =>
        val ret = new XHTMLDocument
        state.open(ret)
        node.child.foreach{ c =>
          applyI(c).foreach{ ci =>
            ret.add(ci)
            state.close(ci)
          }
        }
        ret
      case t : scala.xml.Text =>
        val init = t.toString().takeWhile(_.isWhitespace)
        val end = t.toString().reverse.takeWhile(_.isWhitespace).reverse
        val content = t.toString().drop(init.length).dropRight(end.length)
        val ret = List(init,content,end).map(s => new XHTMLText(s))
        ret.foreach{ c =>
          state.open(c)
        }
        return ret
      case a : Atom[String] =>
        val ret = new XHTMLText(a.data)
        state.open(ret)
        ret
      case e : Elem =>
        val ret = new XHTMLNode(Some(node))
        state.open(ret)
        node.child.foreach{ c =>
          applyI(c).foreach{ ci =>
            ret.add(ci)
            state.close(ci)
          }
        }
        ret
      case null =>
        return Nil
    }
    List(top)
  }.getOrElse(Nil)
  val empty = scala.xml.Text("\u200E")
}

class XHTMLNode(initial_node : Option[Node] = None) {
  var prefix = initial_node.map(_.prefix).getOrElse("")
  var label = initial_node.map(_.label).getOrElse("")
  val attributes = mutable.Map.empty[(String,String),String]
  var scope = initial_node.map(_.scope).orNull
  protected var _children : List[XHTMLNode] = Nil
  var parent : Option[XHTMLNode] = None

  private var annotations : List[XHTMLAnnotation] = Nil
  def addAnnotation(a : XHTMLAnnotation) = annotations ::= a
  def getAnnotations = annotations.sortBy(-_.priority)
  def deleteAnnotation(a : XHTMLAnnotation) = annotations = annotations.filterNot(_ == a)

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
  private var _classes = attributes.get(("","class")).map(_.split(' ').toList).getOrElse(Nil).filterNot(_ == "")
  attributes.remove(("","class"))
  def addClass(cls : String) = if (!_classes.contains(cls)) _classes ::= cls
  def removeClass(cls : String) = _classes = _classes.filterNot(_ == cls)
  def classes = _classes

  def strip : Unit = {
    attributes.remove(("","property"))
    attributes.remove(("","resource"))
    attributes.remove(("stex","arg"))
    _classes = _classes.filterNot(_.startsWith("stex:"))
    children.foreach(_.strip)
  }

  def ancestors : List[XHTMLNode] = if (parent.isDefined) parent.get :: parent.get.ancestors else Nil
  def collectFirstAncestor[A](f : PartialFunction[XHTMLNode,A]) : Option[A] = this match {
    case f(a) => Some(a)
    case _ => parent.flatMap(_.collectFirstAncestor(f))
  }

  def isMathML = collectFirstAncestor {
      case a if a.label == "math" => true
      case a if a.label == "mtext" && a != this => false
    }.getOrElse(false)

  def top : XHTMLNode = parent match {
    case None => this
    case Some(p) => p.top
  }

  def getHead = top.get("head")()().head

  def children = _children


  def node : Node = Elem(prefix,label,XHTML.makeAttributes({
    if (_classes.isEmpty)
      attributes.toSeq.reverse
    else ((("","class"),_classes.mkString(" ")) :: attributes.toSeq.reverse.toList)
  }:_*),scope,true,children.map(_.node) :_*)

  def add(s : String) : Unit = add(XHTML.text(s))
  def add(n : Node) : Unit = add(XHTML(n))
  def add(e : XHTMLNode) : Unit = {
    e.parent = Some(this)
    _children = _children ::: List(e)
  }

  def addAfter(e : XHTMLNode,after : XHTMLNode) : Unit = _children.indexOf(after) match {
    case -1 => _children = _children ::: List(e)
      e.parent = Some(this)
    case i => _children = _children.take(i+1) ::: e :: _children.drop(i+1)
      e.parent = Some(this)
  }

  protected def delete(e : XHTMLNode) : Unit = _children = _children.filterNot(_ == e)
  def delete : Unit = parent.foreach(_.delete(this))

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

  def get(_label : String = "")(_attributes : (String,String,String)*)(cls : String = "") : List[XHTMLNode] = {
    val matches = _label match {
      case "" =>
        _attributes match {
          case Nil =>
            cls match {
              case "" => (n : XHTMLNode) => true
              case c => (n : XHTMLNode) => n._classes.contains(c)
            }
          case _ =>
            (n : XHTMLNode) => _attributes.exists(t => n.attributes.get(t._1,t._2).contains(t._3)) && (cls match {
              case "" => true
              case c => n._classes.contains(c)
            })
        }
      case _ =>
        _attributes match {
          case Nil =>
            cls match {
              case "" => (n : XHTMLNode) => n.label == _label
              case c => (n : XHTMLNode) => n.label == _label && n._classes.contains(c)
            }
          case _ =>
            (n : XHTMLNode) => n.label == _label && _attributes.exists(t => n.attributes.get(t._1,t._2).contains(t._3)) && (cls match {
              case "" => true
              case c => n._classes.contains(c)
            })
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

  def getTopMath : XHTMLNode = if (!parent.get.isMathML) this else parent.get.getTopMath
}

class XHTMLText(init : String) extends XHTMLNode() {
  private var _text = init
  def text = _text

  override def node: scala.xml.Text = scala.xml.Text(text)
  override def strip = {}

  override def isEmpty: Boolean = {
    val trimmed = text.replace('\u2061',' ').trim
    trimmed.isEmpty
  }
}

class XHTMLDocument extends XHTMLNode(None) {
  private val doc_prefix = "<!DOCTYPE html>"

  override def node: Node =
    <html xmlns="http://www.w3.org/1999/xhtml"
          xmlns:om="http://www.openmath.org/OpenMath"
          xmlns:stex="http://www.mathhub.info"
          xmlns:ml="http://www.w3.org/1998/Math/MathML"
    >{children.map(_.node)}</html>

  override def toString: String = doc_prefix + super.toString
}