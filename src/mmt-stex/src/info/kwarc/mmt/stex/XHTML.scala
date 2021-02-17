package info.kwarc.mmt.stex

import info.kwarc.mmt.api.utils.File
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource

import scala.collection.mutable
import scala.xml.{Attribute, Elem, MetaData, NamespaceBinding, Node, PrefixedAttribute, TopScope, UnprefixedAttribute}
import scala.xml.parsing.NoBindingFactoryAdapter

object XHTML {
  private lazy val parserFactory = {
    val ret = new SAXFactoryImpl
    ret.setNamespaceAware(true)
    ret
  }
  private lazy val adapter = new NoBindingFactoryAdapter
  private lazy val parser = parserFactory.newSAXParser()

  def parse(f : File) = {
    val ret = adapter.loadXML(new InputSource(File.Reader(f)),parser)
    apply(ret)
  }

  def apply(node : Node,parent : Option[XHTMLNode] = None) = node match {
    case e : Elem if e.label == "html" && parent.isEmpty =>
      new XHTMLDocument(e)
    case e : Elem if e.label == "math" =>
      new XMHTMLMath(e,parent)
    case e : Elem =>
      new XHTMLElem(e,parent)
    case t : scala.xml.Text =>
      new XHTMLText(t,parent)
    case _ =>
      new XHTMLNode(node,parent)
  }
}

object XHTMLNode {
  def apply(prefix : String, label : String, attrs : ((String,String),String)*) = new XHTMLElem(
    Elem(if (prefix == "") null else prefix,label,makeAttributes(attrs),scala.xml.TopScope,true,Nil :_*),
    None)

  def text(s : String) = new XHTMLNode(scala.xml.Text(s),None)


  def makeAttributes(ls : Seq[((String,String),String)]) = ls.foldLeft(scala.xml.Null : MetaData){
    case (e,(("",k),v)) =>
      new UnprefixedAttribute(k,v,e)
    case (e,((p,k),v)) =>
      new PrefixedAttribute(p,k,v,e)
  }//.toSeq.map((k,v) => Attribute.)
}

class XHTMLNode(initial_node : Node,iparent : Option[XHTMLNode]) {
  protected var _parent = iparent
  protected var _prefix = initial_node.prefix
  protected var _label = initial_node.label
  protected val _attributes = mutable.Map.empty[(String,String),String]
  private def fill(a : MetaData) : Unit = a match {
    case UnprefixedAttribute(str, value, data) =>
      _attributes(("",str)) = value.toString()
      fill(data)
    case PrefixedAttribute(str, str1, value, data) =>
      println(str + str1 + value + data)
      fill(data)
    case scala.xml.Null =>
    case _ =>
      ???
  }
  fill(initial_node.attributes)
  protected var _scope = initial_node.scope
  protected var _children = initial_node.child.toList.map(XHTML.apply(_,Some(this)))

  def node : Node = new Node {
    override val label: String = _label
    override val child: collection.Seq[Node] = _children.map(_.node)
    override val prefix = _prefix
    override val attributes = XHTMLNode.makeAttributes(_attributes.toSeq)
    override val scope = _scope
  }
  def add(e : XHTMLNode,before : Option[XHTMLNode] = None) = before.map(ee => _children.indexOf(ee)).getOrElse(-1) match {
    case -1 => _children = _children ::: List(e)
      e._parent = Some(this)
    case i => _children = _children.take(i) ::: e :: _children.drop(0)
      e._parent = Some(this)
  }
  protected def delete(e : XHTMLNode) : Unit = _children = _children.filterNot(_ == e)
  def delete : Unit = _parent.foreach(_.delete(this))

  override def toString: String = node.toString()

  def get(label : String = "")(attributes : (String,String,String)*) : List[XHTMLNode] = {
    val matches = label match {
      case "" =>
        attributes match {
          case Nil =>
            (n : XHTMLNode) => true
          case _ =>
            (n : XHTMLNode) => attributes.exists(t => n._attributes.get(t._1,t._2).contains(t._3))
        }
      case _ =>
        attributes match {
          case Nil =>
            (n : XHTMLNode) => n._label == label
          case _ =>
            (n : XHTMLNode) => n._label == label && attributes.exists(t => n._attributes.get(t._1,t._2).contains(t._3))
        }
    }
    get(matches)
  }
  protected def get(matches : XHTMLNode => Boolean) : List[XHTMLNode] = _children.filter(matches) ::: _children.flatMap(_.get(matches))

  def isEmpty = _children.isEmpty || _children.forall {
    case t : XHTMLText if t.text.trim == "" =>
      true
    case _ => false
  }
}

class XHTMLElem(e : Elem,parent : Option[XHTMLNode]) extends XHTMLNode(e,parent) {
  override def node: Elem = Elem(_prefix,_label,XHTMLNode.makeAttributes(_attributes.toSeq),_scope,true,_children.map(_.node) :_*)
}

class XHTMLText(e : scala.xml.Text,parent : Option[XHTMLNode]) extends XHTMLNode(e,parent) {
  private var _text = e.toString()
  def text = _text

  override def node: scala.xml.Text = scala.xml.Text(text)
}

class XMHTMLMath(e : Elem,parent : Option[XHTMLNode]) extends XHTMLElem(e,parent) {
  //override def toString: String = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\" " +
  //  _attributes.map{case ((p,k),v) => k+"=\"" + v + "\"" }.mkString(" ") + ">" + _children.mkString + "</math>"
  _attributes(("","xmlns")) = "http://www.w3.org/1998/Math/MathML"
}

class XHTMLDocument(e : Elem) extends XHTMLElem(e,None)  {
  val prefix = """<?xml version="1.0" encoding="utf-8"?>
                 |<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN" "http://www.w3.org/Math/DTD/mathml2/xhtml-math11-f.dtd">""".stripMargin

  override def toString: String = prefix + "\n<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + _children.mkString("\n") + "\n</html>"
}