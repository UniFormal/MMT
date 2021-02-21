package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.{File, MMTSystem, XMLEscaping}
import info.kwarc.mmt.api.{LocalName, NamespaceMap, Path}
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource

import java.io.StringReader
import scala.collection.mutable
import scala.xml._
import scala.xml.parsing.NoBindingFactoryAdapter

case class XHTMLRule(f : PartialFunction[(Node,Option[XHTMLNode],List[XHTMLRule]),XHTMLNode])

object XHTML {
  def overlayHeader: List[XHTMLNode] = {
    val full = applyString(MMTSystem.getResourceAsString("mmt-web/stex/overlay.txt"))(Rules.defaultrules)
    val ret = full.head.get("head")().head.children
    ret.foreach(_.delete)
    ret
  }

  object Rules {
    val document_rule = XHTMLRule({case (e : Elem,parent,rules) if e.label == "html" && parent.isEmpty => new XHTMLDocument(e)(rules)})
    val math_rule = XHTMLRule({case (e : Elem,parent,rules) if e.label == "math" => new XMHTMLMath(e,parent)(rules)})
    implicit val defaultrules : List[XHTMLRule] = List(document_rule,math_rule)
  }
  private lazy val parserFactory = {
    val ret = new SAXFactoryImpl
    ret.setNamespaceAware(true)
    ret
  }
  private lazy val adapter = new NoBindingFactoryAdapter
  private lazy val parser = parserFactory.newSAXParser()

  def parse(f : File)(implicit rules : List[XHTMLRule]) = {
    val ret = adapter.loadXML(new InputSource(File.Reader(f)),parser)
    apply(ret)
  }

  def applyString(s : String)(implicit rules : List[XHTMLRule]) = {
    val ret = adapter.loadXML(new InputSource(new StringReader(s)),parser)
    apply(ret)
  }

  def apply(node : Node, parent : Option[XHTMLNode] = None)(implicit rules : List[XHTMLRule]) : List[XHTMLNode] = {
    rules.find(_.f.isDefinedAt(node,parent,rules)) match {
      case Some(r) => List(r.f(node,parent,rules))
      case _ => node match {
        case t : scala.xml.Text =>
          val init = t.toString().takeWhile(_.isWhitespace)
          val end = t.toString().reverse.takeWhile(_.isWhitespace).reverse
          val content = t.toString().drop(init.length).dropRight(end.length)
          List(init,content,end).filterNot(_.isEmpty).map(s => new XHTMLText(scala.xml.Text(s),parent))
        case e : Elem =>
          List(new XHTMLElem(e,parent))
        case a : Atom[String] =>
          apply(scala.xml.Text(a.data),parent)
        case _ =>
          ???
      }
    }
  }
  val empty = scala.xml.Text("\u200E")
}

object XHTMLNode {
  def apply(prefix : String, label : String, attrs : ((String,String),String)*)(implicit rules : List[XHTMLRule]) = new XHTMLElem(
    Elem(if (prefix == "") null else prefix,label,makeAttributes(attrs:_*),scala.xml.TopScope,true,Nil :_*),
    None)

  def text(s : String)(implicit rules : List[XHTMLRule]) = new XHTMLText(scala.xml.Text(s),None)


  def makeAttributes(ls : ((String,String),String)*) = ls.foldLeft(scala.xml.Null : MetaData){
    case (e,(("",k),v)) =>
      new UnprefixedAttribute(k,v,e)
    case (e,((p,k),v)) =>
      new PrefixedAttribute(p,k,v,e)
  }//.toSeq.map((k,v) => Attribute.)
}

abstract class XHTMLNode(initial_node : Node,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) {
  protected var _parent = iparent
  var ismath = false
  def top : XHTMLNode = _parent match {
    case None => this
    case Some(p) => p.top
  }
  def getHead = top.get("head")().head

  def parent = _parent
  var prefix = initial_node.prefix
  var label = initial_node.label
  val attributes = mutable.Map.empty[(String,String),String]
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
  fill(initial_node.attributes)
  var scope = initial_node.scope
  var children = initial_node.child.toList.flatMap(XHTML.apply(_,Some(this)))

  private val self = this

  def node : Node
  def addString(s : String,before : Option[XHTMLNode] = None) = add(scala.xml.Text(s),before)
  def add(e : Node,before : Option[XHTMLNode] = None) : Unit = XHTML.apply(e,Some(this)).foreach(add(_,before))
  def add(e : XHTMLNode,before : Option[XHTMLNode]) : Unit = before.map(ee => children.indexOf(ee)).getOrElse(-1) match {
    case -1 => children = children ::: List(e)
      e._parent = Some(this)
    case i => children = children.take(i) ::: e :: children.drop(i)
      e._parent = Some(this)
  }
  protected def delete(e : XHTMLNode) : Unit = children = children.filterNot(_ == e)
  def delete : Unit = _parent.foreach(_.delete(this))

  override def toString: String = node.toString()

  def iterate(f : XHTMLNode => Unit) : Unit = {
    f(this)
    children.foreach(_.iterate(f))
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
            (n : XHTMLNode) => n.label == _label
          case _ =>
            (n : XHTMLNode) => n.label == _label && _attributes.exists(t => n.attributes.get(t._1,t._2).contains(t._3))
        }
    }
    get(matches)
  }
  protected def get(matches : XHTMLNode => Boolean) : List[XHTMLNode] = children.filter(matches) ::: children.flatMap(_.get(matches))

  var overlayset = false

  def addOverlay(url:String): Unit = {
    val t = this.top
    if (!t.overlayset) {
      t.overlayset = true
      val h = t.getHead
      XHTML.overlayHeader.foreach(h.add(_,None))
      val body = t.get("body")().head
      body.children ::= new XHTMLElem(
        <div class="stexoverlay" id="stexMainOverlay" style="border-style:solid;position:fixed;top:10px">
          <table style="width:80ch;height:100%">
            <tr style="height:28px"><td style="text-align:right"><button onclick="stexOverlayOff('stexMainOverlay')">X</button></td></tr>
            <tr width="100%" height="100%"><td><iframe id="stexoverlayinner" name="stexoverlayinner" onLoad="if (this.contentWindow.location.href=='about:blank') {} else {stexMainOverlayFade();}" width="100%" height="100%"
                            style="margin:0%; padding:0%; display:block;background-color:hsl(210, 20%, 98%)\">{XHTML.empty}</iframe>
            </td></tr>
            </table>
        </div>
        ,Some(body))
    }
    attributes(("","class")) = attributes.get(("","class")) match {
      case Some(s) => s + " stexoverlaycontainer"
      case _ => "stexoverlaycontainer"
    }
    val id = t.generateId
    attributes(("","onmouseover")) = "stexOverlayOn('"+id+"','"+url+"')"
    attributes(("","onmouseout")) = "stexOverlayOff('"+id+"')"
    attributes(("","onmouseclick")) = "alert('" + url + "')"
    val overlay = new XHTMLElem(<span style="position:relative">
      <iframe src=" " class="stexoverlay" id={id}>{XHTML.empty}</iframe>
    </span>,None)
    if (!ismath) {
      val p = parent.get
      p.add(overlay,Some(this))
      this.delete
      p.add(this,Some(overlay))
      print("")
    } else {
      getNonMath.add(overlay,None)
      print("")
    }
  }

  private var _id = 0
  def generateId = {
    _id += 1
    "stexelem" + (_id-1)
  }
  private def getNonMath : XHTMLNode = if (!ismath) this else parent.get.getNonMath

  def isEmpty : Boolean = children.isEmpty || children.forall(_.isEmpty)
}

class XHTMLElem(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLNode(e,iparent) {
  override def node: Elem = Elem(prefix,label,XHTMLNode.makeAttributes(attributes.toSeq:_*),scope,true,children.map(_.node) :_*)
}

class XHTMLText(e : scala.xml.Text,parent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLNode(e,parent) {
  private var _text = e.toString()
  def text = _text

  override def node: scala.xml.Text = scala.xml.Text(text)

  override def isEmpty: Boolean = text.trim.isEmpty

  override def addOverlay(url:String): Unit = {
    val t = this.top
    if (!t.overlayset) {
      t.overlayset = true
      val h = t.getHead
      XHTML.overlayHeader.foreach(h.add(_, None))
      val body = t.get("body")().head
      body.children ::= new XHTMLElem(
        <div class="stexoverlay" id="stexMainOverlay" style="border-style:solid">
          <table style="width:80ch;height:100%">
            <tr style="height:28px">
              <td style="text-align:right">
                <button onclick="stexOverlayOff('stexMainOverlay')">X</button>
              </td>
            </tr>
            <tr width="100%" height="100%">
              <td>
                <iframe id="stexoverlayinner" name="stexoverlayinner" onLoad="if (this.contentWindow.location.href=='about:blank') {} else {stexMainOverlayFade();}" width="100%" height="100%"
                        style="margin:0%; padding:0%; display:block;background-color:hsl(210, 20%, 98%)\">
                  {XHTML.empty}
                </iframe>
              </td>
            </tr>
          </table>
        </div>
        , Some(body))
    }
    val newthis = new XHTMLElem(<span></span>, None)
    newthis.attributes(("", "class")) = "stexoverlaycontainer"
    val id = t.generateId
    newthis.attributes(("", "onmouseover")) = "stexOverlayOn('" + id + "','" + url + "')"
    newthis.attributes(("", "onmouseout")) = "stexOverlayOff('" + id + "')"
    newthis.attributes(("", "onmouseclick")) = "alert('" + url + "')"
    val overlay = new XHTMLElem(<span style="position:relative">
      <iframe src=" " class="stexoverlay" id={id}>
        {XHTML.empty}
      </iframe>
    </span>, None)
    val p = parent.get
    p.add(overlay, Some(this))
    this.delete
    p.add(newthis, Some(overlay))
    newthis.add(this, None)
  }
}

class XMHTMLMath(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLElem(e,iparent) {
  //override def toString: String = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\" " +
  //  _attributes.map{case ((p,k),v) => k+"=\"" + v + "\"" }.mkString(" ") + ">" + _children.mkString + "</math>"
  attributes(("","xmlns")) = "http://www.w3.org/1998/Math/MathML"
  get()().foreach(_.ismath = true)
  ismath = true
}

class XHTMLDocument(e : Elem)(implicit rules : List[XHTMLRule]) extends XHTMLElem(e,None)  {
  private val doc_prefix = "<!DOCTYPE html>" /* """<?xml version="1.0" encoding="utf-8"?>
                 |<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN" "http://www.w3.org/Math/DTD/mathml2/xhtml-math11-f.dtd">""".stripMargin */

  override def toString: String = doc_prefix + "\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:om=\"http://www.openmath.org/OpenMath\" xmlns:stex=\"http://www.mathhub.info\" xmlns:ml=\"http://www.w3.org/1998/Math/MathML\">\n" + children.mkString("\n") + "\n</html>"
}


case class XHTMLSidebar(id:String,ls:Node*)(implicit rules : List[XHTMLRule]) extends XHTMLElem(
  Elem(null,"span",XHTMLNode.makeAttributes(),scala.xml.TopScope,true,
    (<label for={id} class="sidenote-toggle">{XHTML.empty}</label><input type="checkbox" id={id} class="sidenote-toggle"/><span class="sidenote">{ls}</span>) :_*),
  None)