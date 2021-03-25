package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.{NotationContainer, TextNotation}
import info.kwarc.mmt.api.{ContentPath, GlobalName, LocalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.api.objects.{Context, OMA, OMID, OMMOD, OMS, OMV, Obj, Term, VarDecl}
import info.kwarc.mmt.api.symbols.{Constant, Declaration}
import info.kwarc.mmt.api.utils.XMLEscaping
import info.kwarc.mmt.odk.{IntegerLiterals, NatLiterals, PosLiterals}
import info.kwarc.mmt.stex.STeX

import scala.util.Try
import scala.xml.{Elem, Node}

object XHTMLOMDoc {
  def notation(parse : String = "", present : String = "", verbalize : String = "") = {
    val nc = new NotationContainer
    if (parse != "") nc.parsingDim.set(TextNotation.parse(parse,NamespaceMap.empty))
    if (present != "") nc.presentationDim.set(TextNotation.parse(present,NamespaceMap.empty))
    if (verbalize != "") nc.verbalizationDim.set(TextNotation.parse(verbalize,NamespaceMap.empty))
    nc
  }

  def toRule(property : String => Boolean, resource : Option[String => Boolean] = None)(f : Node => XHTMLNode) : PartialFunction[Node,XHTMLNode] = {
    case n if n.label.startsWith("stex:") && property(n.label)
      && resource.forall(r => n.attributes.asAttrMap.get("resource").exists(r)) => f(n)
    case n if n.attributes.asAttrMap.get("property").exists(property)
      && resource.forall(r => n.attributes.asAttrMap.get("resource").exists(r)) => f(n)
    case n if {
        val cls = n.attributes.asAttrMap.get("class").map(_.split('_').toList).getOrElse(Nil)
        cls.length == 2 &&
          property(cls.head) && resource.forall(_(cls(1)))
      } => f(n)
  }
}

abstract class XHTMLOMDoc(initial_node : Option[Node] = None) extends XHTMLNode(initial_node) {
  protected def semanticChildrenI(e : XHTMLNode) : List[XHTMLOMDoc] = e match {
    case element: XHTMLOMDoc => List(element)
    case o => o.children.flatMap(semanticChildrenI)
  }
  def semanticChildren = children.flatMap(semanticChildrenI)

  lazy val (property,resource) = initial_node match {
    case Some(n) if n.label.startsWith("stex") =>
      (n.label,n.attributes.asAttrMap.getOrElse("resource",""))
    case Some(n) if ismath =>
      val cls = n.attributes.asAttrMap.get("class").map(_.split('_').toList).getOrElse(Nil)
      if(cls.length == 2) {
        (cls.head,cls(1))
      } else ("","")
    case Some(n) =>
      val am = n.attributes.asAttrMap
      (am.getOrElse("property",""),am.getOrElse("resource",""))
    case _ => ("","")
  }

  def getPreElem(parent : Option[PreParent]) : Option[PreElement] = None
}

abstract class XHTMLModule(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
  def path = Path.parseM(resource,NamespaceMap.empty)
  def name = path.name
  def dpath = path.parent
}

trait ToScript extends XHTMLOMDoc {
  override def node = <script type="application/xml+stex" property={property} resource={resource}>{children.map(_.node)}</script>
}


abstract class XHTMLDecl(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) with ToScript {
  def path = Path.parseS(resource,NamespaceMap.empty)
  def module = path.module
  def name = path.name
  def dpath = module.parent
}

abstract class XHTMLTerm(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
  def toTerm : Term
}
trait HasHeadSymbol extends XHTMLOMDoc {
  def head : ContentPath
}

abstract class XHTMLComponent(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
  override def node = Elem(null,property,XHTML.makeAttributes((("","resource"),resource)),scala.xml.TopScope,false,children.map(_.node):_*)
}

abstract class XHTMLTermComponent(initial_node : Option[Node] = None) extends XHTMLComponent(initial_node) {
  def getTerm : Option[Term] = semanticChildren.collectFirst{
    case t : XHTMLTerm => t.toTerm
  }
}
class XHTMLTypeComponent(initial_node : Option[Node] = None) extends XHTMLTermComponent(initial_node)
class XHTMLDefComponent(initial_node : Option[Node] = None) extends XHTMLTermComponent(initial_node)
class XHTMLArityComponent(initial_node : Option[Node] = None) extends XHTMLComponent(initial_node) {
  def arity = resource
}
class XHTMLNotationFragment(initial_node : Option[Node] = None) extends XHTMLComponent(initial_node) {
  def fragment = resource
}
class XHTMLPrecedence(initial_node : Option[Node] = None) extends XHTMLComponent(initial_node) {
  def prec = resource
}
class XHTMLNotationComponent(initial_node : Option[Node] = None) extends XHTMLComponent(initial_node) {
  def notation(path : ContentPath) : Node = get()().collectFirst{case n:XMHTMLMath => n.children match {
    case List(a) =>
      a.get()().foreach {
        case c if (c.label == "mo" || c.label == "mi") && c.children.forall(_.isInstanceOf[XHTMLText]) =>
          c.attributes(("","data-mmt-symref")) = path.toString
        case _ =>
      }
      a.node
    case _ =>
      ???
  }}.getOrElse{
    print("")
    ???
  }
}
class XHTMLMacroNameComponent(initial_node : Option[Node] = None) extends XHTMLComponent(initial_node) {
  def macroname = resource
}

class XHTMLStexArg(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
  def number = resource.toInt
}

abstract class XHTMLComplexTerm(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) {
  sealed trait SubElem {val elem : XHTMLNode}
  case class TextElem(elem : XHTMLNode) extends SubElem
  case class SemanticElem(elem : XHTMLNode) extends SubElem

  lazy val argstr = children.collectFirst{
    case ac : XHTMLArityComponent =>
      ac.delete
      ac.arity
  }

  override def node: Node = {
    argstr
    super.node
  }

  protected def deconstruct(o : XHTMLNode) : List[SubElem] = o.children.flatMap {
    case e : XHTMLOMDoc => List(SemanticElem(e))
    case e : HasHeadSymbol => List(SemanticElem(e))
    case o if o.isEmpty => Nil
    case o if semanticChildrenI(o).isEmpty => List(TextElem(o))
    case o => deconstruct(o)
  }

  override def addOverlay(url: String): Unit = {
    deconstruct(this).foreach {
      case TextElem(e) =>
        e.addOverlay(url)
      case _ =>
    }
  }

}
class XHTMLOMA(initial_node : Option[Node] = None) extends XHTMLComplexTerm(initial_node) with HasHeadSymbol {
  lazy val (ps,arity,frag) = {
    resource.split('#') match {
      case Array(p,a,f) => (p,a,f)
      case Array(l) => (l,"","")
      case Array(p,a) => (p,a,"")
      case _ =>
        println(resource.split('#').mkString("Array(",",",")"))
        ???
    }
  }
  lazy val head = Path.parseMS(ps,NamespaceMap.empty)

  def args = semanticChildren.collect({case a : XHTMLStexArg => a}).map{arg =>
    arg.semanticChildren.collect {
      case t : XHTMLTerm => t
    } match {
      case List(a) =>
        (arg.number,a.toTerm)
      case _ =>
        ???
    }
  }.sortBy(_._1).map(_._2)
  def toTerm = {
    val t = OMA(OMID(head),args)
    t.metadata.update(STeX.meta_notation,STeX.StringLiterals(frag))
    t
  }
}

class XHTMLOMV(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) {
  lazy val path = Path.parseS(resource)
  def name = path.name
  def toTerm = {
    val t = OMV(name)
    t.metadata.update(STeX.meta_vardecl,OMS(path))
    t
  }
}

class XHTMLOMID(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) with HasHeadSymbol {
  lazy val (ps,frag) = {
    resource.split('#') match {
      case Array(p,f) => (p,f)
      case Array(l) => (l,"")
      case _ =>
        ???
    }
  }
  lazy val head = Path.parseMS(ps,NamespaceMap.empty)
  def toTerm = {
    val t = OMID(head)
    t.metadata.update(STeX.meta_notation,STeX.StringLiterals(frag))
    t
  }
}

class XHTMLTref(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) with HasHeadSymbol {
  var head = Path.parseMS(resource,NamespaceMap.empty)
}

class XHTMLOMNum(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) {
  override def toTerm = children match {
    case (t : XHTMLText) :: Nil =>
      t.text.toDoubleOption match {
        // case Some(db) if db.isValidInt && db>0 => STeX.PosLiterals(BigInt(db.toInt))
        case Some(db) if db.isValidInt && db>=0 => STeX.NatLiterals(BigInt(db.toInt))
        case Some(db) if db.isValidInt => STeX.IntLiterals(BigInt(db.toInt))
        case Some(db) => STeX.RealLiterals(db)
        case _ =>
          ???
      }
    case _ =>
      ???
  }
}