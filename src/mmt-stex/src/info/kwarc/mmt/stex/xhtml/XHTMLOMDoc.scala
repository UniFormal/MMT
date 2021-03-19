package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.{NotationContainer, TextNotation}
import info.kwarc.mmt.api.{ContentPath, LocalName, NamespaceMap, Path}
import info.kwarc.mmt.api.objects.{Context, OMA, OMID, OMMOD, OMS, OMV, Obj, Term, VarDecl}
import info.kwarc.mmt.api.symbols.{Constant, Declaration}
import info.kwarc.mmt.api.utils.XMLEscaping
import info.kwarc.mmt.odk.{IntegerLiterals, NatLiterals, PosLiterals}
import info.kwarc.mmt.stex.STeX

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
    case n if (n.label == "span" || n.label == "div")
      && n.attributes.asAttrMap.get("property").exists(property)
      && resource.forall(r => n.attributes.asAttrMap.get("resource").exists(r)) => f(n)
    case n if n.label == "mrow"
      && {
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
    case Some(n) if n.label == "span" || n.label == "div" =>
      val am = n.attributes.asAttrMap
      (am.getOrElse("property",""),am.getOrElse("resource",""))
    case Some(n) if n.label == "mrow" =>
      val cls = n.attributes.asAttrMap.get("class").map(_.split('_').toList).getOrElse(Nil)
      if(cls.length == 2) {
        (cls.head,cls(1))
      } else ("","")
    case _ => ("","")
  }
}

abstract class XHTMLModule(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
  def path = Path.parseM(resource,NamespaceMap.empty)
  def name = path.name
  def dpath = path.parent
  def toModule(controller : Controller) : info.kwarc.mmt.api.modules.Module
}

class XHTMLTheory(initial_node : Option[Node] = None) extends XHTMLModule(initial_node) {
  def toModule(controller : Controller) = Theory(dpath,name,None)
}

abstract class XHTMLDecl(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
  def path = Path.parseS(resource,NamespaceMap.empty)
  def module = path.module
  def name = path.name
  def dpath = module.parent
  def toDeclaration : Declaration
  def bind(tm : Term) = {
    val variables = semanticChildren.collect {
      case v : XHTMLVarDecl => v
    }
    variables.foldRight(tm){case (v,t) =>
      if (v.universal.contains(true) || v.universal.isEmpty) STeX.Forall(Context(v.vardecl),t) else STeX.Exists(Context(v.vardecl),t)
    }
  }
}
abstract class XHTMLTerm(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
  def toTerm : Term
}
trait HasHeadSymbol extends XHTMLOMDoc {
  def head : ContentPath
}

class XHTMLTheorem(initial_node : Option[Node] = None) extends XHTMLDecl(initial_node) {
  def toDeclaration = {
    val termI = semanticChildren.collect{case tm : XHTMLTerm => tm.toTerm} match {
      case List(tm) => tm
      case _ =>
        ???
    }
    val term = bind(termI)
    Constant(OMMOD(module),name,Nil,Some(OMS(STeX.prop)),Some(term),None)
  }
}

class XHTMLVarDecl(initial_node : Option[Node] = None) extends XHTMLDecl(initial_node) {
  def unescape(s : String) : String = {
    val s1 = XMLEscaping.unapply(s)
    if (s1 == s) s else unescape(s1)
  }
  private lazy val vdnode : XHTMLNode = {
    val str = children.mkString.trim
    XHTML.applyString(unescape(str))(Nil).filter(!_.isInstanceOf[XHTMLText]).head
  }
  override def path = Path.parseS(vdnode.attributes.getOrElse(("","path"),{
    print("")
    ???
  }))
  override def name = path.name
  def notation = vdnode.attributes.getOrElse(("","notation"),name.toString)
  def universal = vdnode.attributes.get(("","quantified")) match {
    case Some("universal") => Some(true)
    case Some("existential") => Some(false)
    case _ => None
  }
  def tp : Option[Term] = vdnode.children.find(_.label == "type").map(_.children.head.node).map(Obj.parseTerm(_,NamespaceMap.empty))
  def toDeclaration = {
    val c = Constant(OMMOD(path.module),name,Nil,tp,None,Some("variable"),XHTMLOMDoc.notation("\\" + name,notation))
    c.metadata.update(STeX.meta_quantification,OMS(
      if (universal.contains(true) || universal.isEmpty) STeX.Forall.path
      else STeX.Exists.path
    ))
    c
  }
  def vardecl = VarDecl(name,None,tp,None,if (notation.isEmpty) None else Some(TextNotation.parse(notation,NamespaceMap.empty)))
}

class XHTMLStexArg(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
  def number = resource.toInt
}

abstract class XHTMLComplexTerm(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) with HasHeadSymbol {
  def head = Path.parseMS(resource,NamespaceMap.empty)
  sealed trait SubElem {val elem : XHTMLNode}
  case class TextElem(elem : XHTMLNode) extends SubElem
  case class SemanticElem(elem : XHTMLNode) extends SubElem

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
class XHTMLOMA(initial_node : Option[Node] = None) extends XHTMLComplexTerm(initial_node) {
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
  def toTerm =
    OMA(OMID(head),args)
}

class XHTMLOMV(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) {
  def vardecl = iterateUp(this)
  private def iterateUp(e : XHTMLNode) : Option[VarDecl] = e.get(classOf[XHTMLVarDecl]).collectFirst{
    case vd if vd.name == name => vd.vardecl
  } match {
    case Some(vd) => Some(vd)
    case _ if e.parent.isDefined => iterateUp(e.parent.get)
    case _ => None
  }
  var path = Path.parseS(resource)
  def name = path.name
  def toTerm = OMV(name)
}

class XHTMLOMID(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) with HasHeadSymbol {
  var head = Path.parseMS(resource,NamespaceMap.empty)
  def toTerm = OMID(head)
}

class XHTMLTref(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) with HasHeadSymbol {
  var head = Path.parseMS(resource,NamespaceMap.empty)
}

class XHTMLOMNum(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) {
  override def toTerm = children match {
    case (t : XHTMLText) :: Nil =>
      t.text.toDoubleOption match {
        case Some(db) if db.isValidInt && db>=0 => STeX.NatLiterals(BigInt(db.toInt))
        case _ =>
          ???
      }
    case _ =>
      ???
  }
}