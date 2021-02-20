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

object XHTMLTerm {
  def notation(parse : String = "", present : String = "", verbalize : String = "") = {
    val nc = new NotationContainer
    if (parse != "") nc.parsingDim.set(TextNotation.parse(parse,NamespaceMap.empty))
    if (present != "") nc.presentationDim.set(TextNotation.parse(present,NamespaceMap.empty))
    if (verbalize != "") nc.verbalizationDim.set(TextNotation.parse(verbalize,NamespaceMap.empty))
    nc
  }
  val theorem_rule = XHTMLRule({case (e : Elem,parent,rules) if (e.label == "span" || e.label=="div") && e.attributes.asAttrMap.get("property").contains("stex:theorem") => new XHTMLTheorem(e,parent)(rules)})
  val vardecl_rule = XHTMLRule({case (e : Elem,parent,rules) if e.label == "script" && e.attributes.asAttrMap.get("property").contains("stex:vardecl") => new XHTMLVarDecl(e,parent)(rules)})
  val oma_rule = XHTMLRule({case (e : Elem,parent,rules) if e.label == "span" && e.attributes.asAttrMap.get("property").contains("stex:OMA") => new XHTMLOMA(e,parent)(rules)})
  val omv_rule = XHTMLRule({case (e : Elem,parent,rules) if e.label == "span" && e.attributes.asAttrMap.get("property").contains("stex:OMV") => new XHTMLOMV(e,parent)(rules)})
  val omid_rule = XHTMLRule({case (e : Elem,parent,rules) if e.label == "span" && e.attributes.asAttrMap.get("property").contains("stex:OMID") => new XHTMLOMID(e,parent)(rules)})
  val arg_rule = XHTMLRule({case (e : Elem,parent,rules) if e.label == "span" && e.attributes.asAttrMap.get("property").contains("stex:arg") => new XHTMLStexArg(e,parent)(rules)})
  val theory_rule = XHTMLRule({case (e : Elem,parent,rules) if e.label == "div" && e.attributes.asAttrMap.get("property").contains("stex:theory") => new XHTMLTheory(e,parent)(rules)})
  val tref_rule = XHTMLRule({case (e : Elem,parent,rules) if e.label == "span" && e.attributes.asAttrMap.get("property").contains("stex:tref") => new XHTMLTref(e,parent)(rules)})
  val omint_rule = XHTMLRule({case (e : Elem,parent,rules) if e.label == "span" && e.attributes.asAttrMap.get("property").contains("stex:OMINT") => new XHTMLOMINT(e,parent)(rules)})

  implicit val rules : List[XHTMLRule] = List(theory_rule,theorem_rule,vardecl_rule,oma_rule,omv_rule,omid_rule,omint_rule,tref_rule,arg_rule) ::: XHTML.Rules.defaultrules
}

abstract class XHTMLOmdocElement(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLElem(e,iparent) {
  protected def semanticChildrenI(e : XHTMLNode) : List[XHTMLOmdocElement] = e match {
    case element: XHTMLOmdocElement => List(element)
    case o => o.children.flatMap(semanticChildrenI)
  }
  def semanticChildren = children.flatMap(semanticChildrenI)
}

abstract class XHTMLModule(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLOmdocElement(e,iparent) {
  def path = Path.parseM(attributes(("","resource")),NamespaceMap.empty)
  def name = path.name
  def dpath = path.parent
  def toModule(controller : Controller) : info.kwarc.mmt.api.modules.Module
}

class XHTMLTheory(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLModule(e,iparent) {
  def toModule(controller : Controller) = {
    controller.getO(path) match {
      case Some(th : Theory) => th
      case _ =>
        val th = Theory(dpath,name,None)
        controller.add(th)
        th
    }
  }
}

abstract class XHTMLDecl(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLOmdocElement(e,iparent) {
  def path = Path.parseS(attributes(("","resource")),NamespaceMap.empty)
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
abstract class XHTMLTerm(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLOmdocElement(e,iparent) {
  def toTerm : Term
}
trait HasHeadSymbol extends XHTMLElem {
  def head : ContentPath
}

class XHTMLTheorem(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLDecl(e,iparent) {
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

class XHTMLVarDecl(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLDecl(e,iparent) {
  def unescape(s : String) : String = {
    val s1 = XMLEscaping.unapply(s)
    if (s1 == s) s else unescape(s1)
  }
  private lazy val vdnode = children.filterNot(_.isEmpty) match {
    case List(t : XHTMLText) =>
      val n = XHTML.applyString(unescape(t.toString)).head
      children = List(n)
      n
    case _ =>
      ???
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
  def tp : Option[Term] = vdnode.children.collectFirst {
    case e if e.prefix == "om" && e.label == "type" =>
      e.children.collectFirst {
        case tpN : XHTMLElem if tpN.prefix == "om" =>
          Obj.parseTerm(tpN.node,NamespaceMap.empty)
      }
  }.flatten
  def toDeclaration = {
    val c = Constant(OMMOD(path.module),name,Nil,tp,None,Some("variable"),XHTMLTerm.notation("\\" + name,notation))
    if (universal.contains(true) || universal.isEmpty) c.metadata.update(STeX.meta_quantification,OMS(STeX.Forall.path))
    else c.metadata.update(STeX.meta_quantification,OMS(STeX.Exists.path))
    c
  }
  def vardecl = VarDecl(name,None,tp,None,if (notation.isEmpty) None else Some(TextNotation.parse(notation,NamespaceMap.empty)))
}
class XHTMLStexArg(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLOmdocElement(e,iparent) {
  def number = attributes(("","resource")).toInt
}
trait XHTMLComplexTerm extends XHTMLTerm with HasHeadSymbol {
  def head = Path.parseMS(attributes(("","resource")),NamespaceMap.empty)
  sealed trait SubElem {val elem : XHTMLNode}
  case class TextElem(elem : XHTMLNode) extends SubElem
  case class SemanticElem(elem : XHTMLNode) extends SubElem

  protected def deconstruct(o : XHTMLNode) : List[SubElem] = o.children.flatMap {
    case e : XHTMLTerm => List(SemanticElem(e))
    case e : HasHeadSymbol => List(SemanticElem(e))
    case o if o.isEmpty => Nil
    case o if semanticChildrenI(o).isEmpty => List(TextElem(o))
    case o => deconstruct(o)
  }

  override def addOverlay(url: String): Unit = {
    deconstruct(this).foreach {
      case TextElem(e) => e.addOverlay(url)
      case _ =>
    }
  }

}
class XHTMLOMA(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLTerm(e,iparent) with XHTMLComplexTerm {

  var args = semanticChildren.collect({case a : XHTMLStexArg => a}).map{arg =>
    arg.semanticChildren.collect {
      case t : XHTMLTerm => t
    } match {
      case List(a) =>
        (arg.number,a.toTerm)
      case _ =>
        ???
    }
  }.sortBy(_._1).map(_._2)
  def toTerm = OMA(OMID(head),args)
}
class XHTMLOMV(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLTerm(e,iparent) {
  def vardecl = iterateUp(this)
  private def iterateUp(e : XHTMLNode) : Option[VarDecl] = e.get(classOf[XHTMLVarDecl]).collectFirst{
    case vd if vd.name == name => vd.vardecl
  } match {
    case Some(vd) => Some(vd)
    case _ if e.parent.isDefined => iterateUp(e.parent.get)
    case _ => None
  }
  var path = Path.parseS(attributes(("","resource")))
  def name = path.name
  def toTerm = OMV(name)
}
class XHTMLOMID(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLTerm(e,iparent) with HasHeadSymbol {
  var head = Path.parseMS(attributes(("","resource")),NamespaceMap.empty)
  def toTerm = OMID(head)
}

class XHTMLTref(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLElem(e,iparent) with HasHeadSymbol {
  var head = Path.parseMS(attributes(("","resource")),NamespaceMap.empty)
}

class XHTMLOMINT(e : Elem,iparent : Option[XHTMLNode])(implicit rules : List[XHTMLRule]) extends XHTMLTerm(e,iparent) {
  def value = BigInt(attributes(("","resource")))

  override def toTerm: Term = if (value >= 0) STeX.NatLiterals(value) else ??? // TODO
}