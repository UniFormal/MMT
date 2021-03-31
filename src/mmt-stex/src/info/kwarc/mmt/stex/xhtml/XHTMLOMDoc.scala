package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.informal.Informal
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.{NotationContainer, TextNotation}
import info.kwarc.mmt.api.{ContentPath, GlobalName, LocalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.api.objects.{Context, OMA, OMID, OMMOD, OMS, OMV, Obj, Term, VarDecl}
import info.kwarc.mmt.api.symbols.{Constant, Declaration}
import info.kwarc.mmt.api.uom.Scala.Opaque
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
       val clsstr = n.attributes.asAttrMap.get("class")
        val cls = clsstr.map(_.split('_').toList).getOrElse(Nil)
        cls.length == 2 &&
          property(cls.head) && resource.forall(_(cls(1)))
      } => f(n)
  }
}

abstract class XHTMLOMDoc(initial_node : Option[Node] = None) extends XHTMLNode(initial_node) {
  override def isEmpty: Boolean = false
  protected def semanticChildrenI(e : XHTMLNode) : List[XHTMLNode] = e match {
    case element: XHTMLOMDoc => List(element)
    case o if o.attributes.contains(("stex","arg")) => List(o)
    case o => o.children.flatMap(semanticChildrenI)
  }
  def semanticChildren = children.flatMap(semanticChildrenI)

  lazy val (property,resource) = initial_node match {
    case Some(n) if n.label.startsWith("stex") =>
      (n.label,n.attributes.asAttrMap.getOrElse("resource",""))
    case Some(n) if n.attributes.asAttrMap.get("class").exists(_.contains("stex:")) =>
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

  private def cond(a : XHTMLNode) = a.getClass == classOf[XHTMLNode] || a.getClass == classOf[XMathML]

  override def cleanup: Unit = {
    val prop = property
    val res = resource
    if (ismath) _children = children.filter{
      case o if o.isEmpty => false
      case _ => true
    }
    children.filter{
      case o if o.isEmpty => false
      case _ => true
    } match {
      case List(a) if cond(a) =>
        _label = a.label
        attributes.clear()
        a.attributes.foreach(p =>attributes(p._1) = p._2)
        attributes(("","property")) = prop
        attributes(("","resource")) = res
        a.children.reverse.foreach(addAfter(_,a))
        a.delete
        print("")
      case _ =>
    }
  }
}

abstract class XHTMLModule(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
  def path = Path.parseM(resource,NamespaceMap.empty)
  def name = path.name
  def dpath = path.parent
}

trait ToScript extends XHTMLOMDoc {
  override def inscript = true
  override def cleanup: Unit = {
    super.cleanup
    _label = "script"
    val prop = property
    val res = resource
    attributes.clear()
    attributes(("","type")) = "application/xml+stex"
    attributes(("","property")) = property
    attributes(("","resource")) = resource
    get()().foreach {_.cleanup}
    _children = _children.filterNot(_.isEmpty)
    if (children.isEmpty) add(XHTML.empty)
  }
  override def node = {
    super.node
  }
  // override def node = <script type="application/xml+stex" property={property} resource={resource}>{children.map(_.node)}</script>
}


abstract class XHTMLDecl(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) with ToScript {
  def path = Path.parseS(resource,NamespaceMap.empty)
  def module = path.module
  def name = path.name
  def dpath = module.parent
}

abstract class XHTMLComponent(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
  override def cleanup: Unit = if (inscript && !ismath) {
    val prop = property
    val res = resource
    attributes.clear()
    _label = prop
    _children = _children.filterNot(_.isEmpty)
    attributes(("","resource")) = resource
  }
  // override def node = Elem(null,property,XHTML.makeAttributes((("","resource"),resource)),scala.xml.TopScope,false,children.map(_.node):_*)
}

abstract class XHTMLTermComponent(initial_node : Option[Node] = None) extends XHTMLComponent(initial_node) {
  override def cleanup: Unit = {
    super.cleanup
    _children = semanticChildren.collectFirst {
      case t : XHTMLMath if t.children.length == 1 =>
        add(t.children.head)
        t.children.head
      case t : XHTMLTerm =>
        add(t)
        t
    }.toList
    getTerm
    print("")
  }
  def getTerm : Option[Term] = semanticChildren.collectFirst{
    case t : XHTMLTerm => t.toTerm
  }

  override def node = if (inscript) {
    Elem(null,property,XHTML.makeAttributes((("","resource"),resource)),scope,false,children.map(_.strip):_*)
  }else super.node
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
class XHTMLNotationComponent(initial_node : Option[Node] = None) extends XHTMLTermComponent(initial_node) {
  def notation(path : ContentPath) : Node = children match {
    case List(t) => t.strip
    case _ =>
      ???
  }
}
class XHTMLMacroNameComponent(initial_node : Option[Node] = None) extends XHTMLComponent(initial_node) {
  def macroname = resource
}

class XHTMLStexArg(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
  lazy val number = resource.toInt
  private var term : Option[XHTMLNode] = None
  private def clean(n : XHTMLNode) : Unit = n.children.filterNot(_.isEmpty) match {
      case List(s : XHTMLText) =>
        val num = number
        n.attributes.clear
        n.attributes(("stex","arg")) = num.toString
      case List(a : XHTMLOMDoc) =>
        children.foreach(_.delete)
        a.attributes(("stex","arg")) = number.toString
        term = Some(a)
        add(a)
      case List(a) =>
        children.foreach(_.delete)
        a.attributes(("stex","arg")) = number.toString
        term = Some(a)
        add(a)
        clean(a)
      case _ =>
    }

  override def cleanup = clean(this)

  override def node: Node = term match {
    case Some(t) =>
      t.node
    case _ =>
      super.node
  }
}

object STeXArg {
  def unapply(xh : XHTMLNode) = xh match {
    case a : XHTMLStexArg =>
      a.children match {
        case List(t: XHTMLTerm) => Some(a.number,t.toTerm)
        case List(c : XHTMLText) =>
          Some(a.number,XMathML.toTermI(a))
        case List(c) =>
          Some(a.number,XMathML.toTermI(c))
        case _ =>
          Some(a.number,XMathML.toTermI(a))
      }
    case o : XHTMLTerm if o.attributes.contains(("stex","arg")) =>
      Some(o.attributes(("stex","arg")).toInt,o.toTerm)
    case o if o.attributes.contains(("stex","arg")) =>
      Some(o.attributes(("stex","arg")).toInt,XMathML.toTermI(o))
    case _ =>
      None
  }
}


abstract class XHTMLTerm(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
  var used = false
  def toTerm : Term = {
    used = true
    get()().foreach {
      case t : XHTMLTerm => t.used = true
      case _ =>
    }
    toTermI
  }
  def toTermI : Term
  def isTop = !used

  override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
    case Some(p : PreTheory) if isTop =>
      p.languagemodule.map{m =>
        toTerm match {
          case OMID(_) =>
            return None
          case t =>
            val name = m.newName("term")
            attributes(("stex","constant")) = (m.path ? name).toString
            new PreTerm(t,m,m.path ? name)
        }
      }
    case _ =>
      None
  }
}
trait HasHeadSymbol extends XHTMLOMDoc {
  def head : ContentPath
}


class XHTMLMath(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) {
  private def cond(n : XHTMLNode) = n.getClass == classOf[XHTMLNode]
  override def cleanup: Unit = children match {
    case List(e) if cond(e) && e.label == "semantics" =>
      children.foreach(_.delete)
      e.children match {
        case List(a) =>
          a.delete
          add(a)
        case List(a,b) if cond(b) && b.label == "annotation-xml" =>
          a.delete
          add(a)
        case _ =>
          e.children.foreach{c => c.delete; add(c)}
      }
    case _ =>
  }
  // attributes(("","xmlns")) = "http://www.w3.org/1998/Math/MathML"

  override val ismath = true

  override def toTermI : Term = children match {
    case List(tm: XHTMLTerm) =>
      tm.toTerm
    case _ =>
      ???
  }

}

object XHTMLComplexTerm {
  def args(n : XHTMLOMDoc) = n.semanticChildren.collect {
    case STeXArg(n,t) =>
      (n,t)
  }.sortBy(_._1).map(_._2)
}

abstract class XHTMLComplexTerm(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) with HasHeadSymbol {
  sealed trait SubElem {val elem : XHTMLNode}
  case class TextElem(elem : XHTMLNode) extends SubElem
  case class SemanticElem(elem : XHTMLNode) extends SubElem

  lazy val argstr = children.collectFirst{
    case ac : XHTMLArityComponent =>
      ac.delete
      ac.arity
  }

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

  def args = XHTMLComplexTerm.args(this)
/*
  def args = semanticChildren.collect({case a : XHTMLStexArg => a}).map{arg =>
    arg.semanticChildren.collect {
      case t : XHTMLTerm => t
    } match {
      case List(a) =>
        (arg.number,a.toTerm)
      case _ =>
        (arg.number,XMathML.toTermI(arg))
    }
  }.sortBy(_._1).map(_._2)

 */

  override def node: Node = {
    argstr
    super.node
  }
}
class XHTMLOMA(initial_node : Option[Node] = None) extends XHTMLComplexTerm(initial_node) {

  def toTermI = {
    val t = OMA(OMID(head),args)
    t.metadata.update(STeX.meta_notation,STeX.StringLiterals(frag))
    t
  }
}

class XHTMLOMBind(initial_node : Option[Node] = None) extends XHTMLComplexTerm(initial_node) {

  def toTermI = {
    val t = OMA(OMID(head),args)
    t.metadata.update(STeX.meta_notation,STeX.StringLiterals(frag))
    t
  }
}

class XHTMLOMV(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) {
  lazy val path = Path.parseS(resource)
  def name = path.name
  def toTermI = {
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
  def toTermI = {
    val t = OMID(head)
    t.metadata.update(STeX.meta_notation,STeX.StringLiterals(frag))
    t
  }
}


class XHTMLOMNum(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) {
  override def toTermI = children match {
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

  override def cleanup: Unit = {}
}

object XMathML {
  def toTermI(xn : XHTMLNode) : Term = xn.children match {
    case List(a : XHTMLTerm) =>
      a.toTerm
    case _ =>
      val args = xn.children.map({
        case m: XHTMLTerm => m.toTerm
        case t: XHTMLText if xn.children == List(t) =>
          return (STeX.informal.applySimple(xn.strip))
        case t: XHTMLText =>
          STeX.informal.applySimple(<mi>t.strip</mi>)
        case _ =>
          ???
      })
      STeX.informal.applyOp(xn.label, args)
  }
}

class XMathML(initial_node : Option[Node] = None) extends XHTMLTerm(initial_node) {
  override def isEmpty : Boolean = label != "mspace" && (_children.isEmpty || _children.forall(_.isEmpty))
  override def toTermI: Term = XMathML.toTermI(this)

  override def strip: Node = if (label == "mspace") {
    val width = attributes.get(("","width"))
    <mspace width={width.getOrElse("0pt")}/>
  } else super.strip
}

// TODO deprecate
class XHTMLTref(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) with HasHeadSymbol {
  var head = Path.parseMS(resource,NamespaceMap.empty)
}