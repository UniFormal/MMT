package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.informal.Informal
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.{NotationContainer, TextNotation}
import info.kwarc.mmt.api.{ContentPath, DPath, GlobalName, LocalName, MPath, NamespaceMap, Path, StructuralElement}
import info.kwarc.mmt.api.objects.{Context, OMA, OMBIND, OMBINDC, OMID, OMMOD, OMS, OMV, Obj, Term, VarDecl}
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, PlainInclude, Structure}
import info.kwarc.mmt.api.uom.Scala.Opaque
import info.kwarc.mmt.api.utils.{File, URI, XMLEscaping}
import info.kwarc.mmt.odk.{IntegerLiterals, LFX, NatLiterals, PosLiterals}
import info.kwarc.mmt.sequences.Sequences
import info.kwarc.mmt.stex.Extensions.NotationExtractor
import info.kwarc.mmt.stex.{STeX}

import scala.collection.mutable
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

  def toRule(property : String, resource : Option[String => Boolean] = None)(f : (XHTMLNode,XHTMLParsingState) => Unit) : PartialFunction[(XHTMLNode,XHTMLParsingState), Unit] = {
    case (n,s) if n.prefix + ":" + n.label == property && resource.forall(r => n.attributes.get(("","resource")).exists(r)) => f(n,s)
    case (n,s) if n.label == property && resource.forall(r => n.attributes.get(("","resource")).exists(r)) => f(n,s)
    case (n,s) if n.attributes.get(("","property")).contains(property) && resource.forall(r => n.attributes.get(("","resource")).exists(r)) => f(n,s)
    case (n,s) if n.classes.exists(_.startsWith(property)) =>
      val cls = n.classes.find(_.startsWith(property)).get
      n.removeClass(cls)
      cls.split('_') match {
        case Array(p,r) =>
          n.attributes(("","property")) = p
          n.attributes(("","resource")) = r
          f(n,s)
        case Array(p) =>
          n.attributes(("","property")) = p
          f(n,s)
        case _ =>
          print("")
      }
    case _ =>
  }
}

class OMDocAnnotation(node : XHTMLNode) extends XHTMLAnnotation(node) {
  import node._
  val (property,resource) =  if (label.startsWith("stex"))
    (label,attributes.getOrElse(("","resource"),""))
  else (attributes.getOrElse(("","property"),""),attributes.getOrElse(("","resource"),""))
}

trait PreElement {
  type A <: PreElement
  def getElement(implicit state : SemanticParsingState) : List[StructuralElement]
  def subelements : List[A] = List(this.asInstanceOf[A])
  //def path : Path
  protected var _parent : Option[PreParent] = None
  val metadata = mutable.Map.empty[GlobalName,Obj]
  metadata(STeX.meta_source) = STeX.StringLiterals("sTeX")
  protected def findUp(pe : PreElement => Boolean) : Option[PreElement] = if (pe(this)) Some(this) else _parent.flatMap(_.findUp(pe))
  def find(pe : PreElement => Boolean) : Option[PreElement] = findUp(pe)
  def open(state : SemanticParsingState) : Unit = state.getParent match {
    case p : PreParent =>
      p.add(this)
      _parent = Some(p)
    case _ =>
  }
  def close(state : SemanticParsingState) : Unit = {}
}

object PreElement {

  def makeMacroName(macroname : String) = {
    (STeX.meta_macro,STeX.StringLiterals(macroname))
  }

  def getMacroName(se : StructuralElement) = se.metadata.getValues(STeX.meta_macro).collectFirst {
    case STeX.StringLiterals(mn) => mn
  }

  def getNotations(se : StructuralElement,controller : Controller) = {
    controller.depstore.queryList(se.path,NotationExtractor.notation).map(controller.getO).map {
      case Some(c : Constant) =>
        val arity = c.tp match {
          case Some(STeX.notation.tp(_,a)) => a
          case _ =>
            print("")
            ???
        }
        val (fragment,node) = c.df match {
          case Some(STeX.notation(n,f)) => (f,n)
          case _ =>
            print("")
            ???
        }
        (fragment,arity,XHTML.applyString(node))
    }
  }
}



trait PreParent extends PreElement {
/*
  override def find(pe: PreElement => Boolean): Option[PreElement] = if (pe(this)) Some(this) else {
    _children.find(_.find(pe).isDefined) match {
      case Some(c) => Some(c)
      case _ =>
        _parent.flatMap(_.findUp(pe))
    }
  }
 */

  protected var _children : List[PreElement] = Nil
  def children = _children.reverse
  def add(child : PreElement) = _children ::= child
  private var structnames : List[LocalName] = Nil
  def newName(s : String,i : Int =0) : LocalName = if (structnames.contains(LocalName(s))) {
    if (structnames.contains(LocalName(s + i.toString))) newName(s,i+1) else {
      structnames ::= LocalName(s + i.toString)
      LocalName(s + i.toString)
    }
  } else {
    structnames ::= LocalName(s)
    LocalName(s)
  }
}

object SourceRefAnnotation {
  def toOffset(f : File, line : Int, col : Int) : SourcePosition = {
    var (o,l,c) = (0,1,0)
    File.read(f).foreach {
      case _ if l > line =>
        return SourcePosition(o,line,col)
      case _ if l == line && col == c =>
        return SourcePosition(o,line,col)
      case '\n' =>
        c = 0
        l += 1
        o += 1
      case _ =>
        o += 1
        c += 1
    }
    SourcePosition(-1,line,col)
  }
  def toSourceRef(controller : Controller,sra : SourceRefAnnotation) = {
    val file = controller.backend.resolvePhysical(File(sra.file)).map { case (archive, path) =>
      path.foldLeft(archive.narrationBase)((u, s) => u / s)
    }.getOrElse {
      URI(File(sra.file).toURI)
    }
    SourceRef(file, SourceRegion(toOffset(File(sra.file), sra.from._1, sra.from._2), toOffset(File(sra.file), sra.to._1, sra.to._2)))
  }
}

class SourceRefAnnotation(node: XHTMLNode) extends XHTMLAnnotation(node) {
  var file : String = ""
  var from : (Int,Int) = (0,0)
  var to : (Int,Int) = (0,0)
  def setvars = {
    val str = if (node.attributes.get(("","property")).contains("srcref")) {
      node.attributes(("","resource"))
    } else if (node.attributes.contains(("stex","srcref"))) {node.attributes(("stex","srcref"))}
    else ""
    str.split('#') match {
      case Array(f,r) =>
        file = f
        r.drop(1).dropRight(1).split(')') match {
          case Array(b,e) =>
            b.split(';') match {
              case Array(l,c) =>
                from = (l.toInt,c.toInt)
              case _ =>
                print("")
                ???
            }
            e.drop(1).split(';') match {
              case Array(l,c) =>
                to = (l.toInt,c.toInt)
              case _ =>
                print("")
                ???
            }
          case _ =>
            print("")
            ???
        }
      case _ =>
        print("")
        ???
    }
  }
  override def afterPopulating(s : XHTMLParsingState): Unit = if (node.attributes.get(("","property")).contains("srcref")) {
    node.delete
    node.parent.foreach {p =>
      p.addAnnotation(this)
      p.attributes(("stex","srcref")) = node.attributes.getOrElse(("","resource"),"")
      val n = new SourceRefAnnotation(p)
      n.setvars
    }
  } else setvars
}

object InScript {
  def apply(node : XHTMLNode) = node.collectFirstAncestor{case n if n.getAnnotations.exists(_.isInstanceOf[ToScript]) => n}.isDefined
}

trait Plain extends OMDocAnnotation {
  override def afterPopulating(s : XHTMLParsingState): Unit = {
    super.afterPopulating(s)
    node.children.foreach(_.delete)
    if (node.label == "span") node.add(XHTML.empty)
  }
}

abstract class ModuleAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) with PreParent {
  def path = Path.parseM(resource,NamespaceMap.empty)
  def name = path.name
  def dpath = path.parent
}

class TheoryAnnotation(node : XHTMLNode) extends ModuleAnnotation(node) {
  private val top = this

  override type A = PreTheory
  val pt = new PreTheory(node,path)
  def addMeta(mt : Option[MPath]) = {
    languagemodule.foreach(_.metatheory = mt)
    signaturemodule.foreach(_.metatheory = mt)
  }
  var languagemodule : Option[PreTheory] = Some(pt)
  var signaturemodule : Option[PreTheory] = Some(pt)

  override final def subelements: List[A] = List(signaturemodule.toList,languagemodule.toList).flatten.distinct

  override def add(child: PreElement): Unit = pt.add(child)

  // TODO parameters
  override def getElement(implicit state : SemanticParsingState): List[Theory] = {
    ???
  }//state.applySE(Theory(path.doc,path.name,metatheory))
}

class PreTheory(node : XHTMLNode, override val path : MPath) extends ModuleAnnotation(node) {
  private[xhtml] var metatheory : Option[MPath] = Some(STeX.meta)
  var language = ""

  override def open(state: SemanticParsingState): Unit = {}
  override def close(state: SemanticParsingState): Unit = {}

  override def getElement(implicit state : SemanticParsingState): List[Theory] = {
    if (language != "") metadata(STeX.meta_language) = OMS(STeX.language(language))
    List(Theory(path.doc,path.name,metatheory))
  } //state.applySE(Theory(path.doc,path.name,metatheory))
  override def init: Unit = {}
}

trait ToScript extends OMDocAnnotation {
  import node._
  private val srcref = attributes.get(("stex","srcref"))
  attributes.clear()
  label = "script"
  attributes(("","type")) = "application/xml"
  attributes(("","property")) = property
  attributes(("","resource")) = resource
  srcref.foreach{attributes(("stex","srcref")) = _}
  classes.foreach(removeClass)

  override def afterPopulating(s : XHTMLParsingState): Unit = {
    super.afterPopulating(s)
    if (children.isEmpty) add(XHTML.empty)
    get()()().flatMap(_.getAnnotations).foreach {
      case cmp : ComponentAnnotation => cmp.makeScript
      case _ =>
    }
  }
}

abstract class DeclarationAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) with PreElement {
  override type A = DeclarationAnnotation
  /*
  def path = Path.parseS(resource,NamespaceMap.empty)
  def module = path.module
  def name = path.name
  def dpath = module.parent
   */
}

trait HasNotation {
  protected var _notations : List[(String,String,Node)] = Nil
  def addNotation(frag : String, prec : String, node : Node) = _notations ::= (frag,prec,node)
  var arity : String = ""
  var fragment : String = ""
  var precedence : String = ""
}

trait HasType {
  protected var _types : List[Term] = Nil
  def addType(t : Term) = _types ::= t
}

trait HasDefiniens {
  protected var _definientia : List[Term] = Nil
  def addDefiniens(t : Term) = _definientia ::= t
}

trait HasTermArgs {
  protected var _terms : List[Term] = Nil
  def addArg(t : Term) = _terms ::= t
  def getArgs = _terms.reverse
}

class ConstantAnnotation(node : XHTMLNode) extends DeclarationAnnotation(node) with HasNotation with HasType with HasDefiniens {
  def path = Path.parseS(resource,NamespaceMap.empty)

  private var _roles : List[String] = Nil
  private var _macronames : List[String] = Nil
  def addMacro(s : String) = _macronames ::= s
  def addRole(s : String) = _roles ::= s
  def getElement(implicit state : SemanticParsingState) : List[Declaration] = {
    val c = Constant(OMID(path.module),path.name,Nil,
      _types.headOption.map(state.applyTerm),
      _definientia.headOption.map(state.applyTerm),
      _roles.headOption,NotationContainer.empty())
    _macronames.headOption.foreach{ m =>
      val (k,v) = PreElement.makeMacroName(m)
      metadata(k) = v
    }
    metadata(STeX.meta_arity) = STeX.StringLiterals(arity)
    List(c)
  }
}

class StructureAnnotation(node : XHTMLNode) extends DeclarationAnnotation(node) with PreParent {
  var _name : String = ""
  lazy val domain = Path.parseM(resource)
  lazy val name = if (_name.nonEmpty) LocalName(_name) else LocalName(domain)
  var _nonEmpty = false
  def empty = (!_nonEmpty && _children.isEmpty)
  lazy val path = _parent match {
    case Some(p : ModuleAnnotation) => p.path ? name
    case _ =>
      print("")
      ???
  }


  // TODO structures, views, ...
  override def getElement(implicit state : SemanticParsingState) = List(
    if (empty) PlainInclude(domain,path.module) else {
      ???
    }
  )
}

class ComponentAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) with PreElement {
  import node._
  def makeScript = {
    attributes.clear
    label = property
    attributes(("","resource")) = resource
  }

  override def getElement(implicit state: SemanticParsingState): List[StructuralElement] = ???
}

object TermComponentA {
  def clean(node : XHTMLNode) = node.get()()().collectFirst{
    case n if n.label == "math" =>
      n.children.foreach{case c if c.isEmpty => c.delete case _ =>}
      node.children.foreach(_.delete)
      n.children.headOption.foreach(node.add)
      n
  }.getOrElse {
    node.children.foreach{
      case n if !n.getAnnotations.exists(_.isInstanceOf[TermAnnotation]) => n.delete
      case _ =>
    }
    print("")
  }
}

class TermComponentA(node : XHTMLNode) extends ComponentAnnotation(node : XHTMLNode) {
  override def makeScript: Unit = {
    super.makeScript
    TermComponentA.clean(node)
  }
  def getTerm(state : SemanticParsingState) : Term = node.children match {
    case List(t) if XHTMLTerm.is(t) =>
      t.getAnnotations.collectFirst{case t : TermAnnotation => t.toTerm(state)}.get
    case _ =>
      print("")
      ???
  }
}

/*

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

 */

class TypeComponentA(node : XHTMLNode) extends TermComponentA(node) {
  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    if (node.children.nonEmpty) {
      state.getParent match {
        case c: HasType =>
          c.addType(getTerm(state))
        case _ =>
          print("")
          ???
      }
    }
  }
}
class DefComponentA(node : XHTMLNode) extends TermComponentA(node) {
  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    if (node.children.nonEmpty) {
      state.getParent match {
        case c: HasDefiniens =>
          c.addDefiniens(getTerm(state))
        case _ =>
          print("")
          ???
      }
    }
  }
}
class ArityComponentA(node : XHTMLNode) extends ComponentAnnotation(node) with Plain {
  def arity = resource
  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    state.getParent match {
      case c : HasNotation =>
        c.arity = arity
      case _ =>
        print("")
        ???
    }
  }
}
class NotationFragmentComponentA(node : XHTMLNode) extends ComponentAnnotation(node) with Plain {
  def fragment = resource
  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    state.getParent match {
      case c : HasNotation =>
        c.fragment = fragment
      case _ =>
        print("")
        ???
    }
  }
}
class PrecedenceComponentA(node : XHTMLNode) extends ComponentAnnotation(node) with Plain {
  def prec = resource

  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    state.getParent match {
      case c : HasNotation =>
        c.precedence = prec
      case _ =>
        print("")
        ???
    }
  }
}

class NotationComponentA(node : XHTMLNode) extends TermComponentA(node) {
  override def afterPopulating(s : XHTMLParsingState): Unit = {
    TermComponentA.clean(node)
    super.afterPopulating(s)
    node.children match {
      case List(a) =>
        a.strip
        print("")
      case _ =>
        print("")
        ???
    }
  }

  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    state.getParent match {
      case c : HasNotation =>
        c.addNotation(c.fragment,c.precedence,node.children match {case List(a) => a.node case _ =>
          print("")
            ???
        })
      case _ =>
        print("")
        ???
    }
  }
}

class MacronameComponentA(node : XHTMLNode) extends ComponentAnnotation(node) with Plain {
  def macroname = resource

  override def close(state: SemanticParsingState): Unit = {
    super.close(state)
    state.getParent match {
      case c : ConstantAnnotation =>
        c.addMacro(macroname)
      case _ =>
        print("")
        ???
    }
  }
}

class ArgumentAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) {
  import node._
  override val priority: Int = 5
  val num = if (attributes.get(("","property")).contains("stex:arg")) {
    attributes.remove(("","property"))
    val argstr = attributes.getOrElse(("","resource"),{
      print("")
      ???
    })
    attributes.remove(("","resource"))
    argstr.toInt
  } else if (classes.exists(_.startsWith("stex:arg"))) classes.collectFirst {
    case s if s.startsWith("stex:arg_") =>
      removeClass(s)
      val argstr = s.drop(9)
      argstr.toInt
  }.getOrElse {
    print("")
    ???
  } else if (attributes.contains(("stex","arg"))) attributes(("stex","arg")).toInt
  else {
    print("")
    ???
  }
  attributes(("stex","arg")) = num.toString

  override def afterPopulating(s : XHTMLParsingState): Unit = {
    super.afterPopulating(s)
    if (label == "span" && InScript(node)) {
      TermComponentA.clean(node)
      label = "math"
    }
  }

  def toTerm(state : SemanticParsingState) = node.getAnnotations.collectFirst{case t : TermAnnotation => t.toTerm(state)}.getOrElse(XHTMLTerm(node,state))
}


trait TermAnnotation extends XHTMLAnnotation with PreElement {
  def toTerm(state : SemanticParsingState) : Term = XHTMLTerm(node,state)
  def isTop = node.collectFirstAncestor{case a if XHTMLTerm.is(a) => false}.getOrElse(true)

  override def open(state: SemanticParsingState): Unit = {
    state.getParent match {
      case t : PreParent =>
        super.open(state)
      case t : HasTermArgs =>
        super.open(state)
      case _ =>
        print("")
    }
  }

  override def close(state : SemanticParsingState) = {
    state.getParent match {
      case tp : HasTermArgs =>
        val t = toTerm(state)
        tp.addArg(t)
      case p : PreParent =>
        print("") // TODO add to language module
      case _ =>
        print("")
    }
  }

  override def getElement(implicit state: SemanticParsingState): List[StructuralElement] = {
    print("")
    ???
  }
}

trait HasHeadSymbol extends OMDocAnnotation with TermAnnotation {
  lazy val (head,arity,fragment) = resource.split('#') match {
    case Array(p,a,f) => (Path.parseMS(p,NamespaceMap.empty),a,f)
    case Array(l) => (Path.parseMS(l,NamespaceMap.empty),"","")
    case Array(p,a) => (Path.parseMS(p,NamespaceMap.empty),a,"")
    case _ =>
      println(resource.split('#').mkString("Array(",",",")"))
      ???
  }
}

class MathMLAnnotation(override val node : XHTMLNode) extends XHTMLAnnotation(node) with TermAnnotation {
  override def toTerm(state : SemanticParsingState): Term = XHTMLTerm(node,state)

  if (!node.attributes.contains("","xmlns")) node.attributes(("","xmlns")) = "http://www.w3.org/1998/Math/MathML"
}

case class MathMLLiteral(override val node : XHTMLNode) extends MathMLAnnotation(node) {
  override def toTerm(state : SemanticParsingState): Term = node.children.filterNot(_.isEmpty) match {
    case (t : XHTMLText) :: Nil =>
      t.text.toDoubleOption match {
        case Some(db) if db.isValidInt && db>0 => STeX.PosLiterals(BigInt(db.toInt))
        case Some(db) if db.isValidInt && db>=0 => STeX.NatLiterals(BigInt(db.toInt))
        case Some(db) if db.isValidInt => STeX.IntLiterals(BigInt(db.toInt))
        case Some(db) => STeX.RealLiterals(db)
        case _ =>
          ???
      }
    case _ =>
      print("")
      ???
  }
}

class OMIDAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) with TermAnnotation with HasHeadSymbol {
  override val priority = 3

  override def toTerm(state : SemanticParsingState): Term = {
    val tm = OMID(head)
    if (fragment != "") tm.metadata.update(STeX.meta_notation,STeX.StringLiterals(fragment))
    tm
  }
}

trait ComplexTerm extends OMDocAnnotation with TermAnnotation with HasHeadSymbol {
  private def getargs(n : XHTMLNode,state : SemanticParsingState) : List[(Int,Term)] = n.getAnnotations.collectFirst {
    case a : ArgumentAnnotation => List((a.num,a.toTerm(state)))
  }.getOrElse(n.children.flatMap(getargs(_,state)))

  def sortArgs(state : SemanticParsingState) = {
    val args = if (node.getAnnotations.exists(_.isInstanceOf[ArgumentAnnotation]))
      node.children.flatMap(getargs(_,state))
    else getargs(node,state)
    arity.zipWithIndex.map {
      case ('i'|'b',i) =>
        args.filter(_._1 == i+1) match {
          case List(t) =>
            t._2
          case _ =>
            print("")
            ???
        }
      case ('a',i) =>
       STeX.flatseq(args.filter(_._1 == i+1).map(_._2):_*)
      case ('b',i) =>
        // TODO
        ???
    }.toList
  }
}

class OMAAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) with ComplexTerm {
  override val priority = 3

  override def toTerm(state : SemanticParsingState): Term = {
    val tm = OMA(OMID(head),sortArgs(state))
    if (fragment != "") tm.metadata.update(STeX.meta_notation,STeX.StringLiterals(fragment))
    tm
  }
}
class OMBINDAnnotation(node : XHTMLNode) extends OMDocAnnotation(node) with ComplexTerm {
  override val priority = 3

  override def toTerm(state : SemanticParsingState) = {
    // TODO
    var args : List[Term] = Nil
    var ctx : Context = Context.empty
    sortArgs(state).zip(arity).foreach {
      case (tm,'b') =>
        ctx = ctx ++ state.makeBinder(tm)
      case (tm,_) => args ::= tm
    }
    val tm = OMBINDC(OMID(head),ctx,args)
    if (fragment != "") tm.metadata.update(STeX.meta_notation,STeX.StringLiterals(fragment))
    tm
  }
}

object XHTMLTerm {
  def is(node : XHTMLNode) = node.getAnnotations.exists(_.isInstanceOf[TermAnnotation])

  def apply(xn : XHTMLNode,state : SemanticParsingState) : Term = xn.children.filterNot(_.isEmpty) match {
    case List(a) if is(a) =>
      a.getAnnotations.collectFirst{case ta : TermAnnotation => ta.toTerm(state)}.get
    case List(t : XHTMLText) =>
      xn.strip
      STeX.informal.applySimple(xn.node)
    case _ =>
      val args = xn.children.flatMap({
        case c if c.isEmpty => None
        case a if is(a) =>
          a.getAnnotations.collectFirst{case ta : TermAnnotation => ta.toTerm(state)}
        case t: XHTMLText =>
          t.strip
          Some(STeX.informal.applySimple(<mi>{t.node}</mi>))
        case _ =>
          ???
      })
      STeX.informal.applyOp(xn.label, args)
  }

}

/*

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

 */