package info.kwarc.mmt.stex.xhtml

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents.{Document, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.odk.LFX
import info.kwarc.mmt.sequences.Sequences
import info.kwarc.mmt.stex.Extensions.STeXExtension
import info.kwarc.mmt.stex.STeX

import scala.collection.mutable
import scala.xml.Node

object PreElement {
  def extract(elem : XHTMLNode)(implicit controller : Controller) = {
    var top : Option[PreDocument] = None
    def extractInner(elem : XHTMLNode,parent: Option[PreParent]) : Unit = {
      def recurse(p: => Option[PreParent]) = elem.children.map(extractInner(_, p))
      val nextparent = (parent,top) match {
        case (Some(p),_) => Some(p)
        case (_,o) => o
      }
      elem match {
        case e : XHTMLOMDoc =>
          e.getPreElem(nextparent) match {
            case Some(el : PreDocument) =>
              top match {
                case Some(p) if el.level > p.level =>
                  p.add(el)
                case _ =>
                  top = Some(el)
              }
              recurse(Some(el))
            case Some(el : PreParent) =>
              nextparent.foreach(_.add(el))
              recurse(Some(el))
            case Some(el) =>
              nextparent.foreach(_.add(el))
              recurse(nextparent)
            case _ =>
              recurse(nextparent)
          }
        case _ =>
          recurse(nextparent)
      }
    }
    extractInner(elem,top)
    implicit val transformer = new Transformer
    def convert(pe : PreElement, parent : StructuralElement) : Unit = {
      val ne = pe.getElement
      controller add ne
      (parent,ne) match {
        case (d : Document,t:Theory) =>
          controller add MRef(d.path,t.path)
        case _ =>
      }
      pe match {
        case pp : PreParent =>
          pp.children.reverse.foreach(convert(_,ne))
          controller.simplifier(ne)
        case _ =>
      }
    }
    top match {
      case Some(pd) =>
        val doc = pd.getElement
        controller add doc
        pd.children.reverse.foreach(convert(_,doc))
        doc
      case _ =>
        ???
    }
  }

  def addMacroName(se : StructuralElement,macroname : String) = {
    se.metadata.update(STeX.meta_macro,STeX.StringLiterals(macroname))
  }

  def addNotations(se : StructuralElement,nots : (String,Node)*) = {
    val oldnots = se.metadata.getValues(STeX.meta_notation).flatMap {
      case OMA(OMS(Sequences.flatseq.path),ls) =>
        ls
      case _ =>
        ???
    }
    val nnots = nots.map(p => LFX.Tuple(STeX.StringLiterals(p._1),STeX.StringLiterals(p._2.toString()))).toList ::: oldnots
    if (nnots.nonEmpty)
      se.metadata.update(STeX.meta_notation,Sequences.flatseq(nnots:_*))
  }

  def getMacroName(se : StructuralElement) = se.metadata.getValues(STeX.meta_macro).collectFirst {
    case STeX.StringLiterals(mn) => mn
  }

  def getNotations(se : StructuralElement) = se.metadata.getValues(STeX.meta_notation).flatMap {
    case OMA(OMS(Sequences.flatseq.path),ls) =>
      ls.map{
        case LFX.Tuple(STeX.StringLiterals(frag),STeX.StringLiterals(node)) =>
          (frag,XHTML.applyString(node)(Nil).head)
        case _ =>
          ???
      }
    case _ =>
      ???
  }
}

class Transformer {
  private var _transforms : List[PartialFunction[Term,Term]] = Nil
  def add(pf : PartialFunction[Term,Term]) = _transforms ::= pf
  private val traverser = new StatelessTraverser {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = {
      val ret =_transforms.foldLeft(t)((it,f) => f.unapply(it).getOrElse(it))
      Traverser(this,ret)
    }
  }
  def apply(tm : Term) : Term = traverser(tm,())
}

abstract class PreElement {
  type A <: StructuralElement
  protected def getElementI(implicit transformer : Transformer) : A
  def getElement(implicit transformer : Transformer) = {
    val r = getElementI
    metadata.toList.foreach(p => r.metadata.add(MetaDatum(p._1,p._2)))
    r
  }
  val path : Path
  val _parent : Option[PreParent] = None
  val metadata = mutable.Map.empty[GlobalName,Obj]
  metadata(STeX.meta_source) = STeX.StringLiterals("sTeX")
  protected def findUp(pe : PreElement => Boolean) : Option[PreElement] = if (pe(this)) Some(this) else _parent.flatMap(_.findUp(pe))
  def find(pe : PreElement => Boolean) : Option[PreElement] = findUp(pe)
}


trait PreParent extends PreElement {
  protected var _children : List[PreElement] = Nil
  def children = _children
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

  override def find(pe: PreElement => Boolean): Option[PreElement] = if (pe(this)) Some(this) else {
    _children.find(_.find(pe).isDefined) match {
      case Some(c) => Some(c)
      case _ =>
        _parent.flatMap(_.findUp(pe))
    }
  }
}

class PreDocument(val namespace : String) extends PreParent {
  var level : Int = 0
  type A = Document
  val path = Path.parseD(namespace.trim,NamespaceMap.empty)
  // TODO Sections
  override def getElementI(implicit transformer : Transformer): Document = new Document(path)
}

class PreConstant(val path : GlobalName, val parent : PreParent) extends PreElement {
  private var _types : List[Term] = Nil
  private var _notations : List[(String,Node)] = Nil
  def addNotation(frag : String, node : Node) = _notations ::= (frag,node)
  private var _definientia : List[Term] = Nil
  private var _roles : List[String] = Nil
  private var _macronames : List[String] = Nil
  var arity : String = ""
  def addMacro(s : String) = _macronames ::= s
  def addRole(s : String) = _roles ::= s
  def addType(t : Term) = _types ::= t
  def addDefiniens(t : Term) = _definientia ::= t
  override val _parent = Some(parent)
  parent.add(this)
  // TODO multiple types, multiple definitions, roles
  type A = Constant
  def getElementI(implicit transformer : Transformer) = {
    val c = Constant(OMID(path.module),path.name,Nil,
      _types.headOption.map(transformer.apply),
      _definientia.headOption.map(transformer.apply),
      _roles.headOption,NotationContainer.empty())
    _macronames.headOption.foreach(PreElement.addMacroName(c,_))
    PreElement.addNotations(c,_notations:_*)
    c.metadata.update(STeX.meta_arity,STeX.StringLiterals(arity))
    c
  }
}

class PreRuleConstant(parent : MPath,val path : ContentPath,args : List[Term],rci : RuleConstantInterpreter) extends PreElement {
  type A = RuleConstant
  def getElementI(implicit transformer : Transformer) = {
    rci(parent,OMAorAny(OMID(path),args),true)
  }
}

class PreTheory(val path : MPath, val parent : PreParent) extends PreElement with PreParent {
  parent.add(this)
  override val _parent = Some(parent)
  private var _params : List[Term] = Nil
  var metatheory : Option[MPath] = Some(STeX.meta)

  // TODO metatheory, parameters
  type A = Theory
  override def getElementI(implicit transformer : Transformer): Theory = Theory(path.doc,path.name,metatheory)
}

class PreStructure(val domain : String, val parent : PreParent, val _name : String = "") extends PreElement with PreParent {
  parent.add(this)
  override val _parent = Some(parent)
  val name = if (_name.nonEmpty) LocalName(_name) else parent.newName(domain)
  var _nonEmpty = false
  def empty = (!_nonEmpty && _children.isEmpty)
  val parentpath = parent.path match {
    case mp : MPath => mp
    case gn : GlobalName =>
      gn.module.parent ? (gn.module.name / gn.name)
  }
  val path = parentpath ? LocalName(name)

  type A = Structure
  // TODO structures, views, ...
  override def getElementI(implicit transformer : Transformer) =
    if (empty) PlainInclude(Path.parseM(domain,NamespaceMap.empty),parentpath) else {
      ???
    }
}

class PreFeature(val path : GlobalName, val feature: String,val parent : PreParent) extends PreParent {
  parent.add(this)
  override val _parent = Some(parent)

  type A = DerivedDeclaration
  // TODO everything
  override def getElementI(implicit transformer : Transformer): DerivedDeclaration =
    new DerivedDeclaration(OMID(path.module),LocalName(path.name + "_feature"),feature,TermContainer.empty(),NotationContainer.empty())
}