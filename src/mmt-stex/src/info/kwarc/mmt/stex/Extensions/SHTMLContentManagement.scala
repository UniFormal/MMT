package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.checking.{CheckingEnvironment, ObjectChecker}
import info.kwarc.mmt.api.{ContentPath, DPath, GetError, GlobalName, MPath, NamespaceMap, Path, RuleSet, StructuralElement}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.{HasMetaData, MetaDatum}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{Context, OMA, OMFOREIGN, OMID, OMS, OMV, Term, VarDecl}
import info.kwarc.mmt.api.ontology.{Binary, CustomBinary, RelationalElement, RelationalExtractor, Unary}
import info.kwarc.mmt.api.opaque.{OpaqueChecker, OpaqueElement, OpaqueElementInterpreter, OpaqueXML}
import info.kwarc.mmt.api.symbols.{Constant, NestedModule}
import info.kwarc.mmt.stex.{SHTML, STeXServer}
import info.kwarc.mmt.stex.rules.{IntLiterals, RulerRule, StringLiterals}
import info.kwarc.mmt.stex.xhtml.{HTMLNode, HTMLParser, HTMLText}

import scala.xml.{Node, NodeSeq}

object Symbols {

  import info.kwarc.mmt.stex.SHTML._

  val meta_language = mmtmeta_path ? "language"
  val macroname_sym = mmtmeta_path ? "macroname"
  val arity_sym = mmtmeta_path ? "arity"
  val reorder_sym = mmtmeta_path ? "reorder"
  val meta_notation = mmtmeta_path ? "notation"
  val meta_symdoc = mmtmeta_path ? "symboldoc"
  val meta_example = mmtmeta_path ? "example"
  val assoctype_sym = mmtmeta_path ? "assoctype"

}
trait SHTMLContentManagement { this : STeXServer =>
  import Symbols._
  def addLanguage(s : String, target:HasMetaData) = {
    target.metadata.update(meta_language, StringLiterals(s))
  }
  def getLanguage(target:HasMetaData) = target.metadata.getValues(meta_language).headOption map {
    case StringLiterals(s) => s
  }

  def addMacroName(name: String, target: HasMetaData) = {
    target.metadata.update(macroname_sym, StringLiterals(name))
  }
  def getMacroName(target: HasMetaData) = target.metadata.getValues(macroname_sym).headOption map {
    case StringLiterals(s) => s
  }

  def addArity(args: String, target: HasMetaData) = {
    target.metadata.update(arity_sym, StringLiterals(args))
  }
  def getArity(target:HasMetaData) = {
    target.metadata.getValues(arity_sym).headOption map {
      case StringLiterals(s) => s
    }
  }

  def addAssoctype(atp : String,target:HasMetaData) = {
    target.metadata.update(assoctype_sym,StringLiterals(atp))
  }
  def getAssoctype(target:HasMetaData) = {
    target.metadata.getValues(assoctype_sym).headOption map {
      case StringLiterals(s) => s
    }
  }
  def addReorder(reorder: String, target: HasMetaData) = {
    target.metadata.update(reorder_sym, StringLiterals(reorder))
  }

  def getReorder(target: HasMetaData) : Option[List[Int]] = {
    target.metadata.getValues(reorder_sym).headOption map {
      case StringLiterals(s) if s.forall(c => c.isDigit || c == ',') => s.split(',').map(_.toInt).toList
      case _ => return None
    }
  }

  import info.kwarc.mmt.api.documents.Document
  def addTitle(d: Document, t: Node) = {
    if (d.metadata.getValues(SHTML.title).isEmpty) {
      d.metadata.update(SHTML.title, OMFOREIGN(t))
    }
  }
  def getTitle(d: Document): Option[Node] = {
    d.metadata.getValues(SHTML.title).headOption match {
      case Some(OMFOREIGN(node)) => Some(node)
      case _ => None
    }
  }

  def addNotation(parent:Theory,path: GlobalName, id: String, opprec: Int,argprecs:List[Int], component: HTMLNode, op: Option[HTMLNode]) = {
    addNotationTo(parent, path, id, opprec,argprecs, component, op)
  }

  def addVarNotation(vd : VarDecl, id: String, opprec: Int,argprecs:List[Int], component: HTMLNode, op: Option[HTMLNode]) = {
    addNotationTo(vd,meta_notation,id,opprec,argprecs,component,op)
  }

  private def addNotationTo(parent:HasMetaData,sym:GlobalName,id: String, opprec: Int,argprecs:List[Int], component: HTMLNode, op: Option[HTMLNode]) = {
    val init = List(StringLiterals(id), IntLiterals(opprec),SHTML.flatseq(argprecs.map(IntLiterals(_))), OMFOREIGN(toNode(component)))
    parent.metadata.add(MetaDatum(meta_notation, OMA(OMS(sym), op match {
      case Some(op) => init ::: List(OMFOREIGN(toNode(op)))
      case _ => init
    })))
  }
  def getNotations(p : HasMetaData): List[STeXNotation] = p match {
    case c: Constant => getNotationsC(c)
    case vd: VarDecl if vd.metadata.getValues(SHTML.headterm).nonEmpty =>
      vd.metadata.getValues(SHTML.headterm).head match {
        case OMS(c) => controller.getO(c) match {
          case Some(p) => getNotations(p)
          case _ => getNotationOther(p)
        }
        case _ => getNotationOther(p)
      }
    case o => getNotationOther(o)
  }
  private def getNotationOther(o : HasMetaData) = o.metadata.getValues(meta_notation).flatMap {
    case OMA(OMS(p), List(StringLiterals(id), IntLiterals(opprec), SHTML.flatseq(precs), OMFOREIGN(comp), OMFOREIGN(opcomp))) =>
      Some(STeXNotation(p.toString, None, id, opprec.toInt, precs.map(IntLiterals.unapply(_).get.toInt), toHTML(comp), Some(toHTML(opcomp))))
    case OMA(OMS(p), List(StringLiterals(id), IntLiterals(opprec), SHTML.flatseq(precs), OMFOREIGN(comp))) =>
      Some(STeXNotation(p.toString, None, id, opprec.toInt, precs.map(IntLiterals.unapply(_).get.toInt), toHTML(comp), None))
    case _ => None
  }
  def getNotations(p: GlobalName): List[STeXNotation] = {
    controller.getO(p) match {
      case Some(c : Constant) => getNotationsC(c)
      case _ => Nil
    }
  }

  def getNotationsC(c: Constant): List[STeXNotation] = {
    var ret: List[Path] = Nil
    controller.depstore.query(c.path, -NotationExtractor.notation)(s => ret ::= s)
    val path = c.path
    ret.flatMap {
      controller.getO(_) match {
        case Some(t: Theory) => t.metadata.getValues(meta_notation).flatMap {
          case OMA(OMS(`path`), List(StringLiterals(id), IntLiterals(opprec),SHTML.flatseq(precs), OMFOREIGN(comp), OMFOREIGN(opcomp))) =>
            Some(STeXNotation(c.path.toString, Some(t), id, opprec.toInt,precs.map(IntLiterals.unapply(_).get.toInt), toHTML(comp), Some(toHTML(opcomp))))
          case OMA(OMS(`path`), List(StringLiterals(id), IntLiterals(opprec),SHTML.flatseq(precs), OMFOREIGN(comp))) =>
            Some(STeXNotation(c.path.toString, Some(t), id, opprec.toInt,precs.map(IntLiterals.unapply(_).get.toInt), toHTML(comp), None))
          case _ => None
        }
        case _ => Nil
      }
    }
  }

  private def toHTML(node : Node) : HTMLNode = {
    //val ncomp = present("<mrow>" + node.toString() + "</mrow>",false)(None)
    val ncomp = HTMLParser("<mrow>" + node.toString() + "</mrow>")(new HTMLParser.ParsingState(controller,Nil))
    var toremoves : List[HTMLNode] = Nil
    ncomp.iterate {
      case n if n.plain.attributes.contains((HTMLParser.ns_shtml,"visible")) =>
        toremoves ::= n
      case n : HTMLText if n.text == "\u200D" =>
        toremoves ::= n
      case _ =>
    }
    toremoves.foreach(_.delete)
    ncomp.plain.children.head
  }
  private def toNode(html:HTMLNode) : Node = {
    var toremoves: List[HTMLNode] = Nil
    html.iterate {
      case n if n.plain.attributes.contains((HTMLParser.ns_shtml, "visible")) =>
        toremoves ::= n
      case n: HTMLText if n.text == "\u200D" =>
        toremoves ::= n
      case _ =>
    }
    toremoves.foreach(_.delete)
    html.plain.node
  }

  def addSymdoc(parent:Theory,fors:List[GlobalName],node:Node,language:String) = {
    parent.metadata.add(MetaDatum(meta_symdoc,OMA(OMS(meta_symdoc),StringLiterals(language) :: OMFOREIGN(node) :: fors.map(OMS(_)))))
  }
  def getSymdocs(sym:ContentPath,language:String) : List[Node] = {
    var ret : List[Path] = Nil
    controller.depstore.query(sym,-SymdocRelational.documents)(s => ret ::= s)
    val us = ret.flatMap{p => controller.getO(p) match {
      case Some(t) => t.metadata.getValues(meta_symdoc).flatMap {
        case OMA(OMS(`meta_symdoc`),StringLiterals(lang) :: OMFOREIGN(node) :: ls) if ls.contains(OMID(sym)) => Some((lang,node))
        case _ => None
      }
    }}
    val langs = us.filter(_._1 == language)
    val defaults = us.filter(p => p._1 == "en" && !langs.contains(p))
    val rest = us.filterNot(p => langs.contains(p) || defaults.contains(p))
    (langs ::: defaults ::: rest).map(_._2)
  }

  def addExample(parent: Theory, fors: List[GlobalName], node: Node) = {
    parent.metadata.add(MetaDatum(meta_example, OMA(OMS(meta_example), OMFOREIGN(node) :: fors.map(OMS(_)))))
  }

  def getRuler(tm: Term, ctx: Context) = {
    val rules = try {
      RuleSet.collectRules(ctrl, ctx).get(classOf[RulerRule]).toList
    } catch {
      case _: GetError =>
        Nil
    }

    def get(tm: Term): Option[HasMetaData] = rules.collectFirst {
      case rl if rl(ctrl, get, tm).isDefined =>
        rl(ctrl, get, tm).get
    }

    get(tm) match {
      case Some(c) => Some(c)
      case _ => tm match {
        case OMS(gn) => ctrl.getO(gn) match {
          case Some(c: Constant) => Some(c)
          case _ => None
        }
        case OMV(x) => ctx.findLast(_.name == x) match {
          case Some(vd) => vd.metadata.getValues(SHTML.headterm).headOption match {
            case Some(OMS(p)) => controller.getO(p) match {
              case Some(c : Constant) => Some(c)
              case _ => Some(vd)
            }
            case _ => Some(vd)
          }
          case None => None
        }
        case _ => None
      }
    }

  }
}

case class STeXNotation(sym:String, in: Option[Theory], id:String, opprec:Int,argprecs:List[Int], notation:HTMLNode, op: Option[HTMLNode]) {
  private trait Replacement {
    val index : Int
  }
  private case class Simple(orig:HTMLNode) extends Replacement {
    val index = orig.plain.attributes((HTMLParser.ns_shtml,"argnum")).toInt - 1
  }
  private case class Sep(orig:HTMLNode) extends Replacement {
    val index = {
      val h = orig.children.head
      val idx = h.plain.children.head.plain.attributes((HTMLParser.ns_shtml, "argnum")).toInt - 1
      h.delete
      idx
    }
  }
  private case class Tis(orig:HTMLNode) extends Replacement {
    val index = -1
  }


  def present(args:List[List[NodeSeq]],withprec:Int=0,isvar:Boolean = false,tis:Option[NodeSeq] = None) = {
    var not = (if (args.isEmpty && op.isDefined) op.get else notation).plaincopy
    var donethis = tis.isEmpty
    not.iterate {
      case n if n.plain.attributes.contains((HTMLParser.ns_shtml, "comp")) =>
        if (!isvar) n.plain.attributes((HTMLParser.ns_shtml, "comp")) = sym
        else {
          n.plain.attributes.remove((HTMLParser.ns_shtml, "comp"))
          n.plain.attributes.remove((HTMLParser.ns_shtml, "maincomp"))
          n.plain.attributes((HTMLParser.ns_shtml, "varcomp")) = sym
        }
      case n if n.plain.attributes.contains((HTMLParser.ns_shtml, "maincomp")) =>
        if (!isvar) n.plain.attributes((HTMLParser.ns_shtml, "maincomp")) = sym
        else {
          n.plain.attributes.remove((HTMLParser.ns_shtml, "comp"))
          n.plain.attributes.remove((HTMLParser.ns_shtml, "maincomp"))
          n.plain.attributes((HTMLParser.ns_shtml, "varcomp")) = sym
        }
      case n if n.plain.attributes.contains((HTMLParser.ns_shtml, "varcomp")) =>
        if (!isvar) {
          n.plain.attributes.remove((HTMLParser.ns_shtml, "varcomp"))
          n.plain.attributes((HTMLParser.ns_shtml, "comp")) = sym
        }
        else n.plain.attributes((HTMLParser.ns_shtml, "varcomp")) = sym
      case _ =>
    }
    if (isvar) not.plain.attributes((HTMLParser.ns_mmt,"variable")) = sym
    def replace(old:HTMLNode,news:NodeSeq) = {
      old.plain.parent.foreach { p =>
        p.addAfter(<mrow>{news}</mrow>.toString(), old)
        old.delete
      }
    }
    var replacements: List[Replacement] = Nil
    not.iterate{
      case n if n.plain.attributes.contains((HTMLParser.ns_shtml, "maincomp")) && tis.nonEmpty =>
        replacements ::= Tis(n)
        donethis = true
      case n if n.plain.attributes.contains((HTMLParser.ns_shtml,"argnum")) && n.plain.attributes((HTMLParser.ns_shtml,"argnum")).length == 1 =>
        replacements ::= Simple(n)
      case n if n.plain.attributes.contains((HTMLParser.ns_shtml, "argsep")) =>
        replacements ::= Sep(n)
      case n if n.plain.attributes.contains((HTMLParser.ns_shtml, "argmap")) =>
        // TODO
        print("")
      case _ =>
    }
    replacements.foreach {
      case Tis(n) =>
        replace(n,{<msub>{n.plain.node}<mrow>{tis.get}</mrow></msub>})
      case s@Simple(n) if args.isDefinedAt(s.index) && args(s.index).length == 1 =>
        replace(n,args(s.index).head)
      case s@Simple(n) if args.isDefinedAt(s.index) =>
        val ls = args(s.index)
        if (ls.isEmpty)
          replace(n,{<mrow></mrow>})
        else {
          val ns: NodeSeq = ls.flatMap(n => List(n,{<mo>,</mo>})).dropRight(1).flatten
          replace(n,{<mrow>{ns}</mrow>})
        }
      case s@Simple(n) =>
        replace(n,{<mo>_</mo>})
      case s@Sep(nd) if args.isDefinedAt(s.index) =>
        val ls = args(s.index)
        if (ls.isEmpty) replace(nd, {<mrow></mrow>})
        else {
          val ns : NodeSeq = ls.flatMap(n => n.toList ::: nd.children.map(_.plain.node)).dropRight(nd.children.length)
          replace(nd, {<mrow>{ns}</mrow>})
        }
      case _ =>
        print("")
    }
    val ret = if (opprec > withprec)
      <mrow>
        <mo class="opening" stretchy="true">(</mo>
        {not.node}
        <mo class="closing" stretchy="true">)</mo>
      </mrow>
    else not.node
    if (donethis) ret else {
      <mover>
        <mover>
          {ret}<mo>⏞</mo>
        </mover> <mrow>
        <mtext>this:</mtext>{tis.get}
      </mrow>
      </mover>
    }
  }
}

object NotationExtractor extends RelationalExtractor {
  val notation = CustomBinary("notation", "has notation for", "has notation in")
  override val allBinary: List[Binary] = List(
    notation
  )

  override val allUnary: List[Unary] = List()
  override def apply(e: StructuralElement)(implicit f: RelationalElement => Unit): Unit = e match {
    case t : Theory =>
      t.metadata.getValues(Symbols.meta_notation).foreach {
        case OMA(OMS(sym), _) =>
          f(notation(t.path,sym))
        case _ =>
      }
      t.getPrimitiveDeclarations.foreach {
        case nm : NestedModule => apply(nm.module)
        case _ =>
      }
    case _ =>
  }
}

object SymdocRelational extends RelationalExtractor {
  val documents = CustomBinary("documents","documents","has documentation")
  override val allBinary: List[Binary] = List(
    documents
  )

  override val allUnary: List[Unary] = List()

  override def apply(e: StructuralElement)(implicit f: RelationalElement => Unit): Unit = e match {
    case t: Theory =>
      t.metadata.getValues(Symbols.meta_symdoc).foreach {
        case OMA(OMS(Symbols.meta_symdoc), _ :: _ :: ls) =>
          ls.foreach {
            case OMS(sym) => f(documents(t.path,sym))
          }
        case _ =>
      }
      t.getPrimitiveDeclarations.foreach {
        case nm: NestedModule => apply(nm.module)
        case _ =>
      }
    case _ =>
  }
}

object ExampleRelational extends RelationalExtractor {
  val exemplifies = CustomBinary("example","has example for","has example in")
  override val allBinary: List[Binary] = List(
    exemplifies
  )

  override val allUnary: List[Unary] = List()

  override def apply(e: StructuralElement)(implicit f: RelationalElement => Unit): Unit = e match {
    case t: Theory =>
      t.metadata.getValues(Symbols.meta_example).foreach {
        case OMA(OMS(Symbols.meta_example), _ :: ls) if ls.forall(_.isInstanceOf[OMID]) =>
          ls.foreach {
            case OMS(sym) => f(exemplifies(t.path,sym))
          }
        case _ =>
      }
      t.getPrimitiveDeclarations.foreach {
        case nm: NestedModule => apply(nm.module)
        case _ =>
      }
    case _ =>
  }
}

object Definienda extends OpaqueElementInterpreter with OpaqueChecker {
  case class Def(override val parent:DPath,id : String,fors:List[GlobalName]) extends OpaqueXML(parent,"definiendum",
    <df id={id} fors={fors.map(_.toString).mkString(", ")}/>,Nil)
  override type OE = Def

  /** the format of [[OpaqueElement]]s this can interpret */
  override def format: String = "definiendum"

  /** constructs an [[OpaqueElement]] from a raw string */
    import info.kwarc.mmt.api.utils.xml._
  override def fromNode(parent: DPath, nsMap: NamespaceMap, nodes: NodeSeq): OE = nodes.head match {
    case n @ <df/> => Def(parent,attr(n,"id"),attr(n,"fors").split(", ").map(Path.parseS(_)).toList)
  }

  override def check(oC: ObjectChecker, context: Context, rules: RuleSet, oe: OpaqueElement)(implicit ce: CheckingEnvironment): Unit = {}
}