package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.{ContentPath, GlobalName, MPath, Path, StructuralElement}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.{HasMetaData, MetaDatum}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMA, OMFOREIGN, OMID, OMS, VarDecl}
import info.kwarc.mmt.api.ontology.{Binary, CustomBinary, RelationalElement, RelationalExtractor, Unary}
import info.kwarc.mmt.api.opaque.OpaqueXML
import info.kwarc.mmt.api.symbols.{Constant, NestedModule}
import info.kwarc.mmt.stex.STeXServer
import info.kwarc.mmt.stex.rules.{NatLiterals, StringLiterals}
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
    // TODO
  }
  def getTitle(d: Document): Option[Node] = {
    // TODO
    None
  }

  def addNotation(parent:Theory,path: GlobalName, id: String, opprec: Int, component: HTMLNode, op: Option[HTMLNode]) = {
    addNotationTo(parent, path, id, opprec, component, op)
  }

  def addVarNotation(vd : VarDecl, id: String, opprec: Int, component: HTMLNode, op: Option[HTMLNode]) = {
    addNotationTo(vd,meta_notation,id,opprec,component,op)
  }

  private def addNotationTo(parent:HasMetaData,sym:GlobalName,id: String, opprec: Int, component: HTMLNode, op: Option[HTMLNode]) = {
    val init = List(StringLiterals(id), StringLiterals(opprec.toString), OMFOREIGN(toNode(component)))
    parent.metadata.add(MetaDatum(meta_notation, OMA(OMS(sym), op match {
      case Some(op) => init ::: List(OMFOREIGN(toNode(op)))
      case _ => init
    })))
  }
  def getNotations(p: GlobalName): List[STeXNotation] = {
    controller.getO(p) match {
      case Some(c : Constant) => getNotations(c)
      case _ => Nil
    }
  }

  private def toHTML(node : Node) : HTMLNode = {
    val ncomp = present("<mrow>" + node.toString() + "</mrow>")(None)
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

  def getNotations(c:Constant): List[STeXNotation] = {
    var ret: List[Path] = Nil
    controller.depstore.query(c.path, -NotationExtractor.notation)(s => ret ::= s)
    val path = c.path
    ret.flatMap {
      controller.getO(_) match {
        case Some(t: Theory) => t.metadata.getValues(meta_notation).flatMap {
          case OMA(OMS(`path`), List(StringLiterals(id), StringLiterals(opprec), OMFOREIGN(comp), OMFOREIGN(opcomp))) =>
            Some(STeXNotation(c.path.toString,t,id,opprec.toInt,toHTML(comp),Some(toHTML(opcomp))))
          case OMA(OMS(`path`), List(StringLiterals(id), StringLiterals(opprec), OMFOREIGN(comp))) =>
            Some(STeXNotation(c.path.toString, t, id, opprec.toInt, toHTML(comp), None))
          case _ => None
        }
        case _ => Nil
      }
    }
  }

  def addSymdoc(parent:Theory,fors:List[GlobalName],node:Node) = {
    parent.metadata.add(MetaDatum(meta_symdoc,OMA(OMS(meta_symdoc),OMFOREIGN(node) :: fors.map(OMS(_)))))
  }
  def getSymdocs(sym:ContentPath) : List[Node] = {
    var ret : List[Path] = Nil
    controller.depstore.query(sym,-SymdocRelational.documents)(s => ret ::= s)
    ret.flatMap{p => controller.getO(p) match {
      case Some(t) => t.metadata.getValues(meta_symdoc).flatMap {
        case OMA(OMS(`meta_symdoc`),OMFOREIGN(node) :: ls) if ls.contains(OMID(sym)) => Some(node)
        case _ => None
      }
    }}
  }

  def addExample(parent: Theory, fors: List[GlobalName], node: Node) = {
    parent.metadata.add(MetaDatum(meta_example, OMA(OMS(meta_example), OMFOREIGN(node) :: fors.map(OMS(_)))))
  }
}

case class STeXNotation(sym:String, in: Theory, id:String, opprec:Int, notation:HTMLNode, op: Option[HTMLNode]) {
  def present(args:List[List[NodeSeq]]) = {
    if (args.isEmpty) {notation.plain.node} else {
      def replace(old:HTMLNode,news:NodeSeq) = {
        old.plain.parent.foreach { p =>
          p.addAfter(news.toString(), old)
          old.delete
        }
      }
      val nnot = notation.plaincopy
      var replacements: List[(Int,HTMLNode)] = Nil
      nnot.iterate{
        case n if n.plain.attributes.contains((HTMLParser.ns_shtml,"argnum")) && n.plain.attributes((HTMLParser.ns_shtml,"argnum")).length == 1 =>
          replacements ::= (n.plain.attributes((HTMLParser.ns_shtml,"argnum")).toInt - 1,n)
        case _ =>
      }
      replacements.foreach {
        case (i,n) if args(i).length == 1 =>
          replace(n,args(i).head)
        case (i,n) =>
          val res = args(i).tail.foldLeft(args(i).head){ case (b,nn) =>
            val nnode = n.plaincopy
            var reps:List[HTMLNode] = Nil
            nnode.iterate{
              case n if n.plain.attributes.contains((HTMLParser.ns_shtml, "argnum")) &&
                n.plain.attributes((HTMLParser.ns_shtml, "argnum")).length == 2 &&
                n.plain.attributes((HTMLParser.ns_shtml, "argnum"))(1) == 'a' =>
                reps = n :: reps
              case n if n.plain.attributes.contains((HTMLParser.ns_shtml, "argnum")) &&
                n.plain.attributes((HTMLParser.ns_shtml, "argnum")).length == 2 &&
                n.plain.attributes((HTMLParser.ns_shtml, "argnum"))(1) == 'b' =>
                reps = reps ::: n :: Nil
              case _ =>
            }
            assert(reps.length == 2)
            replace(reps.head,b)
            replace(reps.tail.head,nn)
            nnode.node
          }
          replace(n,res)
      }
      nnot.iterate{
        case n if n.plain.attributes.contains((HTMLParser.ns_shtml,"comp")) =>
          if (sym.count(_=='?') > 1) n.plain.attributes((HTMLParser.ns_shtml,"comp")) = sym
          else {
            n.plain.attributes.remove((HTMLParser.ns_shtml,"comp"))
            n.plain.attributes((HTMLParser.ns_shtml,"varcomp")) = sym
          }
        case n if n.plain.attributes.contains((HTMLParser.ns_shtml, "varcomp")) =>
          if (sym.count(_ == '?') > 1) {
            n.plain.attributes.remove((HTMLParser.ns_shtml, "varcomp"))
            n.plain.attributes((HTMLParser.ns_shtml, "comp")) = sym
          }
          else n.plain.attributes((HTMLParser.ns_shtml, "varcomp")) = sym
        case _ =>
      }
      nnot.node
    }
  }
}

object NotationExtractor extends RelationalExtractor with STeXExtension {
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

object SymdocRelational extends RelationalExtractor with STeXExtension {
  val documents = CustomBinary("documents","documents","has documentation")
  override val allBinary: List[Binary] = List(
    documents
  )

  override val allUnary: List[Unary] = List()

  override def apply(e: StructuralElement)(implicit f: RelationalElement => Unit): Unit = e match {
    case t: Theory =>
      t.metadata.getValues(Symbols.meta_symdoc).foreach {
        case OMA(OMS(Symbols.meta_symdoc), _ :: ls) =>
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

object ExampleRelational extends RelationalExtractor with STeXExtension {
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