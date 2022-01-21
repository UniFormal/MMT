package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.{CPath, ContentPath, NamespaceMap, Path, StructuralElement, TypeComponent}
import info.kwarc.mmt.api.objects.{OMID, OMS, Obj, Term}
import info.kwarc.mmt.api.ontology.{Binary, CustomBinary, RelationalElement, RelationalExtractor, Unary}
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.{Constant, DerivedDeclaration}
import info.kwarc.mmt.api.utils.{MMTSystem, XMLEscaping}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.xhtml.{HTMLConstant, HTMLParser, HTMLRule, HTMLTheory, HasLanguage, OMDocHTML}
import info.kwarc.mmt.stex.xhtml.HTMLParser.HTMLNode
import info.kwarc.mmt.stex.{STeX, translations}

import scala.xml.Node

trait FragmentExtension extends STeXExtension {
  def doConstant(c : Constant, preview : Boolean) : Option[HTMLNode] = None
  def doDerivedDecl(node : HTMLNode, c : DerivedDeclaration, preview : Boolean) : Unit = {}
  def doExpression(node : HTMLNode, tm : Term, component : Option[CPath], preview : Boolean) : Unit = {}
}

object FragmentExtension extends STeXExtension {

  override def start(args: List[String]): Unit = {
    super.start(args)
    if (!server.extensions.contains(SymdocRelational))
      controller.extman.addExtension(SymdocRelational)
  }

  override def serverReturn(request: ServerRequest): Option[ServerResponse] = request.path.lastOption match {
    case Some("fragment") =>
      request.query match {
        case "" =>
          Some(ServerResponse("Empty fragment","txt"))
        case ps =>
          val path = Path.parse(ps)
          doFragment(path)
      }
    case _ => None
  }


  class SymbolDoc(orig : HTMLNode) extends HTMLConstant(orig) with HasLanguage {
    lazy val symbol = Path.parseMS(resource,NamespaceMap.empty)

    override def onAdd: Unit = {
      sstate.foreach { state =>
        collectAncestor {
          case t: HTMLTheory =>
            t.language_theory.foreach{th =>
              val c = Constant(OMID(th.path),state.newName("symboldoc"),Nil,None,Some(STeX.symboldoc(symbol,language,children)),Some("symboldoc"))
              sourceref.foreach(s => SourceRef.update(c,s))
              controller.add(c)
            }
        }
      }
    }
/*
    override def open(state: SemanticParsingState): Unit = state.getParent match {
      case t : TheoryAnnotation =>
        t.languagemodule.foreach {p =>
          p.add(this)
          _parent = Some(p)
        }
      case p : PreParent =>
        p.add(this)
        _parent = Some(p)
      case _ =>
    }

    override def getElement(implicit state: SemanticParsingState): List[StructuralElement] = _parent match {
      case Some(p : PreTheory) =>
        List(Constant(OMID(p.path),p.newName("symboldoc"),Nil,None,Some(STeX.symboldoc(symbol,language,node.children)),Some("symboldoc")))
    }
 */
  }

  override def rules: List[HTMLRule] = List(new HTMLRule {
    override def rule(s: HTMLParser.ParsingState): PartialFunction[HTMLNode, HTMLNode] = {
      case n if property(n).contains("stex:symboldoc") => new SymbolDoc(n)
    }
  })

  def doFragment(path : Path) = {
    controller.getO(path) match {
      case Some(elem) =>
        val (doc,body) = server.emptydoc
        body.attributes(("","style")) = "background-color:white"
        stripMargins(doc)
        val border = body.add(<div style="font-size:small"/>)
        border.add(<font size="+2">{" ☞ "}</font>)
        border.add(<code>{elem.path.toString}</code>)
        border.add(<hr/>)

        val exts = server.extensions.collect {case fe : FragmentExtension => fe}
        elem match {
          case c : Constant =>
            var ret : Option[HTMLNode] = None
            exts.collectFirst{
              case s if {ret = s.doConstant(c,true);ret}.isDefined => ret
            }
            ret match {
              case Some(s) => border.add(s)
              case _ =>
                border.add(getFragment(c))
            }
          case _ =>
            ???
        }
        Some(ServerResponse(doc.toString,"html"))
      case None =>
        Some(ServerResponse("Empty fragment","txt"))
    }
  }

  // TODO LANGUAGE!
  def getFragment(ce : StructuralElement) : String = {
    def text(s : String) = scala.xml.Text(s)
    var ret : List[Path] = Nil
    controller.depstore.query(ce.path,-SymdocRelational.documents)(s => ret ::= s)
    ret.flatMap(controller.getO).headOption match {
      case Some(c : Constant) => c.df match {
        case Some(STeX.symboldoc(_,_,node)) => // TODO language
          return node.map(_.toString()).mkString//XMLEscaping.unapply(str)
        case _ =>
      }
      case _ =>
    }
    ce match {
      case c : Constant =>
        val macroname = OMDocHTML.getMacroName(c)
        (<table>
          <tr><td><b>Type</b></td><td>{c.tp.map(server.xhtmlPresenter.asXML(_,Some(c.path $ TypeComponent))).getOrElse(text("None"))}</td></tr>
          {macroname.foreach{name => OMDocHTML.getNotations(c,controller).map{
            case ("",_,_,n) =>
              <tr><td>{"\\"+name}</td><td>{n}</td></tr>
            case (f,_,_,n) =>
              <tr><td>{"\\"+name+"[" + f + "]"}</td><td>{n}</td></tr>
          }}}
        </table>).toString()
      case _ =>
        ???
    }
  }

  def stripMargins(ltx : HTMLNode) = {
    val body = ltx.get("body")()().head
    body.attributes(("", "style")) = "margin:0;padding:0;"
    val doc = body.get("div")()("ltx_page_main").head
    doc.attributes(("", "style")) = "margin:0;padding:0.1em 0.5em 0.5em 0.5em;"
    doc.get("div")()("ltx_theorem").foreach { e =>
      e.attributes(("", "style")) = "margin:0;"
    }
  }

  def termLink(o : Obj, comp : Option[CPath]) = DocumentExtension.makePostButton(
    server.xhtmlPresenter.asXML(o, comp),
    "/:" + server.pathPrefix + "/expression",
    ("openmath",o.toNode.toString().replace("\n","").replace("\n","")),
    ("component",comp.map(_.toString).getOrElse("None"))
  )

}


object SymdocRelational extends RelationalExtractor with STeXExtension {
  val documents = CustomBinary("documents","documents","has documentation")
  override val allBinary: List[Binary] = List(
    documents
  )

  override val allUnary: List[Unary] = List()

  override def apply(e: StructuralElement)(implicit f: RelationalElement => Unit): Unit = e match {
    case t : Theory =>
      t.getConstants.foreach {
        case c if c.rl.contains("symboldoc") =>
          c.df match {
            case Some(STeX.symboldoc(s,_,_)) =>
              val p = Path.parseMS(s,NamespaceMap.empty)
              f(documents(c.path,p))
              controller.depstore += documents(c.path,p)
            case _ =>
          }
        case _ =>
      }
    case _ =>
  }
}