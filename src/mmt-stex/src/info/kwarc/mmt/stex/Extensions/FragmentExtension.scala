package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.{CPath, ContentPath, NamespaceMap, Path, StructuralElement, TypeComponent}
import info.kwarc.mmt.api.objects.{OMS, Obj, Term}
import info.kwarc.mmt.api.ontology.{Binary, CustomBinary, RelationalElement, RelationalExtractor, Unary}
import info.kwarc.mmt.api.symbols.{Constant, DerivedDeclaration}
import info.kwarc.mmt.api.utils.MMTSystem
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.Extensions.OMDocExtension.{XHTMLLanguage, server}
import info.kwarc.mmt.stex.{STeX, translations}
import info.kwarc.mmt.stex.xhtml.{PreConstant, PreElement, PreParent, PreTheory, XHTML, XHTMLNode, XHTMLOMDoc}

import scala.xml.Node

trait FragmentExtension extends STeXExtension {
  def doConstant(c : Constant, preview : Boolean) : Option[XHTMLNode] = None
  def doDerivedDecl(node : XHTMLNode, c : DerivedDeclaration, preview : Boolean) : Unit = {}
  def doExpression(node : XHTMLNode, tm : Term, component : Option[CPath], preview : Boolean) : Unit = {}
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


  class XHTMLSymbolDoc(initial_node : Option[Node] = None) extends XHTMLOMDoc(initial_node) {
    override def cleanup: Unit = {
      get()().collectFirst {
        case lang : XHTMLLanguage =>
          lang.delete
          attributes(("","language")) = lang.resource
          children.foreach{
            case n if n.isEmpty =>
              n.delete
            case _ =>
          }
      }.getOrElse("")
    }
    lazy val symbol = Path.parseMS(resource,NamespaceMap.empty)
    def language = attributes.getOrElse(("","language"),"")
    override def getPreElem(parent: Option[PreParent]): Option[PreElement] = parent match {
      case Some(p : PreTheory) =>
        p.languagemodule.foreach { m =>
          val c = new PreConstant(m.path ? m.newName("symboldoc"), m)
          c.addDefiniens(STeX.symboldoc(symbol, language, this.children))
          c.addType(OMS(STeX.symboldoc.tp))
          c.addRole("symboldoc")
        }
        None
      case _ =>
        None
    }
  }

  override lazy val xhtmlRules = List(
    XHTMLOMDoc.toRule(_ == "stex:symboldoc")(n => new XHTMLSymbolDoc(Some(n)))
  )

  def doFragment(path : Path) = {
    controller.getO(path) match {
      case Some(elem) =>
        val (doc,body) = server.emptydoc
        body.attributes(("","style")) = "background-color:white"
        stripMargins(doc)
        val border = XHTML(<div style="font-size:small"/>)(Nil).head
        body.add(border)
        border.add(<font size="+2">{" ☞ "}</font>)
        border.add(<code>{elem.path.toString}</code>)
        border.add(<hr/>)

        val exts = server.extensions.collect {case fe : FragmentExtension => fe}
        elem match {
          case c : Constant =>
            var ret : Option[XHTMLNode] = None
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
  def getFragment(ce : StructuralElement) : XHTMLNode = {
    def text(s : String) = scala.xml.Text(s)
    var ret : List[Path] = Nil
    controller.depstore.query(ce.path,-SymdocRelational.documents)(s => ret ::= s)
    ret.flatMap(controller.getO).headOption match {
      case Some(c : Constant) => c.df match {
        case Some(STeX.symboldoc(_,"en",str)) => // TODO language
          return XHTML.applyString(XHTML.unescape(str))(Nil).head
        case _ =>
      }
      case _ =>
    }
    ce match {
      case c : Constant =>
        val macroname = PreElement.getMacroName(c)
        XHTML(<table>
          <tr><td><b>Type</b></td><td>{c.tp.map(server.xhtmlPresenter.asXML(_,Some(c.path $ TypeComponent))).getOrElse(text("None"))}</td></tr>
          {macroname.foreach{name => PreElement.getNotations(c).map{
            case ("",n) =>
              <tr><td>{"\\"+name}</td><td>{n.node}</td></tr>
            case (p,n) =>
              <tr><td>{"\\"+name+"[" + p + "]"}</td><td>{n.node}</td></tr>
          }}}
        </table>)(Nil).head
      case _ =>
        ???
    }
  }

  def stripMargins(ltx : XHTMLNode) = {
    val body = ltx.get("body")().head
    body.attributes(("", "style")) = "margin:0;padding:0;"
    val doc = body.get("div")(("", "class", "ltx_page_main")).head
    doc.attributes(("", "style")) = "margin:0;padding:0.1em 0.5em 0.5em 0.5em;"
    doc.get("div")().foreach { e =>
      if (e.attributes.get(("", "class")).exists(_.contains("ltx_theorem"))) {
        e.attributes(("", "style")) = "margin:0;"
      }
    }
  }

  def termLink(o : Obj, comp : Option[CPath]) = DocumentExtension.makePostButton(
    server.xhtmlPresenter.asXML(o, comp),
    "/:" + server.pathPrefix + "/expression",
    ("openmath",o.toNode.toString().replace("\n","").replace("\n","")),
    ("component",comp.map(_.toString).getOrElse("None"))
  )
}


object SymdocRelational extends RelationalExtractor {
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
            case _ =>
          }
        case _ =>
      }
    case _ =>
  }
}