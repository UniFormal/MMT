package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.{DPath, DefComponent, GlobalName, MPath, NamespaceMap, Path, StructuralElement, TypeComponent}
import info.kwarc.mmt.api.archives.RedirectableDimension
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.{OMA, OMID, OMMOD, OMS, Term}
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.{File, FilePath, MMTSystem}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse, WebQuery}
import info.kwarc.mmt.stex.rules.NatLiterals
import info.kwarc.mmt.stex.vollki.FullsTeXGraph
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState
import info.kwarc.mmt.stex.{ErrorReturn, SHTML, STeXServer}
import info.kwarc.mmt.stex.xhtml.{HTMLNode, HTMLNodeWrapper, HTMLParser, HTMLRule, SHTMLFrame, SHTMLNode, SHTMLRule, SHTMLState}

import scala.util.Try
//import info.kwarc.mmt.stex.xhtml.{HTMLArg, HTMLComp, HTMLLanguageComponent, HTMLNode, HTMLParser, HasHead, NotationComponent}
//import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, HTMLText, ParsingState}

import scala.xml.parsing.XhtmlParser
import scala.xml.{Elem, Node}

trait SHTMLDocumentServer { this : STeXServer =>
  private case class DocParams(q: WebQuery) {
    lazy val path = q.pairs.find(p => p._2.isEmpty && p._1.contains('?') && !p._1.endsWith("="))
      .map(p => Path.parse(p._1))
    lazy val language = q("language")
    lazy val archive = q("archive").flatMap{id => controller.backend.getArchive(id)}
    lazy val filepath = q("filepath")
    lazy val bindings = q("bindings").map(LateBinding.fromNum)
  }
  private case class ErrorResponse(message:String) extends Throwable
  protected def documentRequest(request: ServerRequest) : ServerResponse = try {
    implicit val params : DocParams = DocParams(request.parsedQuery)
    request.path.lastOption match {
      case Some("fulldocument") =>
        var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
        html = html.replace("CONTENT_URL_PLACEHOLDER", "/:" + this.pathPrefix + "/documentTop?" + request.query)
        html = html.replace("BASE_URL_PLACEHOLDER", "")
        html = html.replace("SHOW_FILE_BROWSER_PLACEHOLDER", "false")
        html = html.replace("CONTENT_CSS_PLACEHOLDER","/:" + this.pathPrefix + "/css?" + request.parsedQuery("archive").getOrElse(""))
        ServerResponse(html, "text/html")
      case Some("symbol") =>
        var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
        html = html.replace("CONTENT_URL_PLACEHOLDER", "/:" + this.pathPrefix + "/declaration?" + request.query)
        html = html.replace("BASE_URL_PLACEHOLDER", "")
        html = html.replace("CONTENT_CSS_PLACEHOLDER", "/:" + this.pathPrefix + "/css?")
        ServerResponse(html, "text/html")
      case Some("document") =>
        val ret = doDocument
        val bd = ret.get("div")()("body").head
        bd.plain.attributes.remove((bd.namespace, "style"))
        bd.plain.attributes.remove((bd.namespace, "id"))
        ServerResponse(bd.toString.trim, "text/html")
      case Some("documentTop") =>
        ServerResponse(doDocument.toString.trim, "text/html")
      case Some("fragment") =>
        ServerResponse(doFragment.toString.trim, "text/html")
      case Some("declaration") =>
        doDeclaration
      case Some("css") =>
        ServerResponse(css(request.query),"text/css")
      case Some("variable") =>
        ServerResponse("<b>TODO</b>","text/html")
      case _ =>
        throw ErrorReturn("Unknown path: " + request.path.mkString)
    }
  } catch {
    case ErrorResponse(s) =>
    ServerResponse(s,"txt")
  }
  def css(a : String): String = {
    controller.backend.getArchive(a) match {
      case None => ""
      case Some(a) =>
        var retstr = ""
        iterateLibs(a){f =>
          val css = f / "lib.css"
          if (css.exists()) {
            retstr = File.read(css) + retstr
          }
        }
        retstr
    }
  }

  private case class PresentationRule(key: String, newobj: (String, HTMLParser.ParsingState, HTMLNode) => Option[SHTMLNode], override val priority: Int = 0) extends SHTMLRule(priority) {
    def apply(s: HTMLParser.ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = {
      attrs.find(_._1 == key).flatMap { p =>
        try {
          newobj(p._2, s, n)
        } catch {
          case e =>
            e.printStackTrace()
            None
        }
      }
    }
  }

  def doDocument(implicit dp:DocParams) = {
    val filecontent = getDocument
    val body = filecontent.get("body")()().head
    body
  }

  def getDocument(implicit dp:DocParams): HTMLNode = {
    (dp.archive,dp.filepath) match {
      case (None,_) => throw ErrorResponse("Archive not found")
      case (Some(a),None) =>
        val (doc, bd) = this.emptydoc
        bd.add(<b style="text-align: center">
          {a.id}
        </b>)
        a.properties.collectFirst {
          case ("description", d) if (a.root / "META-INF" / d).exists() =>
            bd.add(File.read(a.root / "META-INF" / d))
          case ("description", d) if (a.root / "meta-inf" / d).exists() =>
            bd.add(File.read(a.root / "meta-inf" / d))
          case ("teaser", t) =>
            bd.add("<div>" + t + "</div>")
        }
        doc
      case (Some(a),Some(path)) =>
        val top = a / RedirectableDimension("xhtml")
        val fn = top / path
        if (fn.exists() && fn.isDirectory) {
          if ((fn / "index.xhtml").exists()) {
            val ret = present(File.read(fn / "index.xhtml"))(dp.bindings)
            ret
          } else {
            val (doc, bd) = this.emptydoc
            bd.add(<b>
              {a.id + "/" + path}
            </b>)
            doc
          }
        } else if (fn.exists()) {
          val ret = present(File.read(fn))(dp.bindings) //HTMLParser(fn)(state)
          ret
        }
        else {
          val (doc, bd) = this.emptydoc
          bd.add(<b>
            {"Document does not exist: " + a.id + "/" + path}
          </b>)
          doc
        }
    }
  }

  def getLanguage(elem: HTMLNode) = {
    val top = getTop(elem)
    ""
    /*
    top.get()((HTMLParser.ns_html, "property", "shtml:language"))().headOption match {
      case Some(l: HTMLLanguageComponent) => l.resource
      case _ => ""
    }
     */
  }

  private def getTop(elem: HTMLNode): HTMLNode = if (elem.label == "body") elem else elem.plain.parent match {
    case Some(p) => getTop(p)
    case _ => elem
  }
/*
  def sidebar(elem: HTMLNode, content: List[Node]) = {
    def parent(e: HTMLNode): Option[(HTMLNode, HTMLNode)] = e.plain.parent match {
      case Some(p) if !p.isMath => Some((p, e))
      case Some(p) => parent(p)
      case None => None
    } //if (e.isMath) parent(e.parent.get) else e

    parent(elem).foreach { case (p, c) =>
      p.addBefore(<div class="sidebar">
        {content}
      </div>, c)
    }
    //val sidenotes = getTop(elem).get()()("sidenote-container").headOption
    //sidenotes.foreach(_.add(<div>{content}</div>))
  }


  def makeButton(urlshort: String, urllong: String, elem: Node, withclass: Boolean = true): Node = // makesafe(XHTML(
    <span class={if (withclass) "propbtn" else ""} style="display:inline" data-overlay-link-click={urllong} data-overlay-link-hover={urlshort}>
      {elem}
    </span>
  //))

  def makePostButton(elem: Node, target: String, data: (String, String)*): Node = // makesafe(XHTML(
    <form method="post" action={target} class="inline" onsubmit="this.target='stexoverlayinner'" target="stexoverlayinner" style="display:none">
      {data.map { p => <input type="hidden" name={p._1} value={p._2} class="inline"/> }}<span onclick="this.closest('form').submit();" type="submit" class="propbtn" style="display:inline">
      {elem}
    </span>
    </form>
  //))

  def makesafe(xh: HTMLNode) = {
    new HTMLNode(xh.state, xh.namespace, xh.label) {
      override def node = xh.node

      override def toString: String = xh.toString
    }
  }
*/


  private def hasAttribute(node:HTMLNode,s:String) = node.plain.attributes.contains((HTMLParser.ns_shtml,s))
  private def getAttribute(node: HTMLNode, s: String) = node.plain.attributes.get((HTMLParser.ns_shtml, s))
  def overlay(elem: HTMLNode/*, urlshort: String, urllong: String*/): Unit = {
    if (elem.plain.classes.contains("overlay-parent")) return
    val id = elem.hashCode().toString
    elem.plain.attributes((elem.namespace, "id")) = id
    elem.plain.classes ::= "hasoverlay-parent"

    //elem.classes ::= "hasoverlay" //:: elem.classes
    def pickelems(n: HTMLNode, inarg: Boolean = false): List[HTMLNode] = n match {
      case comp if hasAttribute(comp, "comp") || hasAttribute(comp, "varcomp") || hasAttribute(comp, "definiendum") =>
        List(comp)
      //case a if hasAttribute(a,"arg") && hasAttribute(a,"term") => Nil
      case a if hasAttribute(a,"arg") => Nil //a.children.flatMap(pickelems(_, true))
      //case t if hasAttribute(t,"term") && inarg => Nil
      case o => o.children.flatMap(pickelems(_, inarg))
    }

    val targets = pickelems(elem)
    targets.foreach { e =>
      e.plain.classes ::= "group-highlight"
      e.plain.attributes((e.namespace, "data-highlight-parent")) = id
      if (hasAttribute(e,"comp")) { getAttribute(e,"comp").foreach{v =>
        val lang = getLanguage(e)
        e.plain.classes ::= "symcomp"
        val urlshort = "/:" + this.pathPrefix + "/fragment?" + v + "&language=" + lang
        val urllong = "/:" + this.pathPrefix + "/declaration?" + v + "&language=" + lang
        e.plain.attributes((elem.namespace, "data-overlay-link-hover")) = urlshort
        e.plain.attributes((elem.namespace, "data-overlay-link-click")) = urllong
      }} else if (hasAttribute(e,"varcomp")) {
        e.plain.classes ::= "varcomp" // TODO
      } else if (hasAttribute(e,"definiendum")) {
        e.plain.classes ::= "definiendum" // TODO
      }
    }
  }

  def doDeclaration(implicit dp:DocParams): ServerResponse = {
    dp.path match {
      case None => throw ErrorResponse("Missing path")
      case Some(path) =>
        controller.getO(path) match {
          case Some(c: Constant) =>
            val (_, body) = this.emptydoc
            body.add(doDeclHeader(c))
            Try(getFragment).toOption match {
              case Some(htm) =>
                body.add(<hr/>)
                body.add(htm)
              case _ =>
            }
            ServerResponse("<body>" + body.toString.trim + "</body>", "text/html")
          case Some(d) =>
            ServerResponse("Not yet implemented: " + d.getClass.toString, "txt")
          case _ =>
            ServerResponse("Declaration not found", "txt")
        }
    }
  }

  def doDeclHeader(c: Constant): Elem = {
    <div>
      <table>
        <tr>
          <td>
            <font size="+2">
              {" ☞ "}
            </font> <code>
            {c.path.toString}
          </code>
          </td> <td>
          {if (controller.extman.get(classOf[FullsTeXGraph.type]).contains(FullsTeXGraph)) {
            <a href={"/:vollki?path=" + c.parent.toString} target="_blank" style="pointer-events:all;color:blue">
              {"> Guided Tour"}
            </a>
          } else <span></span>}
        </td>
        </tr>
      </table> <hr/>
      <table>
        {this.getMacroName(c) match {
        case None => <tr>
          <td></td> <td></td>
        </tr>
        case Some(s) => <tr>
          <td style="padding:3px">
            <b>TeX Macro:</b>
          </td> <td>
            <code>
              {"\\" + s}
            </code>
          </td>
        </tr>
      }}{def td[A](a: A) = <td style="border:1px solid;padding:3px">
        {a}
      </td>

      this.getNotations(c) match {
        case Nil => <tr>
          <td></td> <td></td>
        </tr>
        case ls =>
          <tr>
            <td style="padding-right:3px">
              <b>Notations:</b>
            </td> <td>
            <table>
              <tr>
                {td("identifier")}{td("notation")}{td("operator notation")}{td("in module")}
              </tr>{ls.map(n =>
              <tr>
                {td(n.id match {
                case "" => "(None)"
                case s => s
              })}{td(<math xmlns="http://www.w3.org/1998/Math/MathML">
                {this.htmlpres.doNotation(n.notation.plain.node)}
              </math>)}{td(n.op match {
                case Some(n) => <math xmlns="http://www.w3.org/1998/Math/MathML">
                  {this.htmlpres.doNotation(n.plain.node)}
                </math>
                case None => <span></span>
              })}{td(if (n.in.path != c.parent) n.in.path.toString else "(here)")}
              </tr>
            )}
            </table>
          </td>
          </tr>
      }}{c.tp match {
        case None => <tr>
          <td></td> <td></td>
        </tr>
        case Some(tp) =>
          <tr>
            <td style="padding:3px">
              <b>Type:</b>
            </td> <td>
            {this.xhtmlPresenter.asXML(tp, Some(c.path $ TypeComponent))}
          </td>
          </tr>
      }}{c.df match {
        case None => <tr>
          <td></td> <td></td>
        </tr>
        case Some(df) =>
          <tr>
            <td style="padding:3px">
              <b>Definiens:</b>
            </td> <td>
            {this.xhtmlPresenter.asXML(df, Some(c.path $ DefComponent))}
          </td>
          </tr>
      }}
      </table>
    </div>
  }

  def doFragment(implicit dp:DocParams) = {
    val htm = getFragment
    val (doc, body) = this.emptydoc
    body.plain.attributes((HTMLParser.ns_html, "style")) = "background-color:white"
    stripMargins(doc)
    val border = body.add(<div style="font-size:small"/>)
    //border.add(<font size="+2">{" ☞ "}</font>)
    //border.add(<code>{path.toString}</code>)
    //border.add(<hr/>)
    border.add(htm)
    doc.get("body")()().head
  }

  def getFragment(implicit dp:DocParams) = {
    val path = dp.path match {
      case Some(mp: MPath) =>
        controller.simplifier(mp)
        mp
      case Some(gn: GlobalName) =>
        controller.simplifier(gn.module)
        gn
      case None => throw ErrorResponse("No path given")
    }
    controller.getO(path) match {
      case Some(elem) =>
        elem match {
          case c: Constant =>
            present(getFragmentDefault(c))(dp.bindings)
          case _ =>
            throw ErrorResponse("No symbol with path " + path + " found")
        }
      case _ =>
        throw ErrorResponse("No symbol with path " + path + " found")
    }
  }

  def getFragmentDefault(c : Constant)(implicit dp:DocParams) : String = {
    this.getSymdocs(c.path) match { // TODO language
      case a :: _ => a.toString()
      case _ =>
        val res = "Symbol <b>" + c.name + "</b> in module " + (SourceRef.get(c) match {
          case Some(sr) =>
            controller.backend.resolveLogical(sr.container) match {
              case Some((a, f)) =>
                val url = "/:sTeX/browser/fulldocument" +
                  "?archive=" + a.id + "&filepath=" + (f.init ::: f.last.replace(".tex", ".xhtml") :: Nil).mkString("/")
                s"<a href='${url}'>${c.parent.name.toString}</a>"
              case _ => c.parent.name.toString
            }
          case _ => c.parent.name.toString
        })
        "<div>" + res + "</div>"
    }
  }

  def stripMargins(ltx: HTMLNode) = {
    val body = ltx.get("body")()().head
    body.plain.attributes((HTMLParser.ns_html, "style")) = "margin:0;padding:0;"
    val doc = body.get("div")()("body").head
    doc.plain.attributes((HTMLParser.ns_html, "style")) = "margin:0;padding:0.1em 0.5em 0.5em 0.5em;"
  }

  def present(str : String)(implicit withbindings : Option[LateBinding]) = {
    val ifinputref = withbindings.isDefined
    val bindings = withbindings.getOrElse(new LateBinding)
    var statements : List[Statement] = Nil
    case class Statement(s : String,orig: HTMLNode) extends SHTMLNode(orig) {
      override def onAdd = {
        super.onAdd
        title.foreach(_.plain.classes ::= "shtml-title-" + s)
      }
      def copy = orig.copy
      var title:Option[STitle] = None

      val inline = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "inline"), "true").contains("true")

      if (!inline) bindings.add(StatementStep)
      val styles = plain.attributes.getOrElse((HTMLParser.ns_shtml, "styles"), "").split(',').map(_.trim).toList.filterNot(_.isEmpty)
      plain.classes :::= ("shtml-" + s) :: styles.reverse.map(st => "shtml-" + s + "-" + st)
      val old = plain.attributes.getOrElse((plain.namespace,"style"),"")
      plain.attributes((plain.namespace,"style")) = "counter-set: shtml-statement " + bindings.statement.toString + ";" + old
      statements ::= this
    }
    case class STitle(orig : HTMLNode) extends SHTMLNode(orig) {
      override def onAdd = {
        super.onAdd
        if (isEmpty) plain.classes ::= "empty"
        findAncestor{case s:Statement => s}.foreach(_.title = Some(this))
      }
      def copy = STitle(orig.copy)
    }
    case class Frame(orig : HTMLNode) extends SHTMLFrame(orig) {
      override def init = {}
      orig.plain.classes ::= "frame"
      bindings.add(SlideStep)
      override def onAdd: Unit = {
        val ch = this.children
        val inner = add(<div class="inner-frame"/>)
        ch.foreach(inner.add)
        super.onAdd
      }
    }
    case class SkipSection(orig:HTMLNode) extends SHTMLNode(orig) {
      val lvl = this.plain.attributes.get((HTMLParser.ns_shtml, "skipsection")).map(_.trim.toInt)

      def init = {
        bindings.add(new BlindSectionStep(lvl.getOrElse(-1)))
      }

      override def onAdd: Unit = {
        super.onAdd
        bindings.close
      }

      override def copy = {
        new SkipSection(orig.copy) {
          override def init = {}
        }
      }

      init
    }

      case class Section(orig: HTMLNode) extends SHTMLNode(orig) {
        val lvl = this.plain.attributes.get((HTMLParser.ns_shtml, "section")).map(_.trim.toInt)

        def init = {
          bindings.add(new SectionStep(lvl.getOrElse(-1)))
          val old = plain.attributes.getOrElse((plain.namespace, "style"), "")
          bindings.currlvl match {
            case 1 =>
              this.plain.classes ::= "shtml-sec-part"
              plain.attributes((plain.namespace, "style")) = "counter-set: shtml-sec-part " + bindings.secnum.toString + ";" + old
            case 2 =>
              this.plain.classes ::= "shtml-sec-chapter"
              plain.attributes((plain.namespace, "style")) = "counter-set: shtml-sec-chapter " + bindings.secnum.toString + ";" + old
            case 3 =>
              this.plain.classes ::= "shtml-sec-section"
              plain.attributes((plain.namespace, "style")) = "counter-set: shtml-sec-section " + bindings.secnum.toString + ";" + old
            case 4 =>
              this.plain.classes ::= "shtml-sec-subsection"
              plain.attributes((plain.namespace, "style")) = "counter-set: shtml-sec-subsection " + bindings.secnum.toString + ";" + old
            case _ =>
              this.plain.classes ::= "shtml-sec-paragraph"
              plain.attributes((plain.namespace, "style")) = "counter-set: shtml-sec-paragraph " + bindings.secnum.toString + ";" + old
          }
        }
      override def onAdd: Unit = {
        super.onAdd
        bindings.close
      }
      override def copy = {
        new Section(orig.copy) {
          override def init = {}
        }
      }
      init
    }
    case class FrameNumber(orig:HTMLNode) extends SHTMLNode(orig) {
      def copy = FrameNumber(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.children.foreach(_.delete)
        this.add(bindings.slides.toString)
      }
    }
    case class SecTitle(orig: HTMLNode) extends SHTMLNode(orig) {
      override def onAdd = {
        super.onAdd
        val n = if (this.plain.parent.exists(_.plain.classes.contains("paragraph"))) {
          this.plain.parent.get
        } else this
        if (n.label == "span") {n.plain._label = "div"}
        n.plain.classes ::= "shtml-sectitle"
        if (n.isEmpty) n.plain.classes ::= "empty"
      }

      def copy = SecTitle(orig.copy)
    }
    case class SectionLevel(lvl: Int, orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = SectionLevel(lvl, orig.copy)
      override def onAdd: Unit = {
        super.onAdd
        if (withbindings.isEmpty) bindings.topsec = lvl
      }
    }
    case class Hidable(orig:HTMLNode) extends SHTMLNode(orig) {
      lazy val expanded = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "proofhide"), "") == "true"
      def copy = Hidable(orig.copy)
      var title : Option[HTMLNode] = None
      var body: Option[ProofBody] = None

      override def onAdd: Unit = {
        super.onAdd
        (title,body) match {
          case (Some(t),Some(b)) =>
            t.plain.attributes((t.namespace,"data-collapse-title")) = "true"
            b.plain.attributes((t.namespace, "data-collapse-body")) = "true"
            plain.attributes((this.namespace, "data-collapsible")) = if (expanded) "true" else "false"
            print("")
          case _ =>
        }
      }
    }
    case class ProofTitle(orig:HTMLNode) extends SHTMLNode(orig) {
      def copy = ProofTitle(orig.copy)
      override def onAdd: Unit = {
        super.onAdd
        findAncestor{case h:Hidable => h}.foreach{h =>
          val opt = h.children.find(_.get()()().contains(this))
          h.title = opt
        }
      }
    }
    case class ProofBody(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = ProofBody(orig.copy)
      override def onAdd: Unit = {
        super.onAdd
        findAncestor { case h: Hidable => h }.foreach(h => h.body = Some(this))
      }
    }
    case class FillInSol(orig:HTMLNode) extends SHTMLNode(orig) {
      def copy = FillInSol(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-fillinsol")) = "true"
      }
    }
    case class Problem(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = Problem(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem")) = "true"
      }
    }
    case class MCB(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = MCB(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-mcb")) = "true"
      }
    }
    case class MC(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = MC(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-mc")) =
          plain.attributes((HTMLParser.ns_shtml, "mcc"))
      }
    }
    case class MCSolution(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = MCSolution(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-mc-solution")) = "true"
      }
    }
    case class Solution(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = Solution(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-solution")) = "true"
      }
    }

    val presentationRules: List[HTMLRule] = List(
      new SHTMLRule() {
        override def apply(s: ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = {
          if (n.label == "img") {
            n.plain.attributes.get((HTMLParser.ns_html,"src")) match {
              case Some(s) if s.startsWith("shtml/") =>
                n.plain.attributes((HTMLParser.ns_html,"src")) =
                  "/:" + pathPrefix + "/img?" + s.drop(6)
              case _ =>
            }
          }
          None
        }
      },
      PresentationRule("solution", (_, _, n) => Some(Solution(n))),
      PresentationRule("multiple-choice-block", (_, _, n) => Some(MCB(n))),
      PresentationRule("mcc", (_, _, n) => Some(MC(n))),
      PresentationRule("mcc-solution", (_, _, n) => Some(MCSolution(n))),
      PresentationRule("problem", (_, _, n) => Some(Problem(n))),
      PresentationRule("fillinsol",(_,_,n) => Some(FillInSol(n))),
      PresentationRule("proof",(_,_,n) => Some(Hidable(n))),
      PresentationRule("prooftitle", (_, _, n) => Some(ProofTitle(n))),
      PresentationRule("proofbody", (_, _, n) => Some(ProofBody(n))),
      PresentationRule("subproof", (_, _, n) => Some(Hidable(n))),
      PresentationRule("framenumber",(_,_,n) => Some(FrameNumber(n))),
      PresentationRule("paragraph", (_,_,node) => Some(Statement("paragraph",node))),
      PresentationRule("example", (_, _, node) => Some(Statement("example", node))),
      PresentationRule("definition", (_, _, node) => Some(Statement("definition", node))),
      PresentationRule("assertion", (_, _, node) => Some(Statement("assertion", node))),
      PresentationRule("frame", (_, _, node) => Some(Frame(node))),
      PresentationRule("statementtitle", (_, _, node) => Some(STitle(node))),
      PresentationRule("sectiontitle", (_, _, node) => Some(SecTitle(node))),
      PresentationRule("section", (_, _, node) => Some(Section(node))),
      PresentationRule("skipsection", (_, _, node) => Some(SkipSection(node))),
      PresentationRule("sectionlevel",(s,_,n) => Some(SectionLevel(s.toInt,n))),
      PresentationRule("term", (_, _, node) => {
        overlay(node);
        Some(new SHTMLNode(node) {
          override def onAdd: Unit = overlay(this)

          override def copy: HTMLNode = node.copy
        })
      }),
      PresentationRule("definiendum", (_, _, node) => {
        overlay(node);
        Some(new SHTMLNode(node) {
          override def onAdd: Unit = overlay(this)

          override def copy: HTMLNode = node.copy
        })
      }),
      PresentationRule("inputref", (v, _, node) => {
        val dp = Path.parseD((if (v.endsWith(".tex")) {
          v.dropRight(4)
        } else v) + ".omdoc", NamespaceMap.empty)
        controller.getO(dp) match {
          case Some(d: Document) =>
            controller.backend.resolveLogical(d.path.uri) match {
              case Some((a, ls)) =>
                val path = ls.init.mkString("/") + "/" + ls.last.dropRight(5) + "xhtml"
                node.plain.attributes((node.namespace, "style")) = ""
                node.plain.attributes.remove((HTMLParser.ns_shtml, "visible"))
                node.plain.classes = List("inputref")
                node.plain.attributes((node.namespace, "data-inputref-url")) = "/:" + pathPrefix + "/document?archive=" + a.id + "&filepath=" + path + "&bindings=" + bindings.toNum(controller)
                this.getTitle(d).foreach(n => node.add(n))
              case _ =>
            }
            bindings.merge(dp)(controller)//LateBinding.get(dp)(controller))
          case _ =>
        }
        None
      })
    )

    val state = new ParsingState(this.controller,presentationRules)
    val ret = HTMLParser(str)(state)
    var toremoves: List[HTMLNode] = Nil
    ret.iterate{n =>
      n.plain.attributes.get((HTMLParser.ns_shtml,"id")) match {
        case Some(id) => n.plain.attributes((n.namespace,"id")) = id
        case _ =>
      }
      n.plain.attributes.get((HTMLParser.ns_shtml,"ifinputref")) match {
        case Some("true") if !ifinputref =>
          toremoves ::= n
        case Some("false") if ifinputref =>
          toremoves ::= n
        case _ =>
      }
      n.plain._sourceref = None
      n.plain.attributes.keys.foreach {
        case (HTMLParser.ns_shtml, "visible") =>
          toremoves ::= n
        case (HTMLParser.ns_shtml, p) =>
          n.plain.attributes.remove((HTMLParser.ns_shtml, p))
        case _ =>
      }
    }
    toremoves.foreach(_.delete)
    ret
  }

  lazy val presentationRulesOld : List[PartialFunction[HTMLNode,Unit]] = List(/*
      {case spf : HTMLProofFrame =>
        spf.children.collectFirst {case t:HTMLSProoftitle => t} match {
          case Some(ttl) =>
            spf.children.collectFirst {case t : HTMLSProofbody => t} match {
              case Some(bd) =>
                ttl.attributes((ttl.namespace,"data-collapse-title")) = "true"
                bd.attributes((bd.namespace,"data-collapse-body")) = "true"
                spf.attributes((spf.namespace,"data-collapsible")) = if (spf.expanded) "true" else "false"
            }
          case _ =>
        }
      },
      {case iref : HTMLInputref =>
        val dp = Path.parseD(iref.resource + ".omdoc",NamespaceMap.empty)
        controller.getO(dp) match {
          case Some(d:Document) =>
            controller.backend.resolveLogical(d.path.uri) match {
              case Some((a,ls)) =>
                val path = ls.init.mkString("/") + "/" + ls.last.dropRight(5) + "xhtml"
                val ipref = <div class="inputref" data-inputref-url={"/:" + server.pathPrefix + "/document?archive=" + a.id + "&filepath=" + path}>
                  {d.metadata.get(STeX.meta_doctitle).headOption.map(_.value match {
                    case OMFOREIGN(node) => node
                    case _ => ""
                  }).getOrElse("")}
                </div>
                iref.parent.foreach(_.addAfter(
                  if (iref.ancestors.exists(_.isInstanceOf[HTMLFrame])) {
                    <div class="inputref-container">
                      {ipref}
                    </div>
                  } else ipref
                  ,iref))
              case _ =>
            }
          case _ =>
        }
      },
      {case thm: HTMLTheory =>
        sidebar(thm, <b style="font-size: larger">Theory: {thm.name.toString}</b> :: Nil)
      },
      {case s: HTMLSymbol =>
        controller.getO(s.path) match {
          case Some(c : Constant) =>
            sidebar(s,{<span style="display:inline">Constant {makeButton(
              "/:" + server.pathPrefix + "/fragment?" + c.path + "&language=" + getLanguage(s),
              "/:" + server.pathPrefix + "/declaration?" + c.path + "&language=" + getLanguage(s)
              ,scala.xml.Text(c.name.toString)
            )}<code>(\{s.macroname})</code></span>} :: Nil)
          case _ =>
        }
      },
      {
        case t : HTMLTopLevelTerm if t.orig.isInstanceOf[HTMLDefiniendum] =>
          overlay(t, ""/*/:" + server.pathPrefix + "/declheader?" + t.orig.asInstanceOf[HTMLDefiniendum].head.toString*/,
            "/:" + server.pathPrefix + "/declaration?" + t.orig.asInstanceOf[HTMLDefiniendum].head.toString  + "&language=" + getLanguage(t))
      },
      {
        case t : HTMLDefiniendum =>
          overlay(t, ""/*"/:" + server.pathPrefix + "/declheader?" + t.head.toString*/,
            "/:" + server.pathPrefix + "/declaration?" + t.head.toString  + "&language=" + getLanguage(t))
      },
      {case t: HasHead if t.isVisible && !t.isInstanceOf[HTMLDefiniendum] =>
        if (t.resource.startsWith("var://") || t.resource.startsWith("varseq://")) {
          // TODO
        } else {
          overlay(t, "/:" + server.pathPrefix + "/fragment?" + t.head.toString + "&language=" + getLanguage(t),
            "/:" + server.pathPrefix + "/declaration?" + t.head.toString  + "&language=" + getLanguage(t))
        }
      },
      {case t : HTMLTopLevelTerm if !t.orig.isInstanceOf[HTMLDefiniendum] =>
        t.orig match {
          case h : HasHead if t.isVisible =>
            if (t.resource.startsWith("var://") || t.resource.startsWith("varseq://")) {
              // TODO
            } else {
              overlay(t, "/:" + server.pathPrefix + "/fragment?" + h.head.toString + "&language=" + getLanguage(t),
                "/:" + server.pathPrefix + "/declaration?" + h.head.toString  + "&language=" + getLanguage(t))
            }
          case _ =>
        }
        /*t.constant.foreach {c =>
          DocumentExtension.sidebar(t,{<span style="display:inline">Term {DocumentExtension.makeButton(
            "/:" + server.stexserver.pathPrefix + "/fragment?" + c.path + "&language=" + DocumentExtension.getLanguage(t),
            "/:" + server.stexserver.pathPrefix + "/declaration?" + c.path + "&language=" + DocumentExtension.getLanguage(t)
            ,server.stexserver.xhtmlPresenter.asXML(c.df.get,Some(c.path $ DefComponent)),false
          )}</span>} :: Nil)
        }*/
      } */
  )
}

trait DocumentExtension extends STeXExtension {
  def documentRules : List[PartialFunction[HTMLNode,Unit]]
}

object DocumentExtension extends STeXExtension {

  override def serverReturn(request: ServerRequest): Option[ServerResponse] = None /*  */
/*

  override def doHeader(head: HTMLNode,body: HTMLNode): Unit = {
  }


 */
}

sealed trait BindingStep {
  def term : Term
}
class LateBinding {
  private var secs: List[SectionStepLike] = Nil
  var topsec = -1
  private var steps: List[BindingStep] = Nil
  private[Extensions] var slides : Int = 0
  private var nextstatement = 0
  def currlvl = if (topsec == -1) -1 else {
    topsec + secs.length
  }
  def secnum = seccounts.tail.headOption match {
    case None => 0
    case Some(i) => i
  }
  private var seccounts : List[Int] = List(0)
  def statement = nextstatement
  def isEmpty = steps.isEmpty && secs.isEmpty

  def add(step: BindingStep): Unit = step match {
    case s:SectionStep =>
      secs ::= s
      seccounts = 0 :: (seccounts.head + 1) :: seccounts.tail
      (topsec,s.lvl) match {
        case (-1,-1) => topsec = 0
        case (-1,i) => topsec = i
        case _ =>
      }
      if (currlvl < 4) nextstatement = 0
    case s:BlindSectionStep =>
      secs ::= s
      seccounts = 0 :: seccounts
      (topsec, s.lvl) match {
        case (-1, -1) => topsec = 0
        case (-1, i) => topsec = i
        case _ =>
      }
      //if (secnum < 4) nextstatement = 0
    case SlideStep => slides += 1
    case StatementStep =>
      nextstatement += 1
      addI(step)
    case _ =>
      addI(step)
  }
  private def addI(step: BindingStep): Unit = secs.headOption match {
    //case Some(s: ImportStep) => s.get(server).steps.reverse.foreach(add)
    //case Some(s: SectionStep) => secs ::= s
    case Some(sec) => sec.steps ::= step
    case _ => steps ::= step
  }

  def close = if (secs.isEmpty) {
    print("")
    ???
  } else {
    val s = secs.head
    if (currlvl < 4) s.steps = Nil
    secs = secs.tail
    seccounts = seccounts.tail
    secs match {
      case h :: _ => h.steps ::= s
      case _ => steps ::= s
    }
  }

  override def toString: String = steps.reverse.map(_.toString).mkString + secs.reverse.map{s =>
    "(" + s.steps.reverse.map(_.toString).mkString
  }.mkString

  def term = OMA(OMS(LateBinding.sym),NatLiterals(slides) :: steps.reverse.map(_.term))

  def toNum(implicit controller:Controller) : String = {
    if (isEmpty) return topsec.toString + ";" + slides.toString + ";"
    implicit val sb : StepBuilder = new StepBuilder
    sb.topsect = topsec
    steps.reverse.map(toNumI).mkString
    secs.reverse.foreach {s =>
      sb.sb += '1'
      s.steps.reverse.foreach(toNumI)
    }
    sb.mkString
  }
  private class StepBuilder {
    val sb : StringBuilder = new StringBuilder
    var topsect = -1
    def +=(c : Char) = sb += c
    def mkString = topsect.toString + ";" + slides.toString + ";" + {
      LateBinding.encode(sb.mkString)
    }
  }
  private def toNumI(s : BindingStep)(implicit sb : StepBuilder,controller:Controller) : Unit = s match {
    case s : SectionStep =>
      sb += '1'
      s.steps.reverse.foreach(toNumI)
      sb += '0'
    case s: BlindSectionStep =>
      sb += '3'
      s.steps.reverse.foreach(toNumI)
      sb += '0'
    case StatementStep => sb += '2'
    case ImportStep(d) => // TODO only if necessary
      LateBinding.get(d).steps.reverse.foreach(toNumI)
  }
  def merge(dp:DPath)(implicit controller:Controller) = {
    val lb = LateBinding.get(dp)//(lb : LateBinding) = {
    lb.steps.reverse.foreach(mergeI)
    lb.secs.reverse.foreach{s =>
      add(s)
      s.steps.reverse.foreach(mergeI)
    }
    slides += lb.slides
    if (topsec == -1) topsec = lb.topsec
  }
  def mergeI(s : BindingStep)(implicit controller:Controller) : Unit = s match {
    case ss : SectionStep =>
      add(new SectionStep(ss.lvl))
      ss.steps.reverse.foreach(mergeI)
      close
    case ss : BlindSectionStep =>
      add(new BlindSectionStep(ss.lvl))
      ss.steps.reverse.foreach(mergeI)
      close
    case ImportStep(path) =>
      merge(path)
    case i => add(i)
  }

}
object LateBinding {
  val sym = SHTML.mmtmeta_path ? "bindings"

  private val str = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  private def radixify(i : BigInt): String = {
    if (i < 62) str(i.toInt).toString else {
      val r = (i % 62).toInt
      val d = i / 62
      radixify(d) + str(r).toString
    }
  }
  private def deradixify(s : String): BigInt = {
    if (s.isEmpty) BigInt(0) else if (s.length == 1) {
      BigInt(str.indexOf(s.head))
    } else {
      (deradixify(s.init) * 62) + str.indexOf(s.last)
    }
  }
  def encode(s : String) = radixify(BigInt(s,4))
  def decode(s : String) = deradixify(s).toString(4)

  def get(dp : DPath)(implicit controller:Controller) = {
    controller.getO(dp) match {
      case Some(d: Document) =>
        d.metadata.getValues(LateBinding.sym) match {
          case List(tm: Term) =>
            LateBinding.fromTerm(tm).getOrElse(new LateBinding)
          case _ =>
            new LateBinding
        }
      case _ =>
        new LateBinding
    }
  }
  def parse(s : String) : LateBinding = {
    val b = new LateBinding
    s.foreach {
      case '(' =>
        b.add(new SectionStep(-1))
      case ')' => b.close
      case 'F' => b.add(SlideStep)
      case 'S' => b.add(StatementStep)
      case _ => ???
    }
    b
  }

  def fromNum(s: String): LateBinding = {
    implicit val lb: LateBinding = new LateBinding
    s.split(';') match {
      case Array(a,b) =>
        lb.topsec = a.toInt
        lb.slides = b.toInt
      case Array(a, b, c) =>
        lb.topsec = a.toInt
        lb.slides = b.toInt
        val num = decode(c)
        num.foreach(fromNumI)
      case _ =>
    }
    lb
  }
  private def fromNumI(c : Char)(implicit lb : LateBinding) : Unit = c match {
    case '1' => lb.add(new SectionStep(-1))
    case '0' => lb.close
    //case '2' => lb.add(SlideStep)
    case '2' => lb.add(StatementStep)
    case '3' => lb.add(new BlindSectionStep(-1))
  }

  def fromTerm(tm : Term) : Option[LateBinding] = tm match {
    case OMA(OMS(`sym`),NatLiterals(i) :: ls) =>
      implicit val lb : LateBinding = new LateBinding
      lb.slides = i.toInt
      try {
        ls.foreach(fromTermI)
      } catch {
        case Cancel => return None
      }
      Some(lb)
    case _ => None
  }
  private object Cancel extends Throwable
  def fromTermI(tm : Term)(implicit lb: LateBinding) : Unit = tm match {
    case OMA(OMS(SectionStep.sym),ls) =>
      lb.add(new SectionStep(-1))
      ls.foreach(fromTermI)
      lb.close
    case OMA(OMS(SectionStep.blind), ls) =>
      lb.add(new BlindSectionStep(-1))
      ls.foreach(fromTermI)
      lb.close
    case OMS(SlideStep.sym) =>
      lb.add(SlideStep)
    case OMS(StatementStep.sym) =>
      lb.add(StatementStep)
    case OMA(OMS(ImportStep.sym),List(OMMOD(mp))) =>
      lb.add(ImportStep(mp.parent))
    case _ => throw Cancel
  }
}
trait SectionStepLike extends BindingStep {
  private[Extensions] var steps: List[BindingStep] = Nil

}
class SectionStep(val lvl : Int) extends SectionStepLike {
  def term = OMA(OMS(SectionStep.sym), steps.reverse.map(_.term))

  override def toString: String = "(" + steps.reverse.map(_.toString).mkString + ")"
}
class BlindSectionStep(val lvl : Int) extends SectionStepLike {
  def term = OMA(OMS(SectionStep.blind), steps.reverse.map(_.term))
  override def toString: String = "[" + steps.reverse.map(_.toString).mkString + "]"
}
object SectionStep {
  val sym = SHTML.mmtmeta_path ? "bindings/section"
  val blind = SHTML.mmtmeta_path ? "bindings/blindsection"

}
case object SlideStep extends BindingStep {
  val sym = SHTML.mmtmeta_path ? "bindings/slide"
  val term = OMS(sym)
  override def toString = "F"
}
case object StatementStep extends BindingStep {
  val sym = SHTML.mmtmeta_path ? "bindings/statement"
  val term = OMS(sym)
  override def toString: String = "S"
}

case class ImportStep(path : DPath) extends BindingStep {
  val term = OMA(OMS(ImportStep.sym),List(OMMOD(path ? "import")))
  def get(server: STeXServer) : LateBinding  = ???
}
object ImportStep {
  val sym = SHTML.mmtmeta_path ? "bindings/import"
}

// Definition / Example / ...
// Slide