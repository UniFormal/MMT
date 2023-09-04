package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.{DPath, DefComponent, GlobalName, MPath, NamespaceMap, Path, StructuralElement, TypeComponent, presentation}
import info.kwarc.mmt.api.archives.{Archive, RedirectableDimension}
import info.kwarc.mmt.api.documents.{DRef, Document}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.{OMA, OMFOREIGN, OMID, OMMOD, OMS, Term}
import info.kwarc.mmt.api.opaque.OpaqueXML
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.{File, FilePath, JSON, JSONArray, JSONObject, JSONString, MMTSystem}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse, WebQuery}
import info.kwarc.mmt.stex.rules.{IntLiterals, StringLiterals}
import info.kwarc.mmt.stex.vollki.{FullsTeXGraph, VirtualArchive}
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState
import info.kwarc.mmt.stex.{ErrorReturn, SHTML, STeXServer}
import info.kwarc.mmt.stex.xhtml.{HTMLNode, HTMLNodeWrapper, HTMLParser, HTMLRule, SHTMLFrame, SHTMLNode, SHTMLRule, SHTMLState}

import scala.collection.mutable
import scala.util.Try
//import info.kwarc.mmt.stex.xhtml.{HTMLArg, HTMLComp, HTMLLanguageComponent, HTMLNode, HTMLParser, HasHead, NotationComponent}
//import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, HTMLText, ParsingState}

import scala.xml.parsing.XhtmlParser
import scala.xml.{Elem, Node}

trait SHTMLDocumentServer { this : STeXServer =>
  protected case class DocParams(q: WebQuery) {
    lazy val path = q.pairs.find(p => p._2.isEmpty && p._1.contains('?') && !p._1.endsWith("="))
      .map(p => Path.parse(p._1))
    lazy val language = q("language")
    lazy val archive = q("archive").flatMap{id => getArchive(id)}
    lazy val filepath = q("filepath")
    lazy val bindings = {
      val bnds = q("bindings").map(LateBinding.fromNum)
      log("Bindings: " + q("bindings") + " --> " + bnds.map(_.toString).getOrElse("None"))
      bnds
    }
  }
  private case class ErrorResponse(message:String) extends Throwable
  protected def documentRequest(request: ServerRequest) : ServerResponse = try {
    log("Here: " + request.query + "\n" + request.parsedQuery.pairs.map(p => p._1 + " == " + p._2).mkString("\n"))
    implicit val params : DocParams = DocParams(request.parsedQuery)
    request.path.lastOption match {
      case Some("fulldocument") =>
        var html = MMTSystem.getResourceAsString("mmt-web/stex/tabs.html")
        html = html.replace("%%HTMLSOURCE%%",this.pathPrefix + "/fullhtml?" + request.query)
        html = html.replace("%%OMDOCSOURCE%%", this.pathPrefix + "/omdoc?" + request.query)
        html = html.replace("%%PDFSOURCE%%", this.pathPrefix + "/pdf?" + request.query)
        ServerResponse(html, "text/html")
      case Some("pdf") =>
        val a = params.archive match {
          case Some(a:Archive) => a
          case _ => return ServerResponse("No archive given", "txt")
        }
        val path = params.filepath.getOrElse {
          return ServerResponse("No path given", "txt")
        }
        val file = a / RedirectableDimension("export") / "pdf" / path.replace(".omdoc",".pdf").replace(".xhtml",".pdf")
        if (File(file).exists()) {
          ServerResponse.FileResponse(file)
        } else ServerResponse("No pdf found", "txt")
      case Some("fullhtml") =>
        var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
        html = html.replace("CONTENT_URL_PLACEHOLDER", "/:" + this.pathPrefix + "/documentTop?" + request.query)
        html = html.replace("BASE_URL_PLACEHOLDER", "")
        html = html.replace("SHOW_FILE_BROWSER_PLACEHOLDER", "false")
        html = html.replace("NO_FRILLS_PLACEHOLDER", "FALSE")
        params.archive match {
          case Some(a:VirtualArchive) =>
            html = html.replace("<link rel=\"stylesheet\" href=\"CONTENT_CSS_PLACEHOLDER\">",a.getHeader(params.filepath.get).map(_.toString).mkString.trim)
          case _ =>
            html = html.replace("CONTENT_CSS_PLACEHOLDER", "/:" + this.pathPrefix + "/css?" + request.parsedQuery("archive").getOrElse(""))
        }
        ServerResponse(html, "text/html")
      case Some("symbol") =>
        var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
        html = html.replace("CONTENT_URL_PLACEHOLDER", "/:" + this.pathPrefix + "/declaration?" + request.query)
        html = html.replace("BASE_URL_PLACEHOLDER", "")
        html = html.replace("NO_FRILLS_PLACEHOLDER", "TRUE")
        html = html.replace("SHOW_FILE_BROWSER_PLACEHOLDER", "false")
        html = html.replace("CONTENT_CSS_PLACEHOLDER", "/:" + this.pathPrefix + "/css?")
        ServerResponse(html, "text/html")
      case Some("lo") =>
        var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
        html = html.replace("CONTENT_URL_PLACEHOLDER", "/:" + this.pathPrefix + "/loraw?" + request.query)
        html = html.replace("BASE_URL_PLACEHOLDER", "")
        html = html.replace("NO_FRILLS_PLACEHOLDER", "TRUE")
        html = html.replace("SHOW_FILE_BROWSER_PLACEHOLDER", "false")
        html = html.replace("CONTENT_CSS_PLACEHOLDER", "/:" + this.pathPrefix + "/css?")
        ServerResponse(html, "text/html")
      case Some("document") =>
        val ret = doDocument
        val bd = ret.get("div")()("rustex-body").head
        bd.plain.attributes.remove((bd.namespace, "style"))
        bd.plain.attributes.remove((bd.namespace, "id"))
        ServerResponse(bd.toString.trim, "text/html")
      case Some("documentTop") =>
        ServerResponse(doDocument.toString.trim, "text/html")
      case Some("fragment") =>
        ServerResponse(doFragment.toString.trim.replace("&amp;","&"), "text/html")
      case Some("declaration") =>
        doDeclaration
      case Some("loraw") =>
        doLo
      case Some("css") =>
        ServerResponse(css(request.query),"text/css")
      case Some("variable") =>
        ServerResponse(doVariable.toString.trim,"text/html")
      case Some("sections") =>
        val ret = doSections
        ServerResponse.JsonResponse(ret)
      case Some("definienda") =>
        val ret = doDefinienda
        ServerResponse.JsonResponse(ret)
      case _ =>
        throw ErrorResponse("Unknown path: " + request.path.mkString)
    }
  } catch {
    case ErrorResponse(s) =>
    ServerResponse(s,"txt")
  }
  def css(a : String): String = {
    getArchive(a) match {
      case None => ""
      case Some(a : Archive) =>
        var retstr = ""
        iterateLibs(a){f =>
          val css = f / "lib.css"
          if (css.exists()) {
            retstr = File.read(css) + retstr
          }
        }
        retstr
      case Some(_) => ""
    }
  }

  def doSections(implicit dp:DocParams) : JSON = {
    (dp.archive, dp.filepath) match {
      case (Some(a),Some(fp)) =>
        val nm = fp.replace(".xhtml", ".omdoc").replace(".tex", ".omdoc").split("/")
        val dp = DPath(nm.foldLeft(a.narrationBase)((u, s) => u / s))
        controller.getO(dp) match {
          case Some(d : Document) =>
            JSONObject(("archive",JSONString(a.id)),("filepath",JSONString(fp.replace(".tex",".xhtml").replace(".omdoc",".xhtml"))),
              ("children",doSectionChildren(d)),("ids",JSONArray(SHTMLContentManagement.getSref(d).map(JSONString):_*))
            )
          case _ => JSONObject()
        }
      case _ => JSONObject()
    }
  }

  private def doSectionChildren(d:Document):JSONArray = JSONArray(
    d.getDeclarationsElaborated.flatMap {
      case d: Document => Some(doSections(d))
      case dr: DRef => Some(doSection(dr.target))
      case _ => None
    }: _*
  )

  private def doSection(d : DPath) : JSON = {
    (controller.backend.resolveLogical(d.uri),controller.getO(d)) match {
      case (Some((a,ls)),Some(d:Document)) =>
        JSONObject(("archive", JSONString(a.id)), ("filepath", JSONString(ls.mkString("/").replace(".tex", ".xhtml").replace(".omdoc", ".xhtml"))),
          ("children", doSectionChildren(d)),("ids", JSONArray(SHTMLContentManagement.getSref(d).map(JSONString):_*))
        )
      case _ => JSONObject()
    }
  }

  private def doSections(d: Document): JSON = JSONObject(
    ("title", JSONString(SHTMLContentManagement.getTitle(d).map(_.toString().replace("&amp;amp;","&amp;")).getOrElse(""))),
    ("ids", JSONArray(SHTMLContentManagement.getSref(d).map(JSONString):_*)),
    ("id",JSONString(d.path.last)),
    ("children", doSectionChildren(d))
  )

  def doDefinienda(implicit dp: DocParams): JSON = {
    (dp.archive, dp.filepath) match {
      case (Some(a), Some(fp)) =>
        val nm = fp.replace(".xhtml", ".omdoc").replace(".tex", ".omdoc").split("/")
        val dp = DPath(nm.foldLeft(a.narrationBase)((u,s) => u / s))
        controller.getO(dp) match {
          case Some(d: Document) =>
            JSONArray(doDefinienda(d).distinct:_*)
          case _ => JSONArray()
        }
      case _ => JSONArray()
    }
  }

  private def doDefinienda(d: Document): List[JSON] = {
    // TODO replace by query
    d.getDeclarationsElaborated.flatMap {
      case d: Document => doDefinienda(d)
      case dr: DRef => controller.getO(dr.target) match {
        case Some(d: Document) => doDefinienda(d)
        case _ => Nil
      }
      case Definienda.Def(_, id, ls) => List(JSONObject(
          ("id", JSONString("sref@" + id)),
          ("symbols", JSONArray(ls.map { p => JSONString(p.toString) }: _*))
        ))
      case _ => Nil
    }
  }

  protected case class PresentationRule(key: String, newobj: (String, HTMLParser.ParsingState, HTMLNode) => Option[SHTMLNode], override val priority: Int = 0) extends SHTMLRule(priority) {
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
    val filecontent = present(getDocument)(dp.bindings)
    val body = filecontent.get("body")()().head
    body
  }

  def getDocument(implicit dp:DocParams): String = {
    (dp.archive,dp.filepath) match {
      case (None,_) =>
        val (doc, bd) = this.emptydoc
        bd.add(<b>{"Archive not found"}</b>)
        doc.toString
      case (Some(a: Archive),None) =>
        val (doc, bd) = this.emptydoc
        bd.add(<b style="text-align: center">{a.id}</b>)
        a.properties.collectFirst {
          case ("description", d) if (a.root / "META-INF" / d).exists() =>
            bd.add(File.read(a.root / "META-INF" / d))
          case ("description", d) if (a.root / "meta-inf" / d).exists() =>
            bd.add(File.read(a.root / "meta-inf" / d))
          case ("teaser", t) =>
            bd.add("<div>" + t + "</div>")
        }
        doc.toString
      case (Some(a:Archive),Some(path)) =>
        val top = a / RedirectableDimension("xhtml")
        val fn = top / path
        if (fn.exists() && fn.isDirectory) {
          if ((fn / "index.xhtml").exists()) {
            File.read(fn / "index.xhtml")
          } else {
            val (doc, bd) = this.emptydoc
            bd.add(<b>{a.id + "/" + path}</b>)
            doc.toString
          }
        } else if (fn.exists()) {
          File.read(fn)
        }
        else {
          val (doc, bd) = this.emptydoc
          bd.add(<b>{"Document does not exist: " + a.id + "/" + path}</b>)
          doc.toString
        }
      case (Some(_),None) =>
        val (doc, bd) = this.emptydoc
        doc.toString
      case (Some(a: VirtualArchive),Some(fp)) =>
        val (doc, bd) = this.emptydoc
        a.getDoc(fp).foreach(bd.add)
        doc.toString
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
  def overlay(elem: HTMLNode)(getLinks : String => (String,String))(getVarLink: String => String): Unit = {
    if (elem.plain.classes.contains("overlay-parent")) return
    val id = elem.hashCode().toString
    elem.plain.attributes((elem.namespace, "id")) = id
    elem.plain.classes ::= "hasoverlay-parent"

    //elem.classes ::= "hasoverlay" //:: elem.classes
    def pickelems(n: HTMLNode, inarg: Boolean = false): List[HTMLNode] = n match {
      case comp if hasAttribute(comp, "comp") || hasAttribute(comp, "varcomp") || hasAttribute(comp, "definiendum") || hasAttribute(comp,"maincomp") =>
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
      if (hasAttribute(e,"comp") || hasAttribute(e,"maincomp")) { (getAttribute(e,"comp").toList ::: getAttribute(e,"maincomp").toList).foreach{v =>
        e.plain.classes ::= "symcomp"
        val (urlshort,urllong) = getLinks(v)
        e.plain.attributes((elem.namespace, "data-overlay-link-hover")) = urlshort
        e.plain.attributes((elem.namespace, "data-overlay-link-click")) = urllong
      }} else if (hasAttribute(e,"varcomp")) {
        elem.plain.attributes.get((HTMLParser.ns_mmt,"variable")) match {
          case Some(str) =>
            val link = getVarLink(str)
            e.plain.attributes((elem.namespace, "data-overlay-link-hover")) = link
            e.plain.attributes((elem.namespace, "data-overlay-link-click")) = link
          case _ =>
        }
        e.plain.classes ::= "varcomp" // TODO
      } else if (hasAttribute(e,"definiendum")) {
        e.plain.classes ::= "definiendum" // TODO
      }
    }
  }

  def doLo(implicit dp:DocParams): ServerResponse = {
    dp.path match {
      case None => throw ErrorResponse("Missing path")
      case Some(path) =>
        controller.getO(path) match {
          case Some(c: Constant) =>
            c.df match {
              case Some(OMA(OMS(_),OMFOREIGN(node) :: _)) =>
                val (_, body) = this.emptydoc
                val ret = present(node.toString)(dp.bindings)
                body.add(ret)
                ServerResponse("<body>" + body.toString.trim.replace("&amp;", "&") + "</body>", "text/html")
              case _ => ServerResponse("Not a learning object", "txt")
            }
          case Some(d) =>
            ServerResponse("Not yet implemented: " + d.getClass.toString, "txt")
          case _ =>
            ServerResponse("Declaration not found", "txt")
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
            Try(getAllFragments).toOption match {
              case Some(htm :: Nil) =>
                body.add(<hr/>)
                body.add(htm)
              case Some(ls) =>
                val id = ls.hashCode().toHexString;
                body.add(<hr/>)
                val nhtml = body.add("<ul class=\"shtml-declaration-tabs\"/>")
                val nls = ls.zipWithIndex.map{case (node,i) =>
                  val li = nhtml.add("<li class=\"shtml-declaration-tab\"/>")
                  if (i == 0)
                    li.add(s"<input type=\'radio\' name=\'tabs\' id=\'$id\' checked=\'checked\'/><label for=\'$id\'>1</label>")
                  else {
                    val nid = id + i.toString
                    li.add(s"<input type=\'radio\' name=\'tabs\' id=\'$nid\'/><label for=\'$nid\'>${i+1}</label>")
                  }
                  val inner = li.add("<div class=\"shtml-declaration-tab-content\"/>")
                  inner.add(node)
                }
              case _ =>
            }
            ServerResponse("<body>" + body.toString.trim.replace("&amp;","&") + "</body>", "text/html")
          case Some(d) =>
            ServerResponse("Not yet implemented: " + d.getClass.toString, "txt")
          case _ =>
            ServerResponse("Declaration not found", "txt")
        }
    }
  }

  def doDeclHeader(c: Constant)(implicit dp:DocParams) = {
    val state = new OMDocState(dp.language.getOrElse("en"))
    doSymbol(c)(state).toString()
  }

  def doVariable(implicit dp: DocParams) = {
    val htm = getVariable
    val (doc, body) = this.emptydoc
    body.plain.attributes((HTMLParser.ns_html, "style")) = "background-color:white"
    stripMargins(doc)
    val border = body.add(<div style="font-size:small"/>)
    border.add(htm)
    doc.get("body")()().head
  }
  def getVariable(implicit dp:DocParams) = {
    dp.path match {
      case None => throw ErrorResponse("Path missing")
      case Some(gn : GlobalName) =>
        controller.getO(gn) match {
          case Some(c : Constant) if c.rl.exists(_.contains("variable")) =>
            val sb = new presentation.StringBuilder
            presenter.apply(c,false)(sb)
            sb.get
          case Some(o) => throw ErrorResponse("Not a variable: " + o.path.toString)
          case _ => throw ErrorResponse("Not found: " + gn.toString)
        }
      case Some(o) => throw ErrorResponse("Not a variable: " + o.toString)
    }
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

  def getAllFragments(implicit dp:DocParams) = {
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
            getAllFragmentsDefault(c).map(f => present(f)(dp.bindings))
          case _ =>
            throw ErrorResponse("No symbol with path " + path + " found")
        }
      case _ =>
        throw ErrorResponse("No symbol with path " + path + " found")
    }
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

  def getAllFragmentsDefault(c : Constant)(implicit dp:DocParams) : List[String] = {
    SHTMLContentManagement.getSymdocs(c.path, dp.language.getOrElse("en"))(controller) match { // TODO language
      case Nil =>
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
        List("<div>" + res + "</div>")
      case a => a.map(_.toString())
    }
  }

  def getFragmentDefault(c : Constant)(implicit dp:DocParams) : String = {
    SHTMLContentManagement.getSymdocs(c.path,dp.language.getOrElse("en"))(controller) match { // TODO language
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
    val doc = body.get("div")()("rustex-body").head
    doc.plain.attributes((HTMLParser.ns_html, "style")) = "margin:0;padding:0.1em 0.5em 0.5em 0.5em;"
  }

  def presentationRules(withbindings:Option[LateBinding]) = {
    val map = mutable.HashMap.empty[String,HTMLRule]
    val bindings = withbindings.getOrElse(new LateBinding)
    case class Statement(s: String, orig: HTMLNode) extends SHTMLNode(orig) {
      override def onAdd = {
        super.onAdd
        val chs = this.children
        val span = this.add(<span style="display:contents"></span>)
        chs.foreach(span.add)
        title.foreach(_.plain.classes ::= "shtml-title-" + s)
      }

      def copy = orig.copy

      var title: Option[STitle] = None

      val inline = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "inline"), "true").contains("true")

      if (!inline) bindings.add(StatementStep)
      val styles = plain.attributes.getOrElse((HTMLParser.ns_shtml, "styles"), "").split(',').map(_.trim).toList.filterNot(_.isEmpty)
      plain.classes :::= ("shtml-" + s) :: styles.reverse.map(st => "shtml-" + s + "-" + st)
      //val old = plain.attributes.getOrElse((plain.namespace, "style"), "")
      //plain.attributes((plain.namespace, "style")) = "counter-set: shtml-statement " + bindings.statement.toString + ";" + old
    }
    case class STitle(orig: HTMLNode) extends SHTMLNode(orig) {
      override def onAdd = {
        super.onAdd
        if (isEmpty) plain.classes ::= "empty"
        findAncestor { case s: Statement => s }.foreach(_.title = Some(this))
      }

      def copy = STitle(orig.copy)
    }
    case class Frame(orig: HTMLNode) extends SHTMLFrame(orig) {
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
    case class SkipSection(orig: HTMLNode) extends SHTMLNode(orig) {
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
        withbindings match {
          case Some(wb) => plain.attributes((plain.namespace, "data-with-bindings")) = bindings.toString
          case None => plain.attributes((plain.namespace, "data-with-bindings")) = "None"
        }
        bindings.currlvl match {
          case 1 =>
            this.plain.classes ::= "shtml-sec-part"
            //plain.attributes((plain.namespace, "style")) = "counter-set: shtml-sec-part " + bindings.secnum.toString + ";" + old
          case 2 =>
            this.plain.classes ::= "shtml-sec-chapter"
            //plain.attributes((plain.namespace, "style")) = "counter-set: shtml-sec-chapter " + bindings.secnum.toString + ";" + old
          case 3 =>
            this.plain.classes ::= "shtml-sec-section"
            //plain.attributes((plain.namespace, "style")) = "counter-set: shtml-sec-section " + bindings.secnum.toString + ";" + old
          case 4 =>
            this.plain.classes ::= "shtml-sec-subsection"
            //plain.attributes((plain.namespace, "style")) = "counter-set: shtml-sec-subsection " + bindings.secnum.toString + ";" + old
          case _ =>
            this.plain.classes ::= "shtml-sec-paragraph"
            //plain.attributes((plain.namespace, "style")) = "counter-set: shtml-sec-paragraph " + bindings.secnum.toString + ";" + old
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
    case class FrameNumber(orig: HTMLNode) extends SHTMLNode(orig) {
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
        if (n.label == "span") {
          n.plain._label = "div"
        }
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
    case class Hidable(orig: HTMLNode) extends SHTMLNode(orig) {
      lazy val hidden = this.plain.attributes.getOrElse((HTMLParser.ns_shtml, "proofhide"), "") == "true"

      def copy = Hidable(orig.copy)

      var title: Option[HTMLNode] = None
      var body: Option[ProofBody] = None

      override def onAdd: Unit = {
        super.onAdd
        (title, body) match {
          case (Some(t), Some(b)) =>
            t.plain.attributes((t.namespace, "data-collapse-title")) = "true"
            b.plain.attributes((b.namespace, "data-collapse-body")) = "true"
            plain.attributes((this.namespace, "data-collapsible")) = if (hidden) "false" else "true"
            print("")
          case _ =>
        }
      }
    }
    case class ProofTitle(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = ProofTitle(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        findAncestor { case h: Hidable => h }.foreach { h =>
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
    case class FillInSol(orig: HTMLNode) extends SHTMLNode(orig) {
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

    case class SCB(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = SCB(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-scb")) = "true"
      }
    }
    case class SC(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = SC(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-sc")) =
          plain.attributes((HTMLParser.ns_shtml, "scc"))
      }
    }
    case class SCSolution(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = SCSolution(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-sc-solution")) = "true"
      }
    }

    case class Solution(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = Solution(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-solution")) = "true"
      }
    }
    case class ProblemHint(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = Solution(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-hint")) = "true"
      }
    }
    case class ProblemNote(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = Solution(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-note")) = "true"
      }
    }
    case class ProblemGNote(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = Solution(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-g-note")) = "true"
      }
    }
    case class ProblemPoints(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = Solution(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-points")) = "true"
      }
    }
    case class ProblemMinutes(orig: HTMLNode) extends SHTMLNode(orig) {
      def copy = Solution(orig.copy)

      override def onAdd: Unit = {
        super.onAdd
        plain.attributes((this.namespace, "data-problem-minutes")) = "true"
      }
    }

    def simple(s: String,f : HTMLNode => SHTMLNode) = map(s) = PresentationRule(s,(_,_,n) => Some(f(n)))
    def tuple(s:String,f:(String,HTMLNode) => SHTMLNode) = map(s) = PresentationRule(s,(ns,_,n) => Some(f(ns,n)))

    simple("solution",n => Solution(n))
    simple("problemhint", n => ProblemHint(n))
    simple("problemnote", n => ProblemNote(n))
    simple("problemgnote", n => ProblemGNote(n))
    simple("problempoints", n => ProblemPoints(n))
    simple("problemminutes", n => ProblemMinutes(n))

    simple("multiple-choice-block",n => MCB(n))
    simple("mcc",n => MC(n))
    simple("mcc-solution",n => MCSolution(n))
    simple("single-choice-block", n => SCB(n))
    simple("scc", n => SC(n))
    simple("scc-solution", n => SCSolution(n))

    simple("fillinsol", n => FillInSol(n))

    simple("problem",n => Problem(n))
    simple("proof",n => Hidable(n))
    simple("subproof", n => Hidable(n))
    simple("prooftitle",n => ProofTitle(n))
    simple("proofbody",n => ProofBody(n))
    simple("frame",n => Frame(n))
    simple("framenumber",n => FrameNumber(n))
    simple("paragraph",n => Statement("paragraph",n))
    simple("example", n => Statement("example", n))
    simple("definition", n => Statement("definition", n))
    simple("assertion", n => Statement("assertion", n))
    simple("statementtitle",n => STitle(n))
    simple("section",n => Section(n))
    simple("sectiontitle",n => SecTitle(n))
    simple("skipsection",n => SkipSection(n))
    simple("currentsectionlevel",node => new SHTMLNode(node) {
      override def copy = node.copy
      lazy val capitalize = node.plain.attributes.get((HTMLParser.ns_shtml,"capitalize")).contains("true")

      override def onAdd: Unit = {
        super.onAdd
        node.add(bindings.currlvl match {
          case 0 if capitalize => "Document"
          case 0 => "document"
          case 1 if capitalize => "Part"
          case 1 => "part"
          case 2 if capitalize => "Chapter"
          case 2 => "chapter"
          case 3 if capitalize => "Section"
          case 3 => "section"
          case 4 if capitalize => "Subsection"
          case 4 => "subsection"
          case 5 if capitalize => "Subsubsection"
          case 5 => "subsubsection"
          case _ if capitalize => "Paragraph"
          case _ => "paragraph"
        })
      }
    })
    simple("term",node => new SHTMLNode(node) {
      override def onAdd: Unit = {
        val lang = getLanguage(this)
        overlay(this) { s =>
          ("/:" + pathPrefix + "/fragment?" + s + "&language=" + lang,
            "/:" + pathPrefix + "/declaration?" + s + "&language=" + lang
          )
        }{s => "/:" + pathPrefix + "/variable?" + s + "&language=" + lang}
      }
      override def copy: HTMLNode = node.copy
    })
    simple("definiendum",node => new SHTMLNode(node) {
      override def onAdd: Unit = {
        val lang = getLanguage(this)
        overlay(this) { s =>
          ("/:" + pathPrefix + "/fragment?" + s + "&language=" + lang,
            "/:" + pathPrefix + "/declaration?" + s + "&language=" + lang
          )
        } { s => "/:" + pathPrefix + "/variable?" + s + "&language=" + lang }
      }
      override def copy: HTMLNode = node.copy
    })

    tuple("sectionlevel",(s,n) => SectionLevel(s.toInt,n))


    map("img") = new SHTMLRule() {
      override def apply(s: ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = {
        if (n.label == "img") {
          n.plain.attributes.get((HTMLParser.ns_html, "src")) match {
            case Some(s) if s.startsWith("shtml/") =>
              n.plain.attributes((HTMLParser.ns_html, "src")) =
                "/:" + pathPrefix + "/img?" + s.drop(6)
            case _ =>
          }
        }
        None
      }
    }
    map("linktarget") = new SHTMLRule() {
      override def apply(s: ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = {
        if (n.label == "a") {
          n.plain.attributes.get((HTMLParser.ns_html, "id")) match {
            case Some(s) if s.startsWith("sref@") =>
              n.plain.attributes((HTMLParser.ns_html, "id")) = s.hashCode.toHexString
              n.plain.attributes((HTMLParser.ns_html, "name")) = s.hashCode.toHexString
            case _ =>
          }
        }
        None
      }
    }
    map("sref") = PresentationRule("sref",(s,_,node) => Some(new SHTMLNode(node) {
      override def copy: HTMLNode = this.plain.copy
      val id = ("sref@" + s).hashCode.toHexString
      val in = this.plain.attributes.get((HTMLParser.ns_shtml,"srefin")).flatMap{s =>
        s.split("::").toList match {
          case List(a,b) =>
            getArchive(a) match {
              case Some(a) => Some((a,File(b.replace(".tex",".xhtml"))))
              case _ => None
            }
          case _ => None
        }
      }//(s => File(s.replace(".sref",".tex")))

      override def onAdd: Unit = {
        super.onAdd
        in match {
          case Some((a,f)) =>
            val ch = this.plain.children //TODO needs fixing!
            val na = this.add(<a href={scala.xml.Unparsed("/:" + pathPrefix + "/fullhtml?archive=" + a.id + "&filepath=" + f + "#" + id)} style="color:blue;cursor:pointer"></a>)
            ch.foreach(na.add)
          case _ =>
        }
      }
    }))
    map("inputref") = PresentationRule("inputref",(v,_,node) => {
      val dp = Path.parseD((if (v.endsWith(".tex")) {
        v.dropRight(4)
      } else v) + ".omdoc", NamespaceMap.empty)
      controller.getO(dp) match {
        case Some(d: Document) =>
          controller.backend.resolveLogical(d.path.uri) match {
            case Some((a, ls)) =>
              val ns = ls.init.mkString("/")
              val name = ls.last.dropRight(5) + "xhtml"
              val path = if (ns.isEmpty) name else ns + "/" + name
              node.plain.attributes((node.namespace, "style")) = ""
              node.plain.attributes.remove((HTMLParser.ns_shtml, "visible"))
              node.plain.classes = List("inputref")
              node.plain.attributes((node.namespace, "data-inputref-url")) = "/:" + pathPrefix + "/document?archive=" + a.id + "&filepath=" + path + "&bindings=" + bindings.toNum(controller)
              SHTMLContentManagement.getTitle(d).foreach(n => node.add(n))
            case _ =>
          }
          bindings.merge(dp)(controller) //LateBinding.get(dp)(controller))
        case _ =>
      }
      None
    })

    (map,bindings)
  }

  def present(str : String,remove:Boolean=true)(implicit withbindings : Option[LateBinding]) = {
    val ifinputref = withbindings.isDefined
    val rules = presentationRules(withbindings)._1.values.toList
    val state = new ParsingState(this.controller,rules)
    val ret = HTMLParser(str)(state)
    cleanHTML(ret,ifinputref,remove)
  }

  def cleanHTML(node : HTMLNode,ifinputref:Boolean,remove:Boolean) = {
    var toremoves: List[HTMLNode] = Nil
    node.iterate { n =>
      n.plain.attributes.get((HTMLParser.ns_shtml, "id")) match {
        case Some(id) => n.plain.attributes((n.namespace, "id")) = id
        case _ =>
      }
      n.plain.attributes.get((HTMLParser.ns_shtml, "ifinputref")) match {
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
        case (HTMLParser.ns_shtml, p) if remove =>
          n.plain.attributes.remove((HTMLParser.ns_shtml, p))
        case _ =>
      }
    }
    toremoves.foreach(_.delete)
    node
  }
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

  def term = OMA(OMS(LateBinding.sym),IntLiterals(slides) :: steps.reverse.map(_.term))

  def toNum(implicit controller:Controller) : String = {
    if (isEmpty) return topsec.toString + "_" + slides.toString + "_"
    implicit val sb : StepBuilder = new StepBuilder
    steps.reverse.map(toNumI).mkString
    secs.reverse.foreach {s =>
      sb.sb += '1'
      s.steps.reverse.foreach(toNumI)
    }
    sb.mkString
  }
  val sup = this
  private class StepBuilder {
    val sb : StringBuilder = new StringBuilder
    var slides : Int = sup.slides
    def +=(c : Char) = sb += c
    def mkString = topsec.toString + "_" + slides.toString + "_" + {
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
      val ib = LateBinding.get(d)
      ib.steps.reverse.foreach(toNumI)
      sb.slides += ib.slides
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
      case Some(d: Document) => getFromDoc(d)
      case _ =>
        new LateBinding
    }
  }
  def getFromDoc(d:Document) = d.metadata.getValues(LateBinding.sym) match {
    case List(tm: Term) =>
      LateBinding.fromTerm(tm).getOrElse(new LateBinding)
    case _ =>
      new LateBinding
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
    s.split('_') match {
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
    case OMA(OMS(`sym`),IntLiterals(i) :: ls) =>
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