package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.{GlobalName, NamespaceMap, Path}
import info.kwarc.mmt.api.archives.{Archive, source}
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils.{File, MMTSystem}
import info.kwarc.mmt.api.web.WebQuery
import info.kwarc.mmt.stex.{RusTeX, STeXServer}
import info.kwarc.mmt.stex.xhtml.{HTMLNode, HTMLParser, SHTMLNode, SHTMLRule}
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState

import java.io.{FileInputStream, FileOutputStream}
import scala.collection.mutable

trait ExportExtension { self : STeXServer =>

  private val default_remote = "https://stexmmt.mathhub.info/:sTeX/"

  private class ExportState(val archive:Archive,val dir:File) {
    val dones_frag = mutable.Set.empty[GlobalName]
    val todo_frag = mutable.Set.empty[GlobalName]
    val dones_var = mutable.Set.empty[GlobalName]
    val todo_var = mutable.Set.empty[GlobalName]
  }

  def export(archive:Archive,document: File,to:File) = {
    /*if (to.exists()) to.descendants.foreach {
      case f if f.isDirectory => f.deleteDir
      case f if f.exists() => f.delete()
    }*/
    (to / "aux" ).mkdirs()
    (to / "docs").mkdir()
    (to / "frags").mkdir()
    (to / "img").mkdir()

    to.descendants.foreach(_.delete())
    var index = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
    index = index.replace("CONTENT_URL_PLACEHOLDER", document.setExtension("doc.html").name)
    index = index.replace("BASE_URL_PLACEHOLDER", "")
    index = index.replace("LANGUAGE_PLACEHOLDER", "")
    index = index.replace("TOUR_ID_PLACEHOLDER", "")
    index = index.replace("NO_FRILLS_PLACEHOLDER", "FALSE")
    index = index.replace("SHOW_FILE_BROWSER_PLACEHOLDER", "false")
    index = index.replace("CONTENT_CSS_PLACEHOLDER", "aux/archive.css")

    File.write(to / "aux" / "archive.css",css(archive.id))

    val aux_files = MMTSystem.getResourceList("mmt-web/stex/mmt-viewer").filter(f => File(f).getExtension.isDefined && f != "index.html")
    aux_files.foreach{f =>
      File.write(to / "aux" / f,MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/" + f))
      index = index.replace("/stex/mmt-viewer/" + f,"aux/" + f)
    }
    index = index.replace("/stex/fonts.css",default_remote.dropRight(6) + "stex/fonts.css")

    File.write(to / "index.html",index)
    implicit val state = new ExportState(archive,to)
    doFile(document,to / document.setExtension("doc.html").name,true,None)
    while (state.todo_frag.nonEmpty || state.todo_var.nonEmpty) {
      state.todo_frag.headOption match {
        case Some(head) =>
          doFrag(head)
        case None =>
          val head = state.todo_var.head
          doVar(head)
      }
    }
  }

  val footer = <div style="width:100%;display:inline-block;text-align:end;font-style:oblique;font-weight:bold;"><hr/>
    Created with <a href="https://github.com/slatex/sTeX" style="color:blue;"><div style="display:inline-flex;flex-direction:row;">
       <div style="bottom: -3.22919px;display:inline-block">S</div>
       <div style="margin-left: -3.22919px;display:inline-block">T</div>
       <div style="margin-left: -2.55606px;bottom: -3.22919px;display:inline-block">E</div>
       <div style="margin-left: -1.91666px;display:inline-block">X</div></div></a> by
      <a href="https://kwarc.info" style="color:blue;"><!--KWARC-->
        <img src="https://kwarc.info/public/kwarc_logo.svg" style="height:25px;vertical-align:middle;" title="KWARC"></img></a>
  </div>

  private def getRules(withbindings:Option[LateBinding])(implicit state:ExportState) = {
    val (rules, bindings) = presentationRules(withbindings)
    rules("inputref") = PresentationRule("inputref", (v, _, node) => {
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
              if (a == state.archive) {
                val nfname = (File(path), bindings.toNum(controller)).hashCode().toHexString + ".html"
                val nf = state.dir / "docs" / nfname
                doFile(File(path), nf, false, Some(bindings))
                node.plain.attributes((node.namespace, "data-inputref-url")) = "docs/" + nfname
              } else {
                val prefix = a.properties.get("url-base") match {
                  case Some(p) => if (p.endsWith("/")) p else p + "/"
                  case _ => default_remote
                }
                node.plain.attributes((node.namespace, "data-inputref-url")) = prefix + "document?archive=" + a.id + "&filepath=" + path + "&bindings=" + bindings.toNum(controller)
                bindings.merge(dp)(controller)
              }
              getTitle(d).foreach(n => node.add(n))
            case _ =>
          }
        case _ =>
      }
      None
    })

    rules("img") = new SHTMLRule() {
      override def apply(s: ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = {
        if (n.label == "img") {
          n.plain.attributes.get((HTMLParser.ns_html, "src")) match {
            case Some(s) if s.startsWith("shtml/") =>
              val fn = s.drop(6).split("/").init.mkString("/")
              val f = s.split("/")(1).last + ".png"
              n.plain.attributes((HTMLParser.ns_html, "src")) = "img/" + f
              val target = state.dir / "img" / f
              if (!target.exists()) {
                val orig = RusTeX.mh.up / fn / ".img" / fn
                if (orig.exists()) {
                  target.createNewFile()
                  val out = new FileOutputStream(target.toString)
                  val in = new FileInputStream(orig.toString)
                  out.write(in.readAllBytes())
                  in.close()
                  out.close()
                }
              }
            case _ =>
          }
        }
        None
      }
    }

    val expstate = state
    case class OverlayNode(node : HTMLNode) extends SHTMLNode(node) {
      override def onAdd: Unit = {
        overlay(this) { s =>
          if (expstate.archive.ns.exists(ns => s.startsWith(ns.toString))) {
            val gn = Path.parseS(s)
            val fname = "frags/" + gn.hashCode().toHexString + ".html"
            if (!expstate.dones_frag.contains(gn)) {
              expstate.todo_frag += gn
            }
            (fname, fname)
          } else {
            val prefix = controller.backend.getArchives.collectFirst {
              case a if a.ns.exists(ns => s.startsWith(ns.toString)) =>
                a.properties.get("url-base") match {
                  case Some(p) => if (p.endsWith("/")) p else p + "/"
                  case _ => default_remote
                }
            }.getOrElse(default_remote)
            val lang = getLanguage(this)
            (prefix + "fragment?" + s + "&language=" + lang,
              prefix + "declaration?" + s + "&language=" + lang
            )
          }
        } { s =>
          if (expstate.archive.ns.exists(ns => s.startsWith(ns.toString))) {
            val gn = Path.parseS(s)
            val fname = "frags/" + gn.hashCode().toHexString + ".html"
            if (!expstate.dones_var.contains(gn)) {
              expstate.todo_var += gn
            }
            fname
          } else {
            val prefix = controller.backend.getArchives.collectFirst {
              case a if a.ns.exists(ns => s.startsWith(ns.toString)) =>
                a.properties.get("url-base") match {
                  case Some(p) => if (p.endsWith("/")) p else p + "/"
                  case _ => default_remote
                }
            }.getOrElse(default_remote)
            val lang = getLanguage(this)
            prefix + "variable?" + s + "&language=" + lang
          }
        }
      }
      override def copy: HTMLNode = node.copy
    }

    rules("term") = PresentationRule("term",(_,_,node) => Some(OverlayNode(node)))
    rules("definiendum") = PresentationRule("definiendum", (_, _, node) => Some(OverlayNode(node)))
    rules("guidedtour") = new SHTMLRule() {
      override def apply(s: ParsingState, n: HTMLNode, attrs: List[(String, String)]): Option[SHTMLNode] = {
        if (n.label == "td" && n.plain.classes.contains("vollki-guided-tour")) {
          n.plain.attributes((n.namespace, "style")) = "display:none;"
        } // <td class="vollki-guided-tour">
        None
      }
    }

    (rules,bindings)
  }

  private def doFile(file:File,to:File,top : Boolean = false,withbindings:Option[LateBinding])(implicit state:ExportState): Unit = {
    val params = new DocParams(new WebQuery(Nil)) {
      override lazy val path = None
      override lazy val language = None // TODO
      override lazy val filepath = Some(file.setExtension("xhtml").toString)
      override lazy val bindings = withbindings
      override lazy val archive = Some(state.archive)
    }
    val (rules,bindings) = getRules(withbindings)
    val doc = cleanHTML(HTMLParser(getDocument(params))(new ParsingState(controller,rules.values.toList)),!top,true)
    val ret = if (top) {
      val bd = doc.get("div")()("body").head
      bd.add(footer)
      doc.get("body")()().head
    } else {
      val bd = doc.get("div")()("body").head
      bd.plain.attributes.remove((bd.namespace,"style"))
      bd.plain.attributes.remove((bd.namespace, "id"))
      bd
    }
    File.write(to,ret.toString.trim)
  }

  private def doFrag(gn : GlobalName)(implicit state:ExportState): Unit = {
    state.todo_frag -= gn
    state.dones_frag += gn
    controller.getO(gn) match {
      case Some(c : Constant) =>
        implicit val params = new DocParams(new WebQuery(Nil)) {
          override lazy val path = Some(gn)
          override lazy val language = None // TODO
          override lazy val filepath = None
          override lazy val bindings = None
          override lazy val archive = Some(state.archive)
        }
        val (rules, _) = getRules(None)
        val htm = cleanHTML(HTMLParser(getFragmentDefault(c))(new ParsingState(controller, rules.values.toList)), false, true)

        val (doc, body) = this.emptydoc
        body.plain.attributes((HTMLParser.ns_html, "style")) = "background-color:white"
        stripMargins(doc)
        val border = body.add(<div style="font-size:small"/>)
        border.add(htm)
        val ret = doc.get("body")()().head.toString.trim
        val fname = "frags/" + gn.hashCode().toHexString + ".html"
        File.write(state.dir / fname,ret)
      case _ =>
    }
  }

  private def doVar(gn: GlobalName)(implicit state: ExportState): Unit = {
    state.todo_var -= gn
    state.dones_var += gn
    controller.getO(gn) match {
      case Some(c: Constant) =>
        implicit val params = new DocParams(new WebQuery(Nil)) {
          override lazy val path = Some(gn)
          override lazy val language = None // TODO
          override lazy val filepath = None
          override lazy val bindings = None
          override lazy val archive = Some(state.archive)
        }
        val (rules, _) = getRules(None)
        val htm = cleanHTML(HTMLParser(getVariable)(new ParsingState(controller, rules.values.toList)), false, true)
        val (doc, body) = this.emptydoc
        body.plain.attributes((HTMLParser.ns_html, "style")) = "background-color:white"
        stripMargins(doc)
        val border = body.add(<div style="font-size:small"/>)
        border.add(htm)
        val ret = doc.get("body")()().head.toString.trim
        val fname = "frags/" + gn.hashCode().toHexString + ".html"
        File.write(state.dir / fname, ret)
      case _ =>
    }
  }


}
