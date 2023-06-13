package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.{Archive, ArchiveLike, RedirectableDimension}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.Presenter
import info.kwarc.mmt.api.utils.time.Time
import info.kwarc.mmt.api.utils.{File, FilePath, JSON, JSONObject, JSONString, MMTSystem, XMLEscaping}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.Extensions.{Definienda, ExampleRelational, ExportExtension, NotationExtractor, OMDocHTML, OMDocSHTMLRules, SHTMLBrowser, SHTMLContentManagement, SHTMLDocumentServer, SymdocRelational}
import info.kwarc.mmt.stex.lsp.{MathHubServer, RemoteLSP, STeXLSPServer, SearchResultServer}
import info.kwarc.mmt.stex.rules.MathStructureFeature
import info.kwarc.mmt.stex.vollki.{FullsTeXGraph, JupyterBookArchive, VirtualArchive, VollKi}
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState
import info.kwarc.mmt.stex.xhtml._

import scala.collection.mutable
import scala.runtime.NonLocalReturnControl
import scala.util.Try

case class ErrorReturn(s : String) extends Throwable {
  def toResponse = ServerResponse(s,"plain",ServerResponse.statusCodeNotFound)
}


class STeXServer extends ServerExtension("sTeX") with OMDocSHTMLRules with SHTMLDocumentServer with SHTMLBrowser with SHTMLContentManagement with OMDocHTML with ExportExtension {
  def ctrl = controller
  def getArchives = controller.backend.getStores.collect {
    case a : Archive if a.properties.get("format").contains("stex") => a
    case al:ArchiveLike => al
  }
  def getArchive(id:String) = controller.backend.getStores.collectFirst {
    case al:ArchiveLike if al.id == id => al
  }

  override def start(args: List[String]): Unit = {
    super.start(args)
    controller.extman.addExtension(NotationExtractor)
    controller.extman.addExtension(SymdocRelational)
    controller.extman.addExtension(ExampleRelational)
    controller.extman.addExtension(Definienda)
    controller.extman.addExtension(new MathStructureFeature)
    /*addExtension(DocumentExtension)
    addExtension(FragmentExtension)
    addExtension(BrowserExtension)*/

    val index = RusTeX.mh.up / "meta" / "inf" / "courses.json"
    if (index.exists()) Try(JSON.parse(File.read(index))).toOption match {
      case Some(o:JSONObject) =>
        o.foreach {
          case (JSONString(id),jo:JSONObject) =>
            val map = mutable.HashMap.empty[String, String]
            map("id") = id
            jo.foreach {
              case (JSONString(key),JSONString(value)) =>
                map(key) = value
              case _ =>
            }
            map.get("type") match {
              case Some("jupyterbook") =>
                val store = new JupyterBookArchive(controller, map)
                controller.backend.addStore(store)
                //val (t,_) = Time.measure {
                  store.importAll
                //}
                //println("Takes: " + t)
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }


    controller.backend.getArchives.filter{a =>
      a.properties.get("format").contains("stex")
    }.foreach(_.readRelational(Nil, controller, "rel"))

    controller.extman.addExtension(texPresenter)
    controller.extman.addExtension(xhtmlPresenter)
    controller.extman.addExtension(presenter)
    controller.extman.addExtension(new VollKi(this))
  }

  def iterateLibs(a : Archive)(fn : File => Unit): Unit = {
    val lib = a.root / "lib"
    if ((a.root / "lib").exists()) fn(lib)
    def iter(f : File): Unit = if (f != RusTeX.mh && f != f.up) {
      val maybelib = f.up / "meta-inf" / "lib"
      if (maybelib.exists()) fn(maybelib)
      iter(f.up)
    }
    iter(a.root.up)
  }

  lazy val htmlpres = new MMTsTeXPresenter(texPresenter,xhtmlPresenter)

  override def apply(request: ServerRequest): ServerResponse = try {
    request.path.lastOption match {
      case Some("document" | "pdf" | "fullhtml" | "documentTop" | "fulldocument" | "fragment" | "symbol" | "declaration" | "variable" | "css" | "sections" | "definienda") =>
        documentRequest(request)
      case Some("omdoc" | "omdocfrag" | "omdocuri") =>
        omdocRequest(request)
      case Some(":sTeX") if request.query == "" =>
        browserRequest(request)
      case Some("browser") =>
        browserRequest(request)
      case Some("img") =>
        val fp = request.query.split("/").init.mkString("/")
        controller.backend.getArchive(fp) match {
          case Some(a) =>
            val f = a / RedirectableDimension(".img") / (request.query.split("/").last + ".png")
            if (f.exists()) {
              ServerResponse.FileResponse(f)
            }
            else ServerResponse("Image file " + request.query + ".png not found", "text/plain")
          case _ =>
            ServerResponse("Image file " + request.query + ".png not found", "text/plain")
        }
      case Some("getupdates"|"allarchives"|"allarchfiles"|"archfile"|"search") =>
        controller.extman.get(classOf[RemoteLSP]).headOption match {
          case Some(remote) =>
            remote.serverReturn(request)
          case _ =>
            ServerResponse("Unknown request: \"" + request.path.lastOption + "\"\n" + request.query + "\n" + request.parsedQuery.pairs, "text/plain")
        }
      case Some("searchresult"|"searchresultI") =>
        controller.extman.get(classOf[SearchResultServer]).headOption match {
          case Some(remote) =>
            remote.serverReturn(request)
          case _ =>
            ServerResponse("Unknown request: \"" + request.path.lastOption + "\"\n" + request.query + "\n" + request.parsedQuery.pairs, "text/plain")
        }
      case _ =>
        ServerResponse("Unknown request: \"" + request.path.lastOption + "\"\n" + request.query + "\n" + request.parsedQuery.pairs, "text/plain")
      /*case Some("declaration") =>
        request.query match {
          case "" =>
            ???
          case s =>
            doDeclaration(s)
        }
      case Some("expression") =>
        val xml = request.body.params.get("openmath") match {
          case Some(s) =>
            XMLEscaping.unapply(s)
          case None =>
            ???
        }
        val compO = request.body.params.get("component") match {
          case Some("None") => None
          case Some(s) => Some(Path.parseC(XMLEscaping.unapply(s),NamespaceMap.empty))
          case _ => None
        }*/
    }
    //ServerResponse(ret.toString, "application/xhtml+xml")
  } catch {
    case ret:ErrorReturn => ret.toResponse
    case t : NonLocalReturnControl[Any@unchecked] =>
      throw t
    case t : Throwable =>
      throw t
  }

  def emptydoc = {
    val state = new ParsingState(controller,Nil)
    val doc = HTMLParser.apply(MMTSystem.getResourceAsString("mmt-web/stex/emptydoc.xhtml"))(state)
    (doc,doc.get("div")()("rustex-body").head)
  }


}
