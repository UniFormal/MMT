package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.Presenter
import info.kwarc.mmt.api.utils.{FilePath, MMTSystem, XMLEscaping}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.Extensions.{ExampleRelational, NotationExtractor, OMDocHTML, OMDocSHTMLRules, SHTMLBrowser, SHTMLContentManagement, SHTMLDocumentServer, SymdocRelational}
import info.kwarc.mmt.stex.rules.MathStructureFeature
import info.kwarc.mmt.stex.vollki.FullsTeXGraph
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState
import info.kwarc.mmt.stex.xhtml._

import scala.runtime.NonLocalReturnControl

case class ErrorReturn(s : String) extends Throwable {
  def toResponse = ServerResponse(s,"plain",ServerResponse.statusCodeNotFound)
}


class STeXServer extends ServerExtension("sTeX") with OMDocSHTMLRules with SHTMLDocumentServer with SHTMLBrowser with SHTMLContentManagement with OMDocHTML {
  def ctrl = controller

  def resolveDocumentQuery(query : String) = query match {
    case s if s.startsWith("group=") =>
      val grp = s.drop(6)
      (grp,None,FilePath(Nil))
    case s if s.startsWith("archive=") =>
      val (id,path) = {
        s.drop(8).split('&') match {
          case Array(s) => (s,FilePath(Nil))
          case Array(s,r) if r.startsWith("filepath=") =>
            (s,FilePath(r.drop(9).split('/').toList))
          case _ =>
            print("")
            ???
        }
      }
      val archive = controller.backend.getArchive(id) match {
        case Some(a) => a
        case _ => throw ErrorReturn("Archive " + id + " does not exist")
      }
      id.split('/') match {
        case Array(grp,id) =>
          (grp,Some(archive),path)
        case Array(id) =>
          ("",Some(archive),path)
      }
  }

  override def start(args: List[String]): Unit = {
    super.start(args)
    controller.extman.addExtension(NotationExtractor)
    controller.extman.addExtension(SymdocRelational)
    controller.extman.addExtension(ExampleRelational)
    controller.extman.addExtension(new MathStructureFeature)
    /*addExtension(DocumentExtension)
    addExtension(FragmentExtension)
    addExtension(BrowserExtension)*/


    controller.backend.getArchives.filter{a =>
      a.properties.get("format").contains("stex")
    }.foreach(_.readRelational(Nil, controller, "rel"))

    controller.extman.addExtension(texPresenter)
    controller.extman.addExtension(xhtmlPresenter)
  }

  lazy val htmlpres = new MMTsTeXPresenter(texPresenter,xhtmlPresenter)

  override def apply(request: ServerRequest): ServerResponse = try {
    request.path.lastOption match {
      case Some("document" | "documentTop" | "fulldocument" | "fragment" | "symbol" | "declaration" | "variable") =>
        documentRequest(request)
      case Some("omdoc" | "omdocfrag" | "omdocuri") =>
        omdocRequest(request)
      case Some(":sTeX") if request.query == "" =>
        browserRequest(request)
      case Some("browser") =>
        browserRequest(request)
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
    (doc,doc.get("div")()("body").head)
  }


}
