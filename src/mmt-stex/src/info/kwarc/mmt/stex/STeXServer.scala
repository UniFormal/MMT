package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.{FilePath, MMTSystem, XMLEscaping}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.Extensions.{BrowserExtension, DocumentExtension, FragmentExtension, OMDocExtension, STeXExtension}
import info.kwarc.mmt.stex.vollki.{FullsTeXGraph, STeXGraph}
import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, ParsingState}
import info.kwarc.mmt.stex.xhtml._

import scala.runtime.NonLocalReturnControl

case class ErrorReturn(s : String) extends Throwable {
  def toResponse = ServerResponse(s,"plain",ServerResponse.statusCodeNotFound)
}


class STeXServer extends ServerExtension("sTeX") {

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

  def extensions = controller.extman.get(classOf[STeXExtension])

  def addExtension(ext : Extension) =
    if (!extensions.contains(ext))
      controller.extman.addExtension(ext)
  def addExtension[A <: Extension](cls : Class[A]) =
    controller.extman.get(cls) match {
      case Nil =>
        val n = cls.getConstructor().newInstance()
        controller.extman.addExtension(n)
      case _ =>
    }

  override def start(args: List[String]): Unit = {
    super.start(args)
    addExtension(OMDocExtension)
    //addExtension(FeaturesExtension)
    addExtension(DocumentExtension)
    addExtension(FragmentExtension)
    addExtension(BrowserExtension)
    //addExtension(EditorExtension)
    //addExtension(DemoExtension)
    //addExtension(classOf[PillarFeature])
    //addExtension(classOf[DefinitionFeature])
    //addExtension(FullsTeXGraph)
    //addExtension(STeXGraph)

    controller.backend.getArchives.filter{a =>
      a.properties.get("format").contains("stex")
    }.foreach(_.readRelational(Nil, controller, "rel"))
  }

  override def apply(request: ServerRequest): ServerResponse = try {
    request.path.lastOption match {
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
      case Some(":sTeX") if request.query == "" =>
        val nr = request.copy(path = List(":sTeX","browser"))
        return apply(nr)
      case _ =>
        extensions.foreach(e => e.serverReturn(request) match {
          case Some(rsp) => return rsp
          case _ =>
        })
    }
    ServerResponse("Unknown request: " + request.path.mkString("/"),"text/plain")
    //ServerResponse(ret.toString, "application/xhtml+xml")
  } catch {
    case ret:ErrorReturn => ret.toResponse
    case t : NonLocalReturnControl[Any] =>
      throw t
    case t : Throwable =>
      throw t
  }


  def doHeader(doc : HTMLNode) = {
    val head = doc.get("head")()().head
    val body = doc.get("body")()().head
    val headstring = MMTSystem.getResourceAsString("/mmt-web/stex/htmlfragments/header.xml")
    doc.addBefore(headstring,head)
    head.delete
    val nhead = doc.get("head")()().head

    extensions.foreach(_.doHeader(nhead,body))
    nhead
  }

  def emptydoc = {
    val rules = extensions.flatMap(_.rules)
    val state = new ParsingState(controller,rules)
    val doc = HTMLParser.apply(MMTSystem.getResourceAsString("mmt-web/stex/htmlfragments/emptydoc.xhtml"))(state)
    doHeader(doc)
    (doc,doc.get("div")()("body").head)
  }

  lazy val xhtmlPresenter = controller.extman.get(classOf[STeXPresenterML]) match {
    case p :: _ => p
    case Nil =>
      val p = new STeXPresenterML
      controller.extman.addExtension(p)
      p
  }

  lazy val texPresenter = controller.extman.get(classOf[STeXPresenterTex]) match {
    case p :: _ => p
    case Nil =>
      val p = new STeXPresenterTex
      controller.extman.addExtension(p)
      p
  }

}
