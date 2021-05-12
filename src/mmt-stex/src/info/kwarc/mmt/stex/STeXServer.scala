package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.RedirectableDimension
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.notations.{Delim, Marker, SimpArg, Var}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.refactoring.AcrossLibraryTranslator
import info.kwarc.mmt.api.utils.{FilePath, MMTSystem, XMLEscaping}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.odk.Sage.{Sage, SageSystem}
import info.kwarc.mmt.stex.Extensions.DocumentExtension.controller
import info.kwarc.mmt.stex.Extensions.{BrowserExtension, DocumentExtension, EditorExtension, FragmentExtension, OMDocExtension, STeXExtension, Translator}
import info.kwarc.mmt.stex.features.{DefinitionFeature, PillarFeature, TheoremFeature}
import info.kwarc.mmt.stex.translations.DemoContent
import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, ParsingState}
import info.kwarc.mmt.stex.xhtml._

import scala.runtime.NonLocalReturnControl
import scala.xml.parsing

case class ErrorReturn(s : String) extends Throwable {
  def toResponse = ServerResponse(s,"plain",ServerResponse.statusCodeNotFound)
}


class STeXServer extends ServerExtension("fomid") {

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
    addExtension(EditorExtension)
    //addExtension(DemoExtension)
    addExtension(classOf[PillarFeature])
    addExtension(classOf[DefinitionFeature])

    controller.backend.getArchives.filter(_.id.startsWith("FoMID")).foreach(_.readRelational(Nil, controller, "rel"))
  }

  override def apply(request: ServerRequest): ServerResponse = try {
    val ret = request.path.lastOption match {
      case Some("declaration") =>
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
        }
      case _ =>
        extensions.foreach(e => e.serverReturn(request) match {
          case Some(rsp) => return rsp
          case _ =>
        })
    }
    ServerResponse(ret.toString, "html")
  } catch {
    case ret:ErrorReturn => ret.toResponse
    case t : NonLocalReturnControl[Any] =>
      throw t
    case t : Throwable =>
      throw t
  }

  def doDeclaration(s : String) = {
    val path = Path.parseS(s)
    val c = controller.getConstant(path)
    val (doc,body) = emptydoc
    def space = scala.xml.Text(" ")
    val suffix = c.rl match {
      case Some("variable") =>
        body.add(<b>Variable</b>)
        c.metadata.getValues(STeX.meta_quantification) match {
          case List(OMS(STeX.Forall.path)) => scala.xml.Text("(universally quantified)")
          case List(OMS(STeX.Exists.path)) => scala.xml.Text("(existentially quantified)")
          case _ => space
        }
      case _ =>
        body.add(<b>Symbol</b>)
        space
    }
    body.add(space)
    body.add(<a href={"/?"+s} target="_blank">{XMLEscaping(c.path.toString)}</a>)
    body.add(<br/>)
    body.add(
      <table>
        <tr><th>Macro</th><th>Presentation</th><th>Type</th><th></th></tr>
        <tr>
          <td>{c.notC.parsing match {
              case Some(tn) => scala.xml.Text(tn.markers.mkString(""))
              case _ => scala.xml.Text("(None)")
            }}</td>
          <td>{c.notC.presentation match {
              case Some(tn) => scala.xml.Text(tn.markers.mkString(""))
              case _ => scala.xml.Text("(None)")
            }}</td>
          <td>{c.tp match {
              case Some(tpi) => xhtmlPresenter.asXML(tpi,Some(c.path $ TypeComponent))
              case _ => scala.xml.Text("(None)")
            }}</td>
          <td>{suffix}</td>
        </tr>
      </table>)
    doc
  }

  def doHeader(doc : HTMLNode) = {
    val head = doc.get("head")()().head
    val body = doc.get("body")()().head
    head.get("link")(("","rel","stylesheet"))().foreach(e => e.attributes.get(("","href")) match {
      case Some("https://latex.now.sh/style.css") => e.delete
      case Some("LaTeXML.css") => e.attributes(("","href")) = "/stex/latexml/LaTeXML.css"
      case Some(s) if s.startsWith("ltx-") => e.attributes(("","href")) = "/stex/latexml/" + s
      case _ =>
    })
    head.add(<link rel="stylesheet" href="/stex/latex-css/style.css"/>)
    head.add(<script type="text/javascript" id="MathJax-script" src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/mml-chtml.js">{HTMLParser.empty}</script>)
    extensions.foreach(_.doHeader(head,body))
    head
  }

  def emptydoc = {
    val rules = extensions.flatMap(_.rules)
    val state = new ParsingState(controller,rules)
    val doc = HTMLParser("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN\" \"http://www.w3.org/Math/DTD/mathml2/xhtml-math11-f.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:stex=\"http://kwarc.info/ns/sTeX\" xmlns:mml=\"http://www.w3.org/1998/Math/MathML\" lang=\"en\">")(state) //new XHTMLDocument
    doc.add(<head></head>)
    doc.add(<body><div class="ltx_page_main"><div class="ltx_page_content"><div class="ltx_document"></div></div></div></body>)
    doHeader(doc)
    (doc,doc.get("div")()("ltx_document").head)
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
