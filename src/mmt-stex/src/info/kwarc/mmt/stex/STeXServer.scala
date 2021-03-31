package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.RedirectableDimension
import info.kwarc.mmt.api.notations.{Delim, Marker, SimpArg, Var}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.refactoring.AcrossLibraryTranslator
import info.kwarc.mmt.api.utils.{FilePath, MMTSystem, XMLEscaping}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.odk.Sage.{Sage, SageSystem}
import info.kwarc.mmt.stex.Extensions.{BrowserExtension, DemoExtension, DocumentExtension, EditorExtension, FeaturesExtension, FragmentExtension, OMDocExtension, STeXExtension, Translator}
import info.kwarc.mmt.stex.features.TheoremFeature
import info.kwarc.mmt.stex.translations.DemoContent
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

  override def start(args: List[String]): Unit = {
    super.start(args)
    if (!extensions.contains(OMDocExtension))
      controller.extman.addExtension(OMDocExtension)
    if (!extensions.contains(DemoExtension))
      controller.extman.addExtension(FeaturesExtension)
    if (!extensions.contains(DocumentExtension))
      controller.extman.addExtension(DocumentExtension)
    if (!extensions.contains(FragmentExtension))
      controller.extman.addExtension(FragmentExtension)
    if (!extensions.contains(BrowserExtension))
      controller.extman.addExtension(BrowserExtension)
    if (!extensions.contains(EditorExtension))
      controller.extman.addExtension(EditorExtension)
    if (!extensions.contains(DemoExtension))
      controller.extman.addExtension(DemoExtension)
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
        //doExpression(Obj.parseTerm(scala.xml.XML.loadString(xml),NamespaceMap.empty),compO)

        /*
      case Some("translate") =>
        val xml = request.body.params.get("openmath") match {
          case Some(s) =>
            XMLEscaping.unapply(s)
          case None =>
            ???
        }
        val trl = request.body.params.get("target") match {
          case Some(s) =>
            val translators = extensions.flatMap(_.translators)
            translators.find(_.language == s) match {
              case None =>
                ???
              case Some(trl) => trl
            }
          case _ =>
            ???
        }
        doTranslation(Obj.parseTerm(scala.xml.XML.loadString(xml),NamespaceMap.empty),trl)

         */
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
        body.add(<b>{scala.xml.Text("Variable")}</b>)
        c.metadata.getValues(STeX.meta_quantification) match {
          case List(OMS(STeX.Forall.path)) => scala.xml.Text("(universally quantified)")
          case List(OMS(STeX.Exists.path)) => scala.xml.Text("(existentially quantified)")
          case _ => space
        }
      case _ =>
        body.add(XHTML(<b>{scala.xml.Text("Symbol")}</b>)(Nil).head)
        space
    }
    body.add(space)
    body.add(<a href={"/?"+s} target="_blank">{XMLEscaping(c.path.toString)}</a>)
    body.add(<br/>)
    body.add(
      <table>
        <tr><th>{scala.xml.Text("Macro")}</th><th>{scala.xml.Text("Presentation")}</th><th>{scala.xml.Text("Type")}</th><th></th></tr>
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
/*
  def doExpression(o : Obj,src:Option[CPath]) = {
    val (doc,body) = emptydoc
    body.add("Expression: ")
    body.add(xhtmlPresenter.asXML(o,src))
    body.iterate {
      case e =>
        e.attributes.get(("","data-mmt-symref")) match {
          case Some(str) =>
            e.addOverlay("/:" + this.pathPrefix + "/fragment?" + str)
          case _ =>
        }
      case _ =>
    }
    body.add(<hr/>)
    body.add("LaTeX: ")
    body.add(<code><pre>{scala.xml.Text(texPresenter.asString(o))}</pre></code>)
    body.add(<hr/>)
    body.add("Translations: ")
    val translators = extensions.flatMap(_.translators)
    val translations = <table><tr>
      {translators.map {t =>
        <td><form method="post" action={"/:" + this.pathPrefix + "/translate"} target="_self" class="inline btncont">
          <input type="hidden" name="openmath" value={o.toNode.toString().replace("\n","").replace("\n","")}/>
          <input type="hidden" name="target" value={t.language}/>
          <a onclick="this.closest('form').submit();" type="submit" class="propbtn">
            {scala.xml.Text(t.language)}
          </a>
        </form></td>
      }}</tr></table>
    body.add(translations)
    doc
  }

  def doTranslation(tmI : Term,trl : Translator) = {
    val (doc,body) = emptydoc
    body.add("Expression: ")
    body.add(xhtmlPresenter.asXML(tmI,None))
    body.add(<hr/>)

    trl.translate(tmI) match {
      case (tm,Nil) =>
        body.add("Translated to " + trl.language + ": ")
        body.add(xhtmlPresenter.asXML(tm,None))
      case (_,ls) if ls.nonEmpty =>
        body.add("Translation to " + trl.language + " failed. Translators missing for:")
        body.add(<ul>{ls.map(p => <li><code>{scala.xml.Text(p.toString)}</code></li>)}</ul>)
    }
    body.iterate {
      case e if e.label == "mo" =>
        e.attributes.get(("","data-mmt-symref")) match {
          case Some(str) =>
            e.addOverlay("/:" + this.pathPrefix + "/fragment?" + str)
          case _ =>
        }
      case _ =>
    }
    doc
  }

 */

  def doHeader(doc : XHTMLNode) = {
    val head = doc.get("head")().head
    val body = doc.get("body")().head
    head.get("link")(("","rel","stylesheet")).foreach(e => e.attributes.get(("","href")) match {
      case Some("https://latex.now.sh/style.css") => e.delete
      case Some("LaTeXML.css") => e.attributes(("","href")) = "/stex/latexml/LaTeXML.css"
      case Some(s) if s.startsWith("ltx-") => e.attributes(("","href")) = "/stex/latexml/" + s
      case _ =>
    })
    head.add(<link rel="stylesheet" href="/stex/latex-css/style.css"/>)
    head.add(<script type="text/javascript" id="MathJax-script" src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/mml-chtml.js">{XHTML.empty}</script>)
    extensions.foreach(_.doHeader(head,body))
    head
  }

  def emptydoc = {
    val doc = new XHTMLDocument
    doc.add(<head></head>)
    doc.add(<body><div class="ltx_page_main"><div class="ltx_page_content"><div class="ltx_document"></div></div></div></body>)
    doHeader(doc)
    (doc,doc.get("div")(("","class","ltx_document")).head)
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
