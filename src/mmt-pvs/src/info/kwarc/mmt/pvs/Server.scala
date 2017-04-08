package info.kwarc.mmt.pvs

import info.kwarc.mmt.api.{DPath, LocalName, MPath, Path}
import info.kwarc.mmt.api.objects.{Context, OMV, Term}
import info.kwarc.mmt.api.ontology.{MathWebSearch, MathWebSearchQuery, TermPattern}
import info.kwarc.mmt.api.utils.{JSONArray, JSONObject, JSONString, URI}
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.pvs.syntax.Object
import tiscaf.{HLet, HReqData}

import scala.xml.Node

/**
  * Created by jazzpirate on 08.04.17.
  */
class PVSServer extends ServerExtension("pvs") {

  def apply(path: List[String], query: String, body: Body, session: Session, req: HReqData): HLet = {
    val tm = processXML(body.asXML)
    val mwsquery = doWebQuery(tm)
    val results = mws(mwsquery).map(qr =>
      JSONObject(("Path",JSONString(qr.cpath.toString))::
        ("Position",JSONString(qr.pos.toString)) ::
        {if (qr.term.isDefined) ("Term",JSONString(controller.presenter.asString(qr.term.get))) :: Nil else Nil}:_*
      )) // TODO
    Server.JsonResponse(JSONArray(results:_*))
  }

  private def doWebQuery(tm : Term) : MathWebSearchQuery = {
    val (newterm : Term, metavars : Context) = (tm,???)
    MathWebSearchQuery(TermPattern(metavars,newterm))
  }

  private lazy val mws = controller.extman.get(classOf[MathWebSearch]).headOption.getOrElse{
    val nmws = new MathWebSearch(URI("http://mathhub.info:8659").toJava.toURL)
    controller.extman.addExtension(nmws)
    nmws
  }
  private lazy val parseXML = syntax.makeParser

  def processXML(xml : Node) : Term = {
    val path = DPath(URI.http colon "shemesh.larc.nasa.gov/fm/ftp/larc/PVS-library/SEARCH")
    val obj = parseXML(xml)
    val trl = new ObjectLevelTranslator(new SimpleState(path),controller)
    obj match {
      case o : Object => trl.doObject(o)
      case _ => throw ServerError("Could not parse as PVS Object: " + xml)
    }

  }

  private class SimpleState(val dpath : DPath) extends TranslationState {
    val path = dpath ? "SEARCH_INTERNAL"
    val isPrelude: Boolean = false
    def doSourceRef(o: Object, oM: Term): Unit = {}
    def getLocal(ln: LocalName): Term = OMV(ln)
    def addinclude(p: MPath): Unit = {}
    override def newName(s: String, start: Int, ln: Option[LocalName]): LocalName = LocalName(s)
  }
}
