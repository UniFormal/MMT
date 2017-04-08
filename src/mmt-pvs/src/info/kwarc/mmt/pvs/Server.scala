package info.kwarc.mmt.pvs

import info.kwarc.mmt.api.{DPath, LocalName, MPath, Path}
import info.kwarc.mmt.api.objects.{OMV, Term}
import info.kwarc.mmt.api.ontology.{MathWebSearch, MathWebSearchQuery}
import info.kwarc.mmt.api.utils.{JSONArray, JSONString, URI}
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
    val results = mws.apply(doWebQuery(tm)).map(_.cpath.toString) // TODO
    Server.JsonResponse(JSONArray(results.map(JSONString):_*))
  }

  private def doWebQuery(tm : Term) : MathWebSearchQuery = ???

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
