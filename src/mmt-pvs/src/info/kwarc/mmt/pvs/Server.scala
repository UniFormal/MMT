package info.kwarc.mmt.pvs

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils.{JSONArray, JSONObject, JSONString, URI}
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.lf.{Apply, ApplySpine, Arrow}
import info.kwarc.mmt.pvs.syntax.Object

import scala.util.Try
import scala.xml.Node

/**
  * Created by jazzpirate on 08.04.17.
  */
class PVSServer extends ServerExtension("pvs") {
  private val eqclos = Path.parseS("http://pvs.csl.sri.com/prelude?EquivalenceClosure?EquivClos",NamespaceMap.empty)
  val testterm = ApplySpine(PVSTheory.pvsapply.term,OMV("I1"),OMV("I2"),PVSTheory.parambind(OMS(eqclos),List(OMV("T"))),OMV("A"))

  //PVSTheory.(OMV("A"),OMV("B"))//PVSTheory.fun_type(OMV("A"),OMV("B"))
  val testcon = Context(VarDecl(LocalName("I1")),VarDecl(LocalName("I2")),VarDecl(LocalName("A")),VarDecl(LocalName("B")))

  def apply(request: ServerRequest): ServerResponse = {
    val tm = Try(processXML(request.body.asXML)) match {
      case scala.util.Success(term) => term
      case _ => testterm
    }
    val mwsquery = doWebQuery(tm)
    println(mwsquery.toXML)
    val results = mws(mwsquery).map(makeReply) // TODO
    ServerResponse.JsonResponse(JSONArray(results:_*))
    // ServerResponse.XmlResponse(mwsquery.toXML)
  }

  private def makeReply(qr : SearchResult) : JSONObject = {
    val rootpath = PVSTheory.rootdpath
    val prelpath = rootpath / "prelude"
    val nasapath = PVSTheory.nasapath
    val (libname,thname,symb,comp) = qr.cpath match {
      case CPath((dpath ? thname2) ?? symb2,comp2) if dpath <= prelpath =>
        (JSONString(""),JSONString(thname2.toString),JSONString(symb2.toString),JSONString(comp2.toString))
      case CPath((dpath ? thname2) ?? symb2,comp2) if dpath <= rootpath && dpath != rootpath =>
        (JSONString(dpath.last),JSONString(thname2.toString),JSONString(symb2.toString),JSONString(comp2.toString))
      case CPath((dpath ? thname2) ?? symb2,comp2) if dpath <= nasapath && dpath != nasapath =>
        (JSONString(dpath.last),JSONString(thname2.toString),JSONString(symb2.toString),JSONString(comp2.toString))
      case _ => (JSONString(""),JSONString(""),JSONString("Unknown: " + qr.cpath.toString),JSONString(qr.cpath.component.toString))
    }
    val ls = List(("lib_name",libname),("theory_name",thname),("name",symb),("component",comp),("Position",JSONString(qr.pos.toString)))
    JSONObject({if (qr.term.isDefined) ("Term",JSONString(controller.presenter.asString(qr.term.get))) :: ls else ls}:_*)
  }

  private def doWebQuery(tm : Term) : MathWebSearchQuery = {
    val (newterm : Term, metavars : Context) = (tm,if (tm == testterm) testcon else /* TODO */ Context.empty)
    MathWebSearchQuery(TermPattern(metavars,newterm))
  }

  private lazy val mws = controller.extman.get(classOf[MathWebSearch]).headOption.getOrElse{
    val nmws = new MathWebSearch(URI("http://mathhub.info:24367").toJava.toURL)
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
