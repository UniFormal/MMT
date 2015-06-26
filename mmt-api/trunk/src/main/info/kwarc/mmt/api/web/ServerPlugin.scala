package info.kwarc.mmt.api.web

import java.util.Calendar

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils._
import tiscaf._

/**
 * An MMT extensions that handles certain requests in MMT's HTTP server.
 *
 * It will be called on URIs of the form http://server:port/:CONTEXT/PATH?QUERY
 *
 * @param context the CONTEXT
 */
abstract class ServerExtension(context: String) extends FormatBasedExtension {
  /**
   * @param cont the context of the request
   * @return true if cont is equal to this.context
   */
  def isApplicable(cont: String): Boolean = cont == context

  /**
   * handles the HTTP request
   * @param path the PATH from above (excluding CONTEXT)
   * @param query the QUERY from above
   * @param body the body of the request
   * @return the HTTP response
   *
   *         Implementing classes can and should use Server.XmlResponse etc to construct responses conveniently.
   *
   *         Errors thrown by this method are caught and sent back to the browser.
   */
  def apply(path: List[String], query: String, body: Body): HLet
}

/**
 * interprets the body as MMT content
 */
class PostServer extends ServerExtension("post") {
  def apply(path: List[String], query: String, body: Body) = {
    val wq = WebQuery.parse(query)
    val content = wq.string("body", throw ServerError("found no body in post req"))
    val format = wq.string("format", "mmt")
    val dpathS = wq.string("dpath", throw ServerError("expected dpath"))
    val dpath = DPath(URI(dpathS))
    log("Received content : " + content)
    controller.read(parser.ParsingStream.fromString(content, dpath, format), interpret = true)(ErrorThrower)
    Server.TextResponse("Success")
  }
}

/** interprets the query as an MMT document URI and returns the SVG representation of the theory graph */
class SVGServer extends ServerExtension("svg") {
  /**
   * @param path ignored
   * @param query the document path
   * @param body ignored
   */

  def apply(path: List[String], query: String, body: Body) = {
    val path = Path.parse(query, controller.getNamespaceMap)
    val (inNarr, newPath) = path match {
      // doc path
      case dp: DPath => (true, dp)
      // module path
      case mp: MPath => (false, mp)
      case gp: GlobalName => (false, gp.module.toMPath)
      case cp: CPath => (false, cp.parent.module.toMPath)
    }
    val svgFile = if (inNarr) {
      val dp = newPath.asInstanceOf[DPath]
      val (arch, inPath) = controller.backend.resolveLogical(dp.uri).getOrElse {
        throw LocalError("illegal path: " + query)
      }
      val inPathFile = archives.Archive.narrationSegmentsAsFile(inPath, "omdoc")
      arch.root / "export" / "svg" / "narration" / inPathFile
    } else {
      val mp = newPath.asInstanceOf[MPath]
      val arch = controller.backend.findOwningArchive(mp).getOrElse {
        throw LocalError("illegal path: " + query)
      }
      val inPathFile = archives.Archive.MMTPathToContentPath(mp)
      arch.root / "export" / "svg" / "content" / inPathFile
    }
    val node = utils.File.read(svgFile.setExtension("svg"))
    Server.TypedTextResponse(node, "text")
  }
}

/** interprets the body as a QMT [[ontology.Query]] and evaluates it */
class QueryServer extends ServerExtension("query") {
  /**
   * @param path ignored
   * @param httpquery ignored
   * @param body the query as XML
   */
  def apply(path: List[String], httpquery: String, body: Body) = {
    val mmtquery = body.asXML
    log("qmt query: " + mmtquery)
    val q = Query.parse(mmtquery)(controller.extman.get(classOf[QueryExtension]), controller.relman)
    //log("qmt query: " + q.toString)
    Query.infer(q)(Nil) // type checking
    val res = controller.evaluator.evaluate(q)
    val resp = res.toNode
    Server.XmlResponse(resp)
  }
}

/** HTTP frontend to the [[Search]] class */
class SearchServer extends ServerExtension("search") {
  private lazy val search = new Search(controller)
  private val mmlpres = new presentation.MathMLPresenter

  override def start(args: List[String]) {
    mmlpres.init(controller)
  }

  /**
   * @param path ignored
   * @param httpquery search parameters
   * @param body ignored
   */
  def apply(path: List[String], httpquery: String, body: Body) = {
    val wq = WebQuery.parse(httpquery)
    val base = wq("base")
    val mod = wq("module")
    val name = wq("name")
    val theory = wq("theory")
    val pattern = wq("pattern")
    val format = wq.string("format", "mmt")
    val intype = wq.boolean("type")
    val indef = wq.boolean("definition")
    val allcomps = List(TypeComponent, DefComponent)
    val comps = allcomps.zip(List(intype, indef)).filter(_._2).map(_._1)
    val pp = PathPattern(base, mod, name)
    val tp = (theory, pattern) match {
      case (Some(t), Some(p)) => Some(TermPattern.parse(controller, t, p, format))
      case (_, _) => None
    }
    val sq = SearchQuery(pp, comps, tp)
    val res = search(sq, resolveResults = true)
    val htmlres = HTML.build { h =>
      import h._
      div(attributes = List("xmlns" -> xml.namespace("html"), "xmlns:jobad" -> utils.xml.namespace("jobad"))) {
        res.foreach { r =>
          div("result") {
            val CPath(par, comp) = r.cpath
            div {
              text {
                comp.toString + " of "
              }
              span("mmturi", attributes = List("jobad:href" -> par.toPath)) {
                text {
                  par.last
                }
              }
            }
            r match {
              case SearchResult(cp, pos, None) =>
              case SearchResult(cp, pos, Some(term)) =>
                def style(pc: presentation.PresentationContext) = if (pc.pos == pos) "resultmatch" else ""
                div {
                  mmlpres(term, Some(cp), style)(new presentation.HTMLRenderingHandler(h))
                }
            }
          }
        }
      }
    }
    Server.XmlResponse(htmlres)
  }
}

/** interprets the query as an MMT [[frontend.GetAction]] and returns the result */
class GetActionServer extends ServerExtension("mmt") {
  def apply(path: List[String], query: String, body: Body) = {
    val action = Action.parseAct(query, controller.getBase, controller.getHome)
    val resp: String = action match {
      case GetAction(a: ToWindow) =>
        a.make(controller)
          <done action={a.toString}/>.toString
      case GetAction(a: Respond) =>
        a.get(controller)
      case _ =>
          <notallowed action={action.toString}/>.toString
    }
    Server.XmlResponse(resp)
  }
}

/** interprets the query as an MMT [[frontend.Action]] and returns the log output */
class ActionServer extends ServerExtension("action") {
  private lazy val logCache = new RecordingHandler(logPrefix)

  override def start(args: List[String]) {
    report.addHandler(logCache)
  }

  override def destroy {
    report.removeHandler(logPrefix)
  }

  def apply(path: List[String], query: String, body: Body): HLet = {
    val c = query.replace("%20", " ")
    val act = frontend.Action.parseAct(c, controller.getBase, controller.getHome)
    if (act == Exit) {
      // special case for sending a response when exiting
      new Thread {
        override def run {
          Thread.sleep(2)
          sys.exit
        }
      }.start
      return Server.XmlResponse(<exited/>)
    }
    logCache.record
    controller.handle(act)
    val r = logCache.stop
    logCache.clear
    val html = utils.HTML.build { h =>
      import h._
      div {
        r foreach { l => div {
          text {
            l
          }
        }
        }
      }
    }
    Server.XmlResponse(html)
  }
}


/**
 * Handle the body of the POST request (json format)
 * and store the comment as user+date into the discussions folder
 */
class SubmitCommentServer extends ServerExtension("submit_comment") {
  def apply(path: List[String], query: String, body: Body) = {
    val path = Path.parse(query, controller.getNamespaceMap)
    var s = body.asString
    val date = Calendar.getInstance().getTime.toString
    val end = date.replaceAll("\\s", "")
    //deprecated but will use this until better alternatives come along
    // one possible solution is Argonaut
    val result = scala.util.parsing.json.JSON.parseFull(s)
    var user = ""
    var comment = ""
    result match {
      case Some(map: Map[String@unchecked, String@unchecked]) =>
        user = map.getOrElse("user", null)
        comment = map.getOrElse("comment", null)
      case None => println("Parsing failed")
      case other => println("Unknown data structure: " + other)
    }
    val resp = "<comment>" +
      "<metadata>" +
      "<topic>" + query + "</topic>" +
      "<user>" + user + "</user>" +
      "<date>" + date + "</date>" +
      "</metadata>" +
      "<text>" + comment + "</text>" +
      "</comment>"
    val archive = "/archives/meta/inf/config/OAF/discussions/"
    val root = File(System.getProperty("user.dir")).up.up.up.up
    val f = root + archive + user + end + ".xml"
    val folder = File(f)
    try {
      write(folder, resp)
    }
    catch {
      case ex: Exception =>
        println(ex)
    }
    finally {
      Server.XmlResponse("<p>not ok</p>")
    }
    Server.XmlResponse("<p>OK</p>")
  }

  def Writer(f: File) = {
    f.up.toJava.mkdirs
    new StandardPrintWriter(f)
  }

  def write(f: File, s: String) {
    val fw = Writer(f)
    fw.write(s)
    fw.close
  }
}

class AlignServer extends ServerExtension("align") {
  //override def start(args: List[String]) {}
  def apply(path: List[String], query: String, body: Body) = {
    val root_hol = controller.backend.getArchive("hollight").foreach { a =>
      val alignments_hol = a.root / "relational" / "alignments" / "alignments.rel"
      val fhol = File(alignments_hol)
      try {
        read(fhol)
      }
      catch {
        case ex: Exception =>
          println(ex)
      }
    }
    val root_miz = controller.backend.getArchive("MML").foreach { a =>
      val alignments_miz = a.root / "relational" / "alignments" / "alignments.rel"
      val fmiz = File(alignments_miz)
      try {
        read(fmiz)
      }
      catch {
        case ex: Exception =>
          println(ex)
      }
    }
    val path = Path.parse(query, controller.getNamespaceMap)
    val list = getSymbolAlignments(path)._1
    if (list.isEmpty) {
      Server.TextResponse("")
    }
    else {
      val concept = getSymbolAlignments(path)._2.split("\\?").last
      val symbolAlignments = getSymbolAlignments(path)._1.distinct.map { s => s.toPath }
      val typeAlignments = getTypeAlignments(path).map { s => s.toPath }
      val prepJson = symbolAlignments.map { el =>
        "{\"name\": " + "\"" + el.split('?').last + "\", " + "\", parent\": " + "\"" + concept + "\"" + ", " + "\"address\": " + "\"" + el + "\"" + markLibrary(el) + "}"
      }.mkString(",")
      val rootJson = "[{\"name\":" + "\"" + concept + "\"" + ",\"parent\":\"null\"," + "\"children\": [" + prepJson + "]}]"
      Server.TextResponse(rootJson)
    }
  }

  def findHomeLibrary(path: String, lib: String): Boolean = {
    path contains lib
  }

  def markLibrary(s: String): String = {
    if (s contains "hol") " ,\"note\": " + "\"hol\""
    else if (s contains "MML") " ,\"note\": " + "\"mizar\""
    else if (s contains "Mizar") " ,\"note\": " + "\"mizar\""
    else "root"
  }

  def read(f: File) {
    File.ReadLineWise(f) { line =>
      val ns = NamespaceMap.empty
      val re = controller.relman.parse(line, ns)
      controller.depstore += re
    }
  }

  def printList(args: List[_]): Unit = {
    args.foreach(println)
  }

  def getSymbolAlignments(p: Path): (List[Path], String) = {
    val l = controller.depstore.queryList(p, Symmetric(Transitive(ToObject(IsAlignedWithSymbol))))
    if (l.isEmpty) {
      (l, "")
    }
    else {
      // identify the M-MMT URI and store in val concept
      val concept = l.head
      (controller.depstore.queryList(concept, Symmetric(Transitive(ToObject(IsAlignedWithSymbol)))), concept.toPath)
    }
  }

  def getTypeAlignments(p: Path): List[Path] = {
    controller.depstore.queryList(p, Symmetric(Transitive(ToObject(IsAlignedWithType))))
  }
}
