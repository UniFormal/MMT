package info.kwarc.mmt.api.web
import info.kwarc.mmt.api._
import frontend._
import ontology._
import utils._
import tiscaf._
import java.util.Calendar
import info.kwarc.mmt.api.metadata
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.metadata.MetaData
import info.kwarc.mmt.api.metadata.MetaDatum
import scala.util.parsing.json._

/**
 * An MMT extensions that handles certain requests in MMT's HTTP server.
 *
 * It will be called on URIs of the form http://server:port/:CONTEXT/PATH?QUERY
 *
 * @param cont the CONTEXT
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
   * Implementing classes can and should use Server.XmlResponse etc to construct responses conveniently.
   *
   * Errors thrown by this method are caught and sent back to the browser.
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
    controller.read(parser.ParsingStream.fromString(content, dpath, format), true)(ErrorThrower)
    Server.TextResponse("Success")
  }
}

/** interprets the query as an MMT document URI and returns the SVG representation of the theory graph */
class SVGServer extends ServerExtension("svg") {
  /**
   *  @param path ignored
   *  @param query the document path
   *  @param body ignored
   */

 	def apply(path: List[String], query: String, body: Body) = {
    val path = Path.parse(query, controller.getNamespaceMap)
    val (inNarr, newPath) = path match {
      // doc path
      case dp: DPath      => (true, dp)
      // module path
      case mp: MPath      => (false, mp)
      case gp: GlobalName => (false, gp.module.toMPath)
      case cp: CPath      => (false, cp.parent.module.toMPath)
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
   *  @param path ignored
   *  @param httpquery ignored
   *  @param body the query as XML
   */
  def apply(path: List[String], httpquery: String, body: Body) = {
    val mmtquery = body.asXML
    log("qmt query: " + mmtquery)
    val q = Query.parse(mmtquery)(controller.extman.get(classOf[QueryExtension]))
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
  override def start(args: List[String]) { mmlpres.init(controller) }
  /**
   *  @param path ignored
   *  @param httpquery search parameters
   *  @param body ignored
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
      case (_, _)             => None
    }
    val sq = SearchQuery(pp, comps, tp)
    val res = search(sq, true)
    val htmlres = HTML.build { h =>
      import h._
      div(attributes = List("xmlns" -> xml.namespace("html"), "xmlns:jobad" -> utils.xml.namespace("jobad"))) {
        res.foreach { r =>
          div("result") {
            val CPath(par, comp) = r.cpath
            div {
              text { comp.toString + " of " }
              span("mmturi", attributes = List("jobad:href" -> par.toPath)) { text { par.last } }
            }
            r match {
              case SearchResult(cp, pos, None) =>
              case SearchResult(cp, pos, Some(term)) =>
                def style(pc: presentation.PresentationContext) = if (pc.pos == pos) "resultmatch" else ""
                div { mmlpres(term, Some(cp), style)(new presentation.HTMLRenderingHandler(h)) }
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
        <done action={ a.toString }/>.toString
      case GetAction(a: Respond) =>
        a.get(controller)
      case _ =>
        <notallowed action={ action.toString }/>.toString
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
      (new Thread { override def run { Thread.sleep(2); sys.exit } }).start
      return Server.XmlResponse(<exited/>)
    }
    logCache.record
    controller.handle(act)
    val r = logCache.stop
    logCache.clear
    val html = utils.HTML.build { h =>
      import h._
      div { r foreach { l => div { text { l } } } }
    }
    Server.XmlResponse(html)
  }
}

/**
 * Server Plugin to handle the alignments requests
 * @param path 
 * @param query 
 * @param body 
 */
class AlignServer extends ServerExtension("align") {
  override def start(args: List[String]) {
    val root = File(System.getProperty("user.dir")).up.up.up.up
   // println("Root is " + root)
    val f = root + "/archives/meta/inf/config/OAF/alignments/alignments.rel"
    val folder = File(f)
    read(folder)
  }
  def apply(path: List[String], query: String, body: Body) = {
		val path = Path.parse(query, controller.getNamespaceMap)
		val list = getAlignments(path)
    if (list.isEmpty) {
    	val resp = "<div><p>No alignments for this symbol so far</p></div>"
      Server.XmlResponse(resp)
    } else {
    	var node_hol = ""
      var node_miz = ""
      var hol: List[String] = Nil
      var mizar: List[String] = Nil
      var open_math: List[String] = Nil
      for (p <- list) {
        if (Label(p, "hol-light")) {
          hol = p.toPath :: hol
        }
        if (Label(p, "MML")) {
          mizar = p.toPath :: mizar
        }
        if (Label(p, "openmath")) {
          open_math = p.toPath :: open_math
        }
      }
      if (hol.length == 0) {
        node_hol = ""
      } else {
      	node_hol = "<div><h3>HOL</h3></div>"
        for (h <- hol) {
          var title_hol = h.split('?').last
          node_hol = node_hol + "<div><span class=\"name\" jobad:href=\"" + h + "\">" + title_hol + "</span></div>"
        }
      }
      if (mizar.length == 0) {
        node_miz = ""
      } else {
        node_miz = "<div><h3>Mizar MML</h3></div>"
        for (m <- mizar) {
          var title_mizar = m.split('?').last
          node_miz = node_miz + "<div><span class=\"name\" jobad:href=\"" + m + "\">" + title_mizar + "</span></div>"
        }
      }
      val response = "<div xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:jobad=\"http://omdoc.org/presentation\">" + node_hol + node_miz + "</div>"
      Server.XmlResponse(response)
    }
  }
  def Label(path: Path, lib: String): Boolean = {
    path.toPath contains lib
  }
  def read(f: File) {
    File.ReadLineWise(f) { line =>
      var ns = NamespaceMap.empty
      var re = RelationalElement.parse(line, ns)
      controller.depstore += re
    }
  }
  def printList(args: List[_]): Unit = {
    args.foreach(println)
  }
  def getAlignments(p: Path): List[Path] = {
    controller.depstore.queryList(p, Symmetric(ToObject(IsAlignedWith)))
  }
}
/**
 * GET request to server to create the comment box
 * should be combined with SubmitCommentServer
 *
 */
class CommentBoxServer extends ServerExtension("comment_box") {
  def apply(path: List[String], query: String, body: Body) = {
    val node = ""
    Server.XmlResponse(node)
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
    val date = Calendar.getInstance().getTime().toString()
    val end = date.replaceAll("\\s", "")
    //deprecated but will use this until better altrnatives come along
    // one possible solution is Argonaut
    val result = scala.util.parsing.json.JSON.parseFull(s) 
    var user = ""
    var comment = ""
    result match {
      case Some(map: Map[String, String]) =>
        {
          user = map.getOrElse("user", null)
          comment = map.getOrElse("comment", null)
        }
      case None  => println("Parsing failed")
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
    write(folder, resp);
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