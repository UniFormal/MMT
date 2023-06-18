package info.kwarc.mmt.api.web

import java.util.Calendar
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.frontend.actions._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils._
import ServerResponse._
import info.kwarc.mmt.api.frontend.actions.{Action, GetAction}
import info.kwarc.mmt.api.objects.Context
import info.kwarc.mmt.api.presentation.PresentationMathMLPresenter

import scala.util.Try

/**
  * Creates an MMT extension that, upon being added to controller, hooks up with MMT's web server to listen at
  * [[http://server:port/:pathPrefix/PATH?QUERY]]
  */
abstract class ServerExtension(val pathPrefix: String) extends FormatBasedExtension {
  /**
   * @param cont the context of the request
   * @return true if cont is equal to this.context
   */
  def isApplicable(cont: String): Boolean = cont == pathPrefix

  /**
   * handles a request for this ServerExtension
   *
   * for implementation, the ServerResponse._ methods should be used
   * all errors are caught and displayed to the user when possible
   *
   * @param request The request sent to this ServerExtension
   * @return a response for this request
   */
  def apply(request: ServerRequest): ServerResponse
}

/** an example server extension that acts as a simple static web server
 *
 *  see also [[FileServerHere]]
 */
class FileServer extends ServerExtension("files") {
  private var rootO: Option[File] = None
  /** expects one argument - the root directory from which to serve files */
  override def start(args: List[String]): Unit = {
    args.headOption.map {h => rootO = Some(File(controller.getHome resolve h))}
  }
  def apply(request: ServerRequest): ServerResponse = {
    val root = rootO orElse controller.getMathHub.map(_.local) getOrElse {
      throw LocalError("no root defined")
    }
    val path = request.pathForExtension
    val show = (root / path).canonical
    if (!(root <= show)) {
      throw LocalError("inaccessible path: " + show)
    }
    if (!show.exists) {
      throw LocalError("file not found")
    } else if (show.isDirectory) {
      val html = HTML.build {h =>
        import h._
        h.html {h.body {h.ul {
          show.children foreach {c =>
            h.li {
              h.a("/:" + pathPrefix + "/" + path.map(_ + "/").mkString + c.name) {
               text(c.name)
              }
            }
          }
        }}}
      }
      HTMLResponse(html)
    } else {
      FileResponse(show)
    }
  }
}

/**
 * interprets the body as MMT content
 */
class PostServer extends ServerExtension("post") {
  def apply(request: ServerRequest): ServerResponse = {
    val wq = request.parsedQuery
    val content = wq.string("body", throw ServerError("found no body in post req"))
    val format = wq.string("format", "mmt")
    val dpathS = wq.string("dpath", throw ServerError("expected dpath"))
    val dpath = DPath(URI(dpathS))
    log("Received content : " + content)
    controller.read(parser.ParsingStream.fromString(content, dpath, format), interpret = true)(ErrorThrower)
    TextResponse("Success")
  }
}

/** interprets the query as an MMT document URI and returns the SVG representation of the theory graph */
class SVGServer extends ServerExtension("svg") with ContextMenuProvider {
  /**
   * request.path the export dimension from which to take the graph, "svg" by if empty
   * request.query the [[Path]] for which to retrieve a graph
   */
  def apply(request: ServerRequest): ServerResponse = {
    val path = Path.parse(request.query, controller.getNamespaceMap)
    val graphkey = request.pathForExtension.headOption.getOrElse("svg")
    lazy val exp = controller.extman.getOrAddExtension(classOf[RelationGraphExporter], graphkey).getOrElse {
      throw LocalError(s"svg file does not exist and exporter $graphkey not available: ${request.query}")
    }
    val fromFile = path.dropComp match {
      case _: MPath | _: DPath =>
        svgPath(path) flatMap {case (exportFolder, relPath) =>
          val svgFile = exportFolder / graphkey / relPath
          if (svgFile.exists) {
            val svg = utils.File.read(svgFile.setExtension("svg"))
            Some(svg)
          } else
            None
        }
      case _ =>
        None
    }
    val svg = fromFile getOrElse {
      val se = controller.get(path)
      exp.asString(se)
    }
    ServerResponse(svg, "image/svg+xml")
  }

  import MMTJavascript._
  def getEntries(path: Path) = {
    // all graphs we can build
    val exporters = controller.extman.get(classOf[RelationGraphExporter]).filter(e => e.canHandle(path))
    // all additional graphs that have been exported already
    val existingFiles = svgPath(path) match {
      case None => Nil
      case Some((exportFolder, relPath)) =>
        if (exportFolder.exists) exportFolder.children.collect {
          case f if !exporters.exists(e => e.isApplicable(f.name)) && (f/relPath).exists => f.name
        } else Nil
    }
    val allGraphs = exporters.map(e => (e.key, e.description)) ::: existingFiles.map(k => (k,k))
    allGraphs.map {case (key, description) =>
      ContextMenuEntry("show " + description, showGraph(key, path.toPath))
    }
  }

  /** @return (d,f) such that d/key/f is the path to the svg file for path exported by key */
  private def svgPath(path: Path): Option[(File, FilePath)] = {
    val (inNarr, newPath) = path.dropComp match {
      // narrative
      case dp: DPath => (true, dp)
      // content
      case c: ContentPath => (false, c.module)
    }
    val (arch, relPath) = if (inNarr) {
      val dp = newPath.asInstanceOf[DPath]
      val (arch, inPath) = controller.backend.resolveLogical(dp.uri).getOrElse {
        throw LocalError("illegal path: " + path)
      }
      val inPathFile = Archive.narrationSegmentsAsFile(FilePath(inPath), "omdoc")
      (arch, "narration" :: inPathFile)
    } else {
      val mp = newPath.asInstanceOf[MPath]
      val arch = controller.backend.findOwningArchive(mp).getOrElse {
        log("no archive known that contains " + path)
        return None
      }
      val inPathFile = Archive.MMTPathToContentPath(mp)
      (arch, "content" :: inPathFile)
    }
    val relPathSVG = FilePath(relPath).setExtension("svg")
    Some((arch.root / "export", relPathSVG))
  }
}

/** interprets the body as a QMT [[ontology.Query]] and evaluates it */
class QueryServer extends ServerExtension("query") {
  /**
    * Represents an error that occured because the context could not be properly parsed
    * @param candidate Candidate Path that could not be parsed
    * @param t Internal exception causing this error
    */
  class ContextParsingError(candidate : String, t: Exception) extends LocalError(s"Unable to use context: '$candidate' is not a valid MPath. ") {
    setCausedBy(t)
  }
  /**
    * Represents an error that occured because an item in the context could not be retrieved
    * @param item path to item that could not be retrieved
    * @param t Internal exception causing this error
    */
  class ContextRetrievalError(item: MPath, t: Exception) extends LocalError(s"Unable to use context: '${item.toPath}' could not be retrieved. Make sure the path exists. ") {
    setCausedBy(t)
  }
  /**
    * Represents ane error that occured because the query could not be properly parsed.
    * @param se Internal source error that caused this error
    */
  class QuerySourceParsingError(val se: SourceError) extends LocalError(s"Unable to parse query: Source error in Line ${se.ref.region.start.line}, Column ${se.ref.region.start.column} - Line ${se.ref.region.end.line}, Column ${se.ref.region.end.column}. Make sure your syntax is correct. ") {
    setCausedBy(se)
  }
  /**
    * Represents an error that occured because the query could not be properly translated.
    * @param t Cause of the error
    */
  class QueryTranslationParsingError(val t: Exception) extends Error(s"Unable to parse query: ${t.getMessage}. Make sure your syntax is correct. ") {
    setCausedBy(t)
  }
  def apply(request: ServerRequest): ServerResponse = {
    request.pathForExtension match {
      case List("sparql") =>
        ServerResponse.JsonResponse(
          controller.depstore.query(request.body.asString).getJson
        )
      case List("text") =>
        // find the parameters in the body
        val queryparams = request.body.asJSON match {
          case jo: JSONObject => jo
          case _ => throw LocalError("body must be JSON object")
        }
        // and extract them
        val query = queryparams("query") match {
          case Some(js: JSONString) =>
            js.value
          case _ => ""
        }
        val context = queryparams("context") match {
          case Some(ja: JSONArray) =>
            ja.values
               .map(_.asInstanceOf[JSONString].value)
               .map(pth => {
                 val mpath = try {
                   Path.parseM(pth, NamespaceMap.empty)
                 } catch {
                   case pe: ParseError =>
                     throw new ContextParsingError(pth, pe)
                 }
                 try {
                   controller.get(mpath)
                   Context(mpath)
                 } catch {
                   case ge: GetError =>
                     throw new ContextRetrievalError(mpath, ge)
                 }
               })
               .reduce(_ ++ _)
          case _ => throw LocalError("no context (which must be a JSONArray) found")
        }
        // and parse it
        log(s"parsing query from text $query $context")
        val q = try {
         Query.parse(query, context, controller)
        } catch {
          case se: SourceError =>
            throw new QuerySourceParsingError(se)
          case err: Exception =>
            throw new QueryTranslationParsingError(err)
        }

        // now run the query
        run(q, request)
      // POST to / => parse xml (for backward compatibility)
      case Nil | List("") if request.method == RequestMethod.Post =>
        // read xml from the body
        val queryxml = request.body.asXML
        // and parse it
        log(s"parsing query from xml $queryxml")
        val q = Query.parse(queryxml)(controller.extman.get(classOf[QueryFunctionExtension]), controller.relman)
        // now run the query
        run(q, request)
      // GET to / => show the query page (for humans)
      case Nil | List("") if request.method == RequestMethod.Get =>
        ResourceResponse("qmt.html")
        // anything else => Error
      case _ =>
        TextResponse("Not found")
    }
  }

  def run(query : Query, request: ServerRequest) : ServerResponse = {
    // check that the query is correct
    // TODO: Throw a proper error if this fails
    log(s"checking query $query")
    QueryChecker.infer(query)(Context.empty)
    // run the actual query
    log(s"running query $query")
    val res = controller.evaluator(query)
    ServerResponse.fromXML(res.toNode)
  }
}

/** HTTP frontend to the [[Search]] class */
class SearchServer extends ServerExtension("search") {
  private lazy val search = new Search(controller)
  private val mmlpres = new PresentationMathMLPresenter

  override def start(args: List[String]): Unit = {
    mmlpres.init(controller)
  }

  def apply(request: ServerRequest): ServerResponse = {
    val wq = request.parsedQuery
    val base = wq("base")
    val mod = wq("module")
    val name = wq("name")
    val theory = wq("theory")
    val pattern = wq("pattern") orElse request.body.asStringO
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
    XmlResponse(htmlres)
  }
}

abstract class TEMASearchServer(format : String) extends ServerExtension("tema-" + format) {
  val presenter : presentation.ObjectPresenter
  def process(query : String, settings : Map[String,String]) : objects.Term

  def toHTML(tm : objects.Term) : String = {
    val rh = new presentation.StringBuilder()
    presenter.apply(tm, None)(rh)
    rh.get
  }

  def getSettings(path : List[String], query : String, body : Body) : Map[String, String]

  def apply(request: ServerRequest): ServerResponse = {
    val searchS = request.body.asString
    val settings = getSettings(request.path, request.query, request.body)
    val mathmlS = toHTML(process(searchS, settings))
    val mathml = scala.xml.XML.loadString(mathmlS)
    val resp = postProcessQVars(mathml)
    TextResponse(resp.toString, "html")
  }

  def preProcessQVars(n : scala.xml.Node) : scala.xml.Node = n match {
    case <QVAR>{c}</QVAR> =>
      val name = xml.attr(c, "name")
      <OMV name={"qvar_" + name} />
    case n : scala.xml.Elem =>
      val child = n.child.map(preProcessQVars)
      new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, false, child : _*)
    case _ => n

  }

  def postProcessQVars(n : scala.xml.Node) : scala.xml.Node = n match {
    case n if n.label == "mi" && n.text.startsWith("qvar_") =>
      <mi class="math-highlight-qvar"> {n.text.substring(5)} </mi>
    case n if n.label == "ci" && n.text.startsWith("qvar_") =>
      <mws:qvar>{n.text.substring(5)}</mws:qvar>
    case n : scala.xml.Elem =>
      val child = n.child.map(postProcessQVars)
      new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, false, child : _*)
    case _ => n
  }
}

/** interprets the query as an MMT [[frontend.actions.GetAction]] and returns the result */
class GetActionServer extends ServerExtension("mmt") {
  def apply(request: ServerRequest): ServerResponse = {
    val action = Action.parseAct(controller, request.query)
    val resp: String = action match {
      case GetAction(a: ToWindow) =>
        a.make(controller)
          <done action={a.toString}/>.toString
      case GetAction(a: Respond) =>
        a.get(controller)
      case _ =>
          <notallowed action={action.toString}/>.toString
    }
    XmlResponse(resp)
  }
}

/** an HTTP interface for processing [[Message]]s */
class MessageHandler extends ServerExtension("content") {
  def apply(request: ServerRequest): ServerResponse = {
     val path = request.pathForExtension
     if (path.length != 1)
       throw LocalError("path must have length 1")
     val wq = request.parsedQuery
     lazy val inFormat = wq.string("inFormat")
     lazy val outFormat = wq.string("outFormat")
     lazy val theory = wq.string("theory")
     lazy val context = objects.Context(Path.parseM(theory, controller.getNamespaceMap))
     lazy val inURI = Path.parse(wq.string("uri"))
     lazy val in = request.body.asString
     val message: Message = path.head match {
       case "get"    => GetMessage(inURI, outFormat)
       case "delete" => DeleteMessage(inURI)
       case "add"    => AddMessage(???, inFormat, in)
       case "update" => UpdateMessage(???, inFormat, in)
       case "eval"   => EvaluateMessage(Some(context), inFormat, in, outFormat)
       case "prove"  => ProveMessage(Some(context), inFormat, in, outFormat)
       case "infer"  => InferMessage(Some(context), inFormat, in, outFormat)
       case s => throw LocalError("unknown command: " + s)
     }
     controller.handle(message) match {
       case ObjectResponse(obj, tp) => TextResponse(obj, tp)
       case StructureResponse(id) => TextResponse(id)
       case ErrorResponse(msg) => throw LocalError(msg)
     }
  }
}

/** interprets the query as an MMT [[frontend.actions.Action]] and returns the log output */
class ActionServer extends ServerExtension("action") {
  private lazy val logCache = new RecordingHandler(logPrefix)

  override def start(args: List[String]): Unit = {
    report.addHandler(logCache)
  }

  override def destroy: Unit = {
    report.removeHandler(logPrefix)
  }

  def apply(request: ServerRequest): ServerResponse = {
    val c = request.decodedQuery
    val act = frontend.actions.Action.parseAct(controller, c)
    if (act == Exit) {
      // special case for sending a response when exiting
      new Thread {
        override def run: Unit = {
          Thread.sleep(100)
          controller.handle(act)
        }
      }.start
      return XmlResponse(<exited/>)
    }
    logCache.record
    controller.handle(act)
    val r = logCache.stop
    logCache.clear
    val html = utils.HTML.build {h =>
      import h._
      div {
        r foreach {l => div {
          text {
            l
          }
        }
        }
      }
    }
    XmlResponse(html)
  }
}


/**
 * experimental server for submitting comments
 *
 * Handle the body of the POST request (json format)
 * and store the comment as user+date into the discussions folder
 */
class SubmitCommentServer extends ServerExtension("submit_comment") {
  def apply(request: ServerRequest): ServerResponse = {
    val path = Path.parse(request.query, controller.getNamespaceMap)
    var s = request.body.asString
    val date = Calendar.getInstance().getTime.toString
    val end = date.replaceAll("\\s", "")
    //deprecated but will use this until better alternatives come along
    // one possible solution is Argonaut
    val result = Try(JSON.parse(s)).toOption
    var user = ""
    var comment = ""
    result match {
      case Some(m@JSONObject(_)) => {
        user = m.getAsString("user")
        comment = m.getAsString("comment")
      }
      case None => println("Parsing failed")
      case other => println("Unknown data structure: " + other)
    }
    val resp = "<comment>" +
      "<metadata>" +
      "<topic>" + request.query + "</topic>" +
      "<user>" + user + "</user>" +
      "<date>" + date + "</date>" +
      "</metadata>" +
      "<text>" + comment + "</text>" +
      "</comment>"
    val archive = "/archives/meta/inf/config/OAF/discussions/"
    val root = File(System.getProperty("user.dir")).up.up.up.up
    val f = root.toString + archive + user + end + ".xml"
    val folder = File(f)
    try {
      write(folder, resp)
    }
    catch {
      case ex: Exception =>
        println(ex)
    }
    finally {
      XmlResponse("<p>not ok</p>")
    }
    XmlResponse("<p>OK</p>")
  }

  def Writer(f: File) = {
    File.Writer(f)
  }

  def write(f: File, s: String): Unit = {
    val fw = Writer(f)
    fw.write(s)
    fw.close
  }
}
