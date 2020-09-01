package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import frontend._
import backend._
import documents._
import info.kwarc.mmt.api.utils.{MMTSystem, MMT_TODO}

import scala.xml._

case class ServerError(msg: String) extends Error(msg)

import ServerResponse._

/** Represents an implementation for servers */
trait ServerImplementation {
  /** the name of this server */
  val serverName : String
  /** the address this server should listen to */
  val listenAddress : String
  /** the port this server should listen to */
  val listenPort : Int
  /** handle a request sent to the server */
  def handleRequest(request : ServerRequest) : ServerResponse
  /** handle a log message by the underlying server */
  def handleMessage(s : String) : Unit
  /** handle a fatal error in the underlying implementation */
  def handleError(e: Throwable) : Unit
}

/** An HTTP RESTful server. */
class Server(val port: Int, val host: String, controller: Controller) extends TiscafServerImplementation with Logger {
  val serverName : String = "MMT HTTP Server"
  val listenAddress : String = host
  val listenPort : Int = port

  def handleMessage(s: String) {
    log(s)
  }
  def handleError(t: Throwable) {
    val e = GeneralError("error in underlying server").setCausedBy(t)
    log(e)
  }

  val logPrefix = "server"
  val report = controller.report

  /** handle a single request in a safe way */
  def handleRequest(request: ServerRequest): ServerResponse = {
    // log the request that was being made
    log(request.toStringShort)
    // build a response
    val response = try {
      //log(request.body.asString)
      resolve(request)
    } catch {
      case err: Error => errorResponse(err, "html")
      case e : Exception => errorResponse(ServerError("unknown error").setCausedBy(e), "html")
    }

    // log the response being made
    //log(response.toString)
    //set cors headers and return
    response.setCORSFor(request)
    response
  }

  /** resolves a specific request -- may throw exceptions */
  private def resolve(request: ServerRequest) : ServerResponse = {
    // check if our request method is OPTIONS, if so, do nothing
    if (request.method == RequestMethod.Options) {
      new ServerResponse
    // everything else
    } else {
      request.extensionName match {
        case Some("notices") => ServerResponse.fromText(MMTSystem.legalNotices) // legal notices are required
        case Some("debug") => resolveDebug(request)
        case Some("change") => resolveChange(request)
        case Some("mws") => resolveMWS(request)
        case Some(ext) => resolveExtension(ext, request)
        case None =>
          val path = request.path match {
            case Nil | List("") => "browse.html"
            case _ => request.pathString
          }
          ResourceResponse(path)
      }
    }
  }

  /** use a ServerExtension */
  private def resolveExtension(name : String, request : ServerRequest) : ServerResponse = {
    // retrieve the extension that should handle the context
    val extension = controller.extman.getOrAddExtension(classOf[ServerExtension], name) getOrElse {
      throw RegistrationError(s"no plugin registered for context $name")
    }
    log(s"handling request via plugin ${extension.logPrefix}")
    try {
      extension(request)
    } catch {
      case e: Error =>
        throw e
      case e: Exception =>
        throw extension.LocalError(s"unknown error while serving ${request.pathString}").setCausedBy(e)
    }
  }

  /** handles a response to the :change url */
  private def resolveChange(request : ServerRequest) : ServerResponse = {
    // TODO: Make this a very basic extension
    // to get rid of the special cases
    val bodyString = request.body.asString
    val bodyXML = Utility.trim(XML.loadString(bodyString))
    val reader = new moc.DiffReader(controller)
    val diff = reader(bodyXML)
    moc.Patcher.patch(diff, controller)
    ServerResponse.fromText("Success")
  }

  /** prints web-server debug info */
  private def resolveDebug(request: ServerRequest) : ServerResponse = {
    // TODO: Make this a small extension
    val bodyString = List(
      "MMT WebServer DEBUG / TESTING PAGE",
      "==================================",
      "",
      "",
      s"HTTP Request Method: ${RequestMethod.toString(request.method)}",
      s"HTTP Request Path:   ${request.pathString}",
      s"HTTP Query String:   ${request.query}",
      s"HTTP Parsed Query:   ${request.parsedQuery}",
      "",
      "HTTP Header fields:",
      request.headers.map(kv => s"${kv._1}: ${kv._2}").mkString("\n")
    ).mkString("\n")

    ServerResponse.fromText(bodyString)
  }

  @MMT_TODO("use SearchServer instead")
  /** handles a resolving an MWS response */
  private def resolveMWS(request : ServerRequest) : ServerResponse = {
    val body = request.body

    val offset = try request.headers("Offset").toInt catch {case _:Exception => 0}
    val size = try request.headers("Size").toInt catch {case _:Exception => 30}
    val query = request.query
    val qt = controller.extman.get(classOf[QueryTransformer], query).getOrElse(TrivialQueryTransformer)
    val (mwsquery, params) = query match {
      case "mizar" =>
        val bodyXML = body.asXML
        val mmlVersion = request.headers.getOrElse("MMLVersion", "4.166")
        val currentAid = request.headers.getOrElse("Aid", "HIDDEN")
        (bodyXML, List(currentAid, mmlVersion))
      case "tptp" => (scala.xml.Text(body.asString), Nil)
      case "lf" =>
        val scope = request.headers.lift("scope") match {
          case Some(s) =>
            Path.parse(s) match {
              case mp: MPath => mp
              case _ => throw ServerError("expected MPath, found : " + s)
            }
          case _ => throw ServerError("expected a scope (mpath) passed in header")
        }
        val tm = try {
          val str = body.asString
          val iiC = InterpretationInstructionContext(NamespaceMap(scope.doc))
          val pr = controller.objectParser(parser.ParsingUnit(parser.SourceRef.anonymous(str), objects.Context(scope), str, iiC))(ErrorThrower)
          pr.toTerm
        } catch {
          case e: Throwable =>
            throw e
        }

        def genQVars(n: Node): Node = n match {
          case a: scala.xml.Atom[_] => a
          case <ci>
            {q}
            </ci> =>
            if (q.toString.startsWith("?") || q.toString.startsWith("%3F")) {
              <mws:qvar>
                {q}
              </mws:qvar>
            } else {
              n
            }
          case _ => new scala.xml.Elem(n.prefix, n.label, n.attributes, n.scope, true, n.child.map(x => genQVars(x)): _*)
        }

        val processedQuery = genQVars(tm.toCML)
        (<mws:expr>
          {processedQuery}
        </mws:expr>, Nil)
      case _ => (body.asXML, Nil) // default: body is forwarded to MWS untouched
    }
    val tqs = qt.transformSearchQuery(mwsquery, params)

    def wrapMWS(n: Node): Node = <mws:query output="xml" limitmin={offset.toString} answsize={size.toString}>
      {n}
    </mws:query>

    val mws = controller.extman.get(classOf[ontology.MathWebSearch]).headOption.getOrElse(throw ServerError("no MathWebSearch engine defined")).url
    tqs.foreach(q => println(wrapMWS(q)))
    val res = tqs.map(q => utils.xml.post(mws, wrapMWS(q)))
    // calling MWS via HTTP post
    val total = res.foldRight(0)((r, x) => x + (r \ "@total").text.toInt)
    val totalsize = res.foldRight(0)((r, x) => x + (r \ "@size").text.toInt)
    val answrs = res.flatMap(_.child)
    val node = <mws:answset total={total.toString} size={totalsize.toString} xmlns:mws="http://www.mathweb.org/mws/ns">
      {answrs}
    </mws:answset>
    XmlResponse(node)
  }
}
