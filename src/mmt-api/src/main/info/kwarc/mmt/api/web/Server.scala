package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import frontend._
import info.kwarc.mmt.api.utils.{MMTSystem, URI}

import scala.xml._

case class ServerError(msg: String) extends Error(msg)

import ServerResponse._

/** Represents an implementation for servers */
trait ServerImplementation {
  def bindHost : String
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
class Server(val port: Int, val host: String, controller: Controller) extends AkkaServerImplementation/*TiscafServerImplementation*/ with Logger {
  val serverName : String = "MMT HTTP Server"
  val listenAddress : String = host
  val listenPort : Int = port

  /**
    * The base URI at which all [[ServerExtension]]s listen.
    *
    * Usually something like [[http://localhost:8080/]].
    */
  def baseURI: URI = URI(s"http://$host:$port/")

  def handleMessage(s: String): Unit = {
    log(s)
  }
  def handleError(t: Throwable): Unit = {
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
}
