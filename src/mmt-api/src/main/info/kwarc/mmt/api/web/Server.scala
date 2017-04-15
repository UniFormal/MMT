package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._

import frontend._
import backend._
import utils._

import tiscaf._
import tiscaf.let._

import scala.xml._
import scala.concurrent._

case class ServerError(msg: String) extends Error(msg)
case class ServerProcessingError(request : ServerRequest, exception : Exception) extends Error(s"unknown error while processing ${request.pathStr}") {
  setCausedBy(exception)
}


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

  def handleMessage(s: String): Unit = {
    controller.report("tiscaf", s)
  }

  def handleError(e: Throwable): Unit = {
    logError("error in underlying server: " + e.getClass + ":" + e.getMessage + "\n" + e.getStackTrace.map(_.toString).mkString("", "\n", ""))
  }

  val logPrefix = "server"
  val report = controller.report

  /** handle a single request in a safe way */
  def handleRequest(request: ServerRequest): ServerResponse = {

    // log the request that was being made
    log(s"${RequestMethod.toString(request.method)} /${request.pathStr}")



    // build a response
    val response = try {
      resolve(request)
    } catch {
      case err: Error => errorResponse(err, request)
      case e : Exception => errorResponse(ServerProcessingError(request, e), request)
    }

    // log the response being made
    log(s"${RequestMethod.toString(request.method)} /${request.pathStr} ${response.statusCode.code}")

    //set cors headers and return
    response.setCORSFor(request)
    response
  }

  /** resolves a specific request -- may throw exceptions */
  private def resolve(request: ServerRequest) : ServerResponse = {

    // check if our request method is OPTIONS, if so, do nothing
    if(request.method == RequestMethod.Options){
      new ServerResponse
    } else {
      request.extensionName match {
        case Some("change") => resolveChange(request)
        case Some("mws") => resolveMWS(request)
        case Some(ext) => resolveExt(ext, request)
        case None => resolveResource(request)
      }
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
    TextResponse("Success")
  }

  @deprecated("use SearchServer instead")
  /** handles a resolving an MWS resposne */
  private def resolveMWS(request : ServerRequest) : ServerResponse = {
    val body = request.body

    val offset = try request.headers("Offset").toInt catch {case _:Throwable => 0}
    val size = try request.headers("Size").toInt catch {case _:Throwable => 30}
    val query = request.queryString
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
          val pr = controller.objectParser(parser.ParsingUnit(parser.SourceRef.anonymous(str), objects.Context(scope), str, NamespaceMap(scope.doc)))(ErrorThrower)
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

  /** handles a single response */
  def resolveExt(name : String, request : ServerRequest) : ServerResponse = {
    // TODO: Check if we want access control

    // retrieve the extension that should handle the context
    val extension = controller.extman.getOrAddExtension(classOf[ServerExtension], name) getOrElse {
      return errorResponse(s"no plugin registered for context $name", request)
    }

    // log that we are handling an extension request
    log(s"handling request via plugin ${extension.logPrefix}")

    // and try to handle
    try {
      extension(request).asInstanceOf[ServerResponse]
    } catch {
      case e: Error =>
        throw e
      case e: Exception =>
        throw extension.LocalError(s"unknown error while serving ${request.pathStr}").setCausedBy(e)
    }
  }

  /** handles a resource request */
  def resolveResource(request : ServerRequest) : ServerResponse = {
    // log that we are handling an extension request
    log(s"handling resource request /${request.pathStr}")

    request.pathComponents match {
      case Nil | List("") => resource("browse.html", request)
      case _ => resource(request.pathStr, request)
    }
  }
}
