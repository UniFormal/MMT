package info.kwarc.mmt.mathhub

import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}
import info.kwarc.mmt.mathhub.library._
import info.kwarc.mmt.mathhub.logger.LogServer

class Server extends ServerExtension("mathhub") with LibraryServer with LogServer with GraphServer {
  override val logPrefix: String = "mathhub"

  override def start(args: List[String]): Unit = {
    startLogServer()
  }

  override def destroy: Unit = {
    stopLogServer()
  }

  def apply(request: ServerRequest): ServerResponse = try {
    applyActual(request)
  } catch {
    case PathNotFound(p) =>
      ServerResponse(s"API Route not found: $p", "text/plain", ServerResponse.statusCodeNotFound)
    case t: Throwable =>
      ServerResponse(
        Option(t.getMessage).getOrElse("null"), "text", ServerResponse.statusCodeInternalServerError
      )
  }

  def applyActual(request: ServerRequest) : ServerResponse = request.pathForExtension match {
    // both content and version go into the content plugin
    // TODO: Refactor this in the future
    case "content" :: l => applyContent(l, request)
    case "version" :: Nil => applyContent(List("version"), request)

    // the log server
    case "log" :: l => applyLog(l, request)

    // the graph server, for now just exposed under :jgraph
    // TODO: Refactor this once Max has refactored the code
    case ":jgraph" :: _ => applyGraph(request)

    // everything else isn't found
    case _ => throw PathNotFound(request)
  }

  def missingParameter(name: String) : ServerResponse = {
    ServerResponse(s"Missing GET parameter: $name", "text/plain", ServerResponse.statusCodeBadRequest)
  }

}

/** exception that is thrown when a path is not found */
case class PathNotFound(path: String) extends Throwable
object PathNotFound {
  def apply(request: ServerRequest): PathNotFound = PathNotFound(request.pathString)
}
