package info.kwarc.mmt.mathhub

import info.kwarc.mmt.api.utils.JSONConverter
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}

class Server extends ServerExtension("mathhub") with MathHubAPIImpl  {
  override val logPrefix = "user"

  def apply(request: ServerRequest): ServerResponse = try {
    applyActual(request)
  } catch {
    case e: Exception => ServerResponse(
      e.getMessage, "text", ServerResponse.statusCodeInternalServerError
    )
  }

  def applyActual(request: ServerRequest) : ServerResponse = request.pathForExtension match {
    // TODO: Do proper path escaping
    case "content" :: "uri" :: g => toResponse(getURI(g.mkString("/")))
    case "content" :: "groups" :: Nil => toResponse(getGroups)
    case "content" :: "group" :: g => toResponse(getGroup(g.mkString("/")))
    case "content" :: "archive" ::g :: a => toResponse(getArchive(g, a.mkString("/")))
    case "content" :: "document" :: g :: a :: d => toResponse(getDocument(g, a, d.mkString("/")))
    case "content" :: "module" :: g :: a :: m => toResponse(getModule(g, a, m.mkString("/")))

    // fallback: Not Found
    case _ => ServerResponse("Not found", "text/plain", ServerResponse.statusCodeNotFound)
  }

  /** turns an object into a server response */
  def toResponse(result: IAPIObjectItem): ServerResponse = ServerResponse.JsonResponse(result.toJSON)
  def toResponse(results: List[IAPIObjectItem]): ServerResponse = {
    import IAPIObjectItem._
    ServerResponse.JsonResponse(JSONConverter.toJSON(results))
  }
}
