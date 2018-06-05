package info.kwarc.mmt.mathhub

import info.kwarc.mmt.api.utils.JSONConverter
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}

class Server extends ServerExtension("mathhub"){
  override val logPrefix: String = "mathhub"

  def apply(request: ServerRequest): ServerResponse = try {
    applyActual(request)
  } catch {
    case e: Exception => ServerResponse(
      e.getMessage, "text", ServerResponse.statusCodeInternalServerError
    )
  }

  /** decodes an ID used with the API */
  private def decodeID(values: List[String]) : String = {
    import java.net.URLDecoder
    import java.nio.charset.StandardCharsets
    URLDecoder.decode(values.mkString("/"), StandardCharsets.UTF_8.toString)
  }

  def applyActual(request: ServerRequest) : ServerResponse = request.pathForExtension match {
    case "content" :: "uri" :: args => toResponse(getURI(decodeID(args)))
    case "content" :: "groups" :: Nil => toResponse(getGroups())
    case "content" :: "group" :: args => toResponse(getGroup(decodeID(args)))
    case "content" :: "archive" :: args => toResponse(getArchive(decodeID(args)))
    case "content" :: "document" :: args => toResponse(getDocument(decodeID(args)))
    case "content" :: "module" :: args => toResponse(getModule(decodeID(args)))

    // fallback: Not Found
    case path => ServerResponse(s"API Route not found: ${path.mkString("/")}", "text/plain", ServerResponse.statusCodeNotFound)
  }

  /** turns an object into a server response */
  def toResponse(result: Option[IAPIObjectItem]): ServerResponse = result match {
    case Some(r) => ServerResponse.fromJSON(r.toJSON)
    case None => ServerResponse("Not found", "text", ServerResponse.statusCodeNotFound)
  }
  def toResponse(results: List[IAPIObjectItem]): ServerResponse = {
    import IAPIObjectItem._
    ServerResponse.JsonResponse(JSONConverter.toJSON(results))
  }


  //
  // API Methods
  //

  /** helper method to build a MathHubAPI Context
    * TODO: Figure out global caching
    */
  private def context: MathHubAPIContext = new MathHubAPIContext(controller, this.report)

  def getURI(uri: String) : Option[IReferencable] = {
    log(s"getObject($uri)")
    context.getObject(uri)
  }
  def getGroups() : List[IGroupRef] = {
    log(s"getGroups()")
    context.getGroups()
  }
  def getGroup(id: String) : Option[IGroup] = {
    log(s"getGroup($id)")
    context.getGroup(id)
  }
  def getArchive(id: String) : Option[IArchive] = {
    log(s"getArchive($id)")
    context.getArchive(id)
  }
  def getModule(id: String): Option[IModule] = {
    log(s"getModule($id)")
    context.getModule(id)
  }
  def getDocument(id: String): Option[IDocument] = {
    log(s"getDocument($id)")
    context.getDocument(id)
  }
}
