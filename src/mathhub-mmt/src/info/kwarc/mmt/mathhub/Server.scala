package info.kwarc.mmt.mathhub

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.text.DateFormat

import info.kwarc.mmt.api.utils.{JSONArray, MMTSystem}
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

  def applyActual(request: ServerRequest) : ServerResponse = request.pathForExtension match {
    case "version" :: Nil =>
      toResponse(getMMTVersion)

    case "content" :: "uri" :: Nil =>
      toResponse(getURI(request.parsedQuery.string("uri", return missingParameter("uri"))))
    case "content" :: "groups" :: Nil =>
      toResponse(getGroups())
    case "content" :: "group" :: Nil =>
      toResponse(getGroup(request.parsedQuery.string("id", return missingParameter("id"))))
    case "content" :: "archive" :: Nil =>
      toResponse(getArchive(request.parsedQuery.string("id", return missingParameter("id"))))
    case "content" :: "document" :: Nil =>
      toResponse(getDocument(request.parsedQuery.string("id", return missingParameter("id"))))
    case "content" :: "module" :: Nil =>
      toResponse(getModule(request.parsedQuery.string("id", return missingParameter("id"))))

    // fallback: Not Found
    case path => ServerResponse(s"API Route not found: ${path.mkString("/")}", "text/plain", ServerResponse.statusCodeNotFound)
  }

  def missingParameter(name: String) : ServerResponse = {
    ServerResponse(s"Missing GET parameter: $name", "text/plain", ServerResponse.statusCodeBadRequest)
  }

  /** turns an object into a server response */
  def toResponse(result: Option[IResponse]): ServerResponse = result match {
    case Some(r) => ServerResponse.fromJSON(r.toJSON)
    case None => ServerResponse("Not found", "text", ServerResponse.statusCodeNotFound)
  }
  def toResponse(results: List[IResponse]): ServerResponse = {
    ServerResponse.JsonResponse(JSONArray(results.map(_.toJSON): _*))
  }


  //
  // API Methods
  //

  /** gets the version of the MMT System */
  def getMMTVersion : Option[IMMTVersionInfo] = {
    val versionNumber = MMTSystem.version.split(" ").head

    val format = new java.text.SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
    val buildDate = MMTSystem.buildTime.map(format.parse).map(_.getTime.toString)

    Some(IMMTVersionInfo(versionNumber, buildDate))
  }

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
