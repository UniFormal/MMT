package info.kwarc.mmt.mathhub

import info.kwarc.mmt.api.utils.{JSON, JSONConverter, JSONListBuffer}
import info.kwarc.mmt.api.web.{ServerExtension, ServerRequest, ServerResponse}

class Extension extends ServerExtension("mathhub") {
  def apply(request: ServerRequest): ServerResponse = try {
    applyActual(request)
  } catch {
    case e: Exception => ServerResponse(
      e.getMessage, "text", ServerResponse.statusCodeInternalServerError
    )
  }

  def applyActual(request: ServerRequest) : ServerResponse = request.pathForExtension match {
    case List("content", "groups") => toResponse(getGroups())
    case List("content", "group", g) => toResponse(getGroup(g))
    case List("content", "archive", g, a) => toResponse(getArchive(g, a))
    case List("content", "module", g, a, m) => toResponse(getModule(g, a, m))

    // fallback: Not Found
    case _ => ServerResponse("Not found", "text/plain", ServerResponse.statusCodeNotFound)
  }

  /** turns an object into a server response */
  def toResponse(omdoc: OMDOC): ServerResponse = ServerResponse.JsonResponse(omdoc.toJSON)
  def toResponse(omdocs: List[OMDOC]): ServerResponse = {
    import OMDOC._
    ServerResponse.JsonResponse(JSONConverter.toJSON(omdocs))
  }


  //
  // API Methods
  //


  private def getGroups() : List[IGroupItem] = {
    ???
  }

  private def getGroup(group: String) : IGroup  = {
    ???
  }

  private def getArchive(group: String, name: String) : IArchive = {
    ???
  }

  private def getModule(group: String, archive: String, name: String): IModuleItem = {
    ???
  }
}
