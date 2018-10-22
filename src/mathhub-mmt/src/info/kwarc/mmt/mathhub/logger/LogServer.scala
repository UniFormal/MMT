package info.kwarc.mmt.mathhub.logger

import info.kwarc.mmt.api.utils.JSONArray
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.mathhub.{PathNotFound, Server}

trait LogServer {
  this: Server =>

  /** the caching log handler (if any) */
  private var handler: Option[CachingReportHandler] = None

  /** adds the logging handler for mathhub */
  protected def startLogServer(): Unit = {
    val crh = new CachingReportHandler("mathhub-cache")
    controller.report.addHandler(crh)
    handler = Some(crh)
  }

  /** reports the log handler for mathhub */
  protected def stopLogServer(): Unit = {
    controller.report.removeHandler(handler.get.id)
  }

  protected def applyLog(contentPath: List[String], request: ServerRequest): ServerResponse = {
    val entries = contentPath match {
      case "all" :: Nil =>
        handler.get.all
      case "after" :: Nil =>
        val uuid = request.parsedQuery.string("uuid", return missingParameter("uuid"))
        handler.get.after(uuid)

      case _ => throw PathNotFound(request)
    }

    ServerResponse.JsonResponse(JSONArray.fromList(entries.map(_.toJSON)))
  }
}
