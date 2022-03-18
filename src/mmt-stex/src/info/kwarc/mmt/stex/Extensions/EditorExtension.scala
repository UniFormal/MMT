package info.kwarc.mmt.stex.Extensions
/*
import info.kwarc.mmt.api.archives
import info.kwarc.mmt.api.archives.{Build, BuildTargetModifier}
import info.kwarc.mmt.api.utils.{File, FilePath, MMTSystem}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.ErrorReturn

object EditorExtension extends STeXExtension {
  override def serverReturn(request: ServerRequest): Option[ServerResponse] = request.path.tail match {
    case List("editor") =>
      Some(doEditor(request.query))
    case List("editor","content") =>
      val f = getTeXFile(request.query)
      Some(ServerResponse.fromText(File.read(f)))
    case List("editor","save") =>
      val f = getTeXFile(request.query)
      val ret = request.body.params("content")
      File.write(f,ret)
      Some(ServerResponse.TextResponse("OK."))
    case List("editor","compile") =>
      val f = getTeXFile(request.query)
      val ret = request.body.params("content")
      File.write(f,ret)
      server.resolveDocumentQuery(request.query) match {
        case (_,Some(archive),path) =>
          controller.buildArchive(List(archive.id),"stex-omdoc",Build,path.setExtension("tex"))
      }
      Some(ServerResponse.TextResponse("OK."))
    case _ => None
  }

  def doEditor(query : String) = {
    var content = MMTSystem.getResourceAsString("/mmt-web/stex/editor/main.html")
    content = content.replace("CONTENT_URL",query)
    ServerResponse(content,"application/xhtml+xml")
  }

  def getTeXFile(s : String) = {
    server.resolveDocumentQuery(s) match {
      case (_,Some(a),filepath) =>
        val src = ((a / archives.source) / filepath).setExtension("tex")
        if (src.exists()) src else throw ErrorReturn("File not found: " + src.toString)
      case _ =>
        throw ErrorReturn("No archive provided")
    }
  }
}
*/