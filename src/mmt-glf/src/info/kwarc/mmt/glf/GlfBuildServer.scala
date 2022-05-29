package info.kwarc.mmt.glf

import info.kwarc.mmt.api.archives.{BuildSuccess, BuildTask}
import info.kwarc.mmt.api.checking.Interpreter
import info.kwarc.mmt.api.{ErrorContainer, MultipleErrorHandler}
import info.kwarc.mmt.api.utils.{FilePath, JSON, JSONArray, JSONBoolean, JSONObject, JSONString}
import info.kwarc.mmt.api.web.{ServerError, ServerExtension, ServerRequest, ServerResponse}


class GlfBuildServer extends ServerExtension("glf-build"){
    val gfImporter = new GfImporter()
    lazy val mmtImporter : Interpreter = controller.extman.get(classOf[Interpreter]).head

    override def start(args: List[String]): Unit = {
        super.start(args)
        controller.extman.addExtension(gfImporter)
    }

    def apply(request: ServerRequest): ServerResponse = {
        val query : GlfBuildQuery = GlfBuildQuery.fromJSON(request.body.asJSON)

        val archive = controller.backend.getArchive(query.archive)
          .getOrElse(return errorResponse("Failed to find archive: " + query.archive))

        val file = FilePath(query.file)
        var isGf = false
        file.getExtension match {
            case Some("mmt") => isGf = false
            case Some("gf") => isGf = true
            case Some(x) => return errorResponse("Unexpected file extension: " + x)
            case None => return errorResponse("File doesn't appear to have an extension")
        }

        val errorcontainer = new ErrorContainer

        val result = if (isGf) {
            val buildtask = new BuildTask("mmt-gf", archive, archive / gfImporter.inDim / file,
                None, file, archive / gfImporter.outDim / file.setExtension(gfImporter.outExt),
                new MultipleErrorHandler(List(errorcontainer)))
            try {
                gfImporter.buildFile(buildtask)
            } catch {
                case e : Exception => return errorResponse(e)
            }
        } else {   // MMT file
            val buildtask = new BuildTask("mmt-omdoc", archive, archive / mmtImporter.inDim / file,
                None, file, archive / mmtImporter.outDim / file.setExtension(mmtImporter.outExt),
                new MultipleErrorHandler(List(errorcontainer)))
            try {
                mmtImporter.buildFile(buildtask)
            } catch {
                case e : Exception => return errorResponse(e)
            }
        }
        ServerResponse.JsonResponse(JSONObject(
            ("isSuccessful", JSONBoolean(result.isInstanceOf[BuildSuccess])),
            ("errors", JSONArray(errorcontainer.getErrors.map(error => JSONString(error.toString)) : _*))
        ))
    }

    private def errorResponse(message : String) : ServerResponse = {
        ServerResponse.JsonResponse(JSONObject(
            ("isSuccessful", JSONBoolean(false)),
            ("errors", JSONArray(JSONString(message)))
        ))
    }

    private def errorResponse(ex : Exception): ServerResponse = {
        ex match {
            case _ : GfImportException =>
                ServerResponse.JsonResponse(JSONObject(
                    ("isSuccessful", JSONBoolean(false)),
                    ("errors", JSONArray(JSONString(ex.getClass.toString + ": " + ex.getMessage)))
                ))
            case _ =>
                ex.printStackTrace()
                ServerResponse.JsonResponse(JSONObject(
                    ("isSuccessful", JSONBoolean(false)),
                    ("errors", JSONArray(JSONString("Unexpected error: " + ex.getClass + ": " + ex.getMessage)))
                ))
        }
    }
}

class GlfBuildQuery(val file : String,
                    val archive : String)


object GlfBuildQuery {
    def fromJSON(json: JSON): GlfBuildQuery = {
        var file : Option[String] = None
        var archive: Option[String] = None

        json match {
            case JSONObject(map) =>
                for (entry <- map) {
                    entry match {
                        case (JSONString("file"), JSONString(value)) => file = Some(value)
                        case (JSONString("archive"), JSONString(value)) => archive = Some(value)
                        case (key, _) => throw ServerError("Invalid JSON: can't handle entry '" + key.toFormattedString("") + "'")
                    }
                }
            case _ => throw ServerError("Invalid JSON: Expected object")
        }
        new GlfBuildQuery(file.getOrElse(throw ServerError("No file specified")),
            archive.getOrElse(throw ServerError("No archive specified")))
    }
}
