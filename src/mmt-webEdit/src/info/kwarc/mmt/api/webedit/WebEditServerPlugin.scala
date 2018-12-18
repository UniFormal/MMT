package info.kwarc.mmt.api.webedit

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web._

import scala.concurrent._


class WebEditServerPlugin extends ServerExtension("editing") with Logger {
  private lazy val editingService = new EditingServicePlugin(controller)

  def error(msg: String, req : ServerRequest): ServerResponse = {
    log("ERROR: " + msg)
    ServerResponse.errorResponse(msg)
  }

  def bodyAsJSON(b : Body) = {
    val bodyS = b.asString
    scala.util.parsing.json.JSON.parseRaw(bodyS) match {
      case Some(j : scala.util.parsing.json.JSONObject) => j
      case _ => throw ServerError("Invalid JSON " + bodyS)
    }
  }

  def apply(request: ServerRequest): ServerResponse = {
    try {
      request.path match {
        case "autocomplete" :: _ => getAutocompleteResponse(request)
        case "resolve" :: _ => getResolveIncludesResponse(request)
        case "minIncludes" :: _ => getMinIncludes(request)
        case "symbolCompletion" :: _ => getSymbolCompletion(request)
        //case "termInference" :: _ => getTermInference(request)
        case "constantCorrection" :: _ => getConstantCorrection(request)
        case "includeCorrection" :: _ => getIncludeCorrection(request)
        case _ => error("Invalid request: " + request.path.mkString("/"), request)
      }
    } catch {
      case e: Error =>
        log(e.shortMsg)
        ServerResponse.errorResponse(e.shortMsg)
      case e: Exception =>
        error("Exception occured : " + e.getStackTrace(), request)
    }
  }

  private def getResolveIncludesResponse(request: ServerRequest): ServerResponse = {
    val reqBody = request.body
    val params = bodyAsJSON(reqBody).obj

    val symbol = params.get("symbol").getOrElse(throw ServerError("No symbol found")).toString
    val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString

    val response = editingService.getResolveIncludesResponse(new MMTResolveIncludesRequest(symbol, mpathS))
    ServerResponse.JsonResponse(JSONObject(response.getResponse: _*))
  }

  private def getCoverCardinality(includes: List[MPath], theory: MPath) = {
    val thy = controller.get(theory) match {
      case theor: Theory => theor
      case _ => throw ServerError("No theory found")
    }
    val fromCheck = thy.getIncludes
    fromCheck.filter(x => includes.contains(x))
  }


  private def getMax(includes: List[MPath], theories: List[MPath], current: Int, currentCover: List[MPath]): List[MPath] =
    theories match {
      case hd :: tl =>
        val card = getCoverCardinality(includes, hd)
        if (card.length > current) getMax(includes, tl, card.length, card)
        else
          getMax(includes, tl, current, currentCover)
      case Nil => currentCover
    }


  private def minInclLevel(incl: List[MPath], availTheories: List[MPath], CoverTheories: List[MPath]): List[MPath] = {
    val k = getMax(incl, availTheories, 0, Nil)
    val removed = incl.filterNot(x => k.contains(x))
    if (removed == Nil) k ::: CoverTheories else minInclLevel(removed, availTheories, k ::: CoverTheories)
  }

  private def getMinIncludes(request: ServerRequest): ServerResponse = {
    val reqBody = request.body
    val params = bodyAsJSON(reqBody).obj

    val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString

    val newIncludes = editingService.getMinIncludes(new MMTMinIncludesRequest(mpathS))
    val response = JSONArray(newIncludes.getResponse: _*)
    ServerResponse.JsonResponse(response)
  }

  private def getAutocompleteResponse(request: ServerRequest): ServerResponse = {
    val reqBody = request.body
    val params = bodyAsJSON(reqBody).obj

    val prefix = params.get("prefix").getOrElse(throw ServerError("No prefix found")).toString
    val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString

    val myPaths = editingService.getAutocompleteResponse(new MMTAutoCompleteRequest(prefix, mpathS))
    val response = JSONArray(myPaths.getResponse: _*)
    ServerResponse.JsonResponse(response)
  }

  private def getSymbolCompletion(request: ServerRequest): ServerResponse = {
    val reqBody = request.body
    val params = bodyAsJSON(reqBody).obj

    val prefix = params.get("prefix").getOrElse(throw ServerError("No prefix found")).toString
    val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString

    val stringResponse = editingService.getSymbolCompletion(prefix)

    val response = new JSONArray(stringResponse map JSONString: _*)
    ServerResponse.JsonResponse(response)
  }

  private def rank(includes: MPath, term: MPath): Int = {

    val constants = controller.get(term) match {
      case t: Theory => t.getDeclarations
      case _ => throw new ServerError("No declarations")
    }
    val declarations: List[Declaration] = controller.get(includes) match {
      case t: Theory => t.getDeclarations
      case _ => throw new ServerError("No declarations")
    }
    val decl = declarations.map(_.name)

    //the function to get used Declarations
    def getUsage(term: Term): List[OMID] = {
      term match {
        case OMBINDC(nterm, ncontext, nbodyList) => (nterm :: nbodyList).flatMap(getUsage(_))
        case OMA(f, args) => (f :: args).flatMap(getUsage(_))
        case OMS(f) => OMS(f) :: Nil
        case k => Nil
      }
    }

    val usedDeclarations = constants.flatMap(_.getComponents).flatMap {
      case DeclarationComponent(_, t: AbstractTermContainer) => t.get.map(getUsage)
      case _ => Nil
    }.toSet

    val usedDeclarationsNames = usedDeclarations.flatMap(l => l.map(_.path.name))
    val includesDeclarations = decl.toSet
    includesDeclarations.intersect(usedDeclarationsNames).size
  }

  // private def getTermInference : HLet = new HLet {
  //    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
  //      val reqBody = new Body(tk)
  //      val params = reqBody.asJSON.obj
  //
  //      val termS = params.get("term").getOrElse(throw ServerError("No type found")).toString
  //      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString
  //
  //      val resp = editingService.getTermInference(new MMTTermInferenceRequest(termS,mpathS))
  //      Server.JsonResponse(new JSONArray(resp.getResponse())).aact(tk)
  //    }
  // }

  private def getConstantCorrection(request: ServerRequest): ServerResponse = {
    val reqBody = request.body
    val params = bodyAsJSON(reqBody).obj

    val constant = params.get("constant").getOrElse(throw ServerError("No type found")).toString
    val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString


    val resp = editingService.getConstantCorrection(new MMTConstantCorrectionRequest(constant, mpathS))
    ServerResponse.JsonResponse(JSONString(resp.getResponse))
  }

  private def getIncludeCorrection(request: ServerRequest): ServerResponse = {
      val reqBody = request.body
      val params = bodyAsJSON(reqBody).obj

      val k = new LanguageDictionary(controller)

      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString
      val response = editingService.getIncludeCorrection(new MMTIncludeCorrectionRequest(mpathS))(controller.library)

      ServerResponse.JsonResponse(JSONString(response.getResponse))
    }

}

