package info.kwarc.mmt.api.webedit

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web._
import tiscaf._

import scala.concurrent._


class WebEditServerPlugin extends ServerExtension("editing") with Logger {
  private lazy val editingService = new EditingServicePlugin(controller)

  def error(msg: String, req : HReqData): HLet = {
    log("ERROR: " + msg)
    Server.errorResponse(msg, req)
  }

  def bodyAsJSON(b : Body) = {
    val bodyS = b.asString
    scala.util.parsing.json.JSON.parseRaw(bodyS) match {
      case Some(j : scala.util.parsing.json.JSONObject) => j
      case _ => throw ServerError("Invalid JSON " + bodyS)
    }
  }

  def apply(request: Request): HLet = {
    try {
      request.path match {
        case "autocomplete" :: _ => getAutocompleteResponse
        case "resolve" :: _ => getResolveIncludesResponse
        case "minIncludes" :: _ => getMinIncludes
        case "symbolCompletion" :: _ => getSymbolCompletion
        //case "termInference" :: _ => getTermInference
        case "constantCorrection" :: _ => getConstantCorrection
        case "includeCorrection" :: _ => getIncludeCorrection
        case _ => error("Invalid request: " + request.path.mkString("/"), request.data)
      }
    } catch {
      case e: Error =>
        log(e.shortMsg)
        Server.errorResponse(e.shortMsg, request.data)
      case e: Exception =>
        error("Exception occured : " + e.getStackTrace(), request.data)
    }
  }

  private def getResolveIncludesResponse: HLet = new HLet {
    def aact(tk: HTalk)(implicit ec: ExecutionContext): Future[Unit] = {
      val reqBody = new Body(tk)
      val params = bodyAsJSON(reqBody).obj

      val symbol = params.get("symbol").getOrElse(throw ServerError("No symbol found")).toString
      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString

      val response = editingService.getResolveIncludesResponse(new MMTResolveIncludesRequest(symbol, mpathS))
      Server.JsonResponse(JSONObject(response.getResponse: _*)).aact(tk)
    }
  }

  private def getCoverCardinality(includes: List[MPath], theory: MPath) = {
    val thy = controller.get(theory) match {
      case theor: DeclaredTheory => theor
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

  private def getMinIncludes: HLet = new HLet {
    def aact(tk: HTalk)(implicit ec: ExecutionContext): Future[Unit] = {
      val reqBody = new Body(tk)
      val params = bodyAsJSON(reqBody).obj

      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString

      val newIncludes = editingService.getMinIncludes(new MMTMinIncludesRequest(mpathS))
      val response = JSONArray(newIncludes.getResponse: _*)
      Server.JsonResponse(response).aact(tk)
    }
  }

  private def getAutocompleteResponse: HLet = new HLet {
    def aact(tk: HTalk)(implicit ec: ExecutionContext): Future[Unit] = {
      val reqBody = new Body(tk)
      val params = bodyAsJSON(reqBody).obj

      val prefix = params.get("prefix").getOrElse(throw ServerError("No prefix found")).toString
      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString

      val myPaths = editingService.getAutocompleteResponse(new MMTAutoCompleteRequest(prefix, mpathS))
      val response = JSONArray(myPaths.getResponse: _*)
      Server.JsonResponse(response).aact(tk)
    }
  }

  private def getSymbolCompletion: HLet = new HLet {
    def aact(tk: HTalk)(implicit ec: ExecutionContext): Future[Unit] = {
      val reqBody = new Body(tk)
      val params = bodyAsJSON(reqBody).obj

      val prefix = params.get("prefix").getOrElse(throw ServerError("No prefix found")).toString
      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString

      val stringResponse = editingService.getSymbolCompletion(prefix)

      val response = new JSONArray(stringResponse map JSONString: _*)
      Server.JsonResponse(response).aact(tk)
    }
  }

  private def rank(includes: MPath, term: MPath): Int = {

    val constants = controller.get(term) match {
      case t: DeclaredTheory => t.getDeclarations
      case _ => throw new ServerError("No declarations")
    }
    val declarations: List[Declaration] = controller.get(includes) match {
      case t: DeclaredTheory => t.getDeclarations
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

  private def getConstantCorrection: HLet = new HLet {
    def aact(tk: HTalk)(implicit ec: ExecutionContext): Future[Unit] = {
      val reqBody = new Body(tk)
      val params = bodyAsJSON(reqBody).obj

      val constant = params.get("constant").getOrElse(throw ServerError("No type found")).toString
      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString


      val resp = editingService.getConstantCorrection(new MMTConstantCorrectionRequest(constant, mpathS))
      Server.JsonResponse(JSONString(resp.getResponse)).aact(tk)
    }
  }

  private def getIncludeCorrection: HLet = new HLet {
    def aact(tk: HTalk)(implicit ec: ExecutionContext): Future[Unit] = {
      val reqBody = new Body(tk)
      val params = bodyAsJSON(reqBody).obj

      val k = new LanguageDictionary(controller)

      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString
      val response = editingService.getIncludeCorrection(new MMTIncludeCorrectionRequest(mpathS))(controller.library)

      Server.JsonResponse(JSONString(response.getResponse)).aact(tk)
    }

  }

}

