package scala.info.kwarc.mmt.api.webedit

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import objects._

import scala.util.parsing.json._
import scala.concurrent._
import tiscaf._

class WebEditServerPlugin extends ServerExtension("editing") with Logger {
  
  def error(msg : String) : HLet = {
    log("ERROR: " + msg)
    Server.errorResponse(msg)
  }
  
  def apply(uriComps: List[String], query: String, body : Body): HLet = {
    try {
      uriComps match {
        case "save" :: _ => getSaveResponse
        case "preview" :: _ => getPreviewResponse
        case "autocomplete" :: _ => getAutocompleteResponse
        case _ => error("Invalid request: " + uriComps.mkString("/"))
      }
    } catch {
      case e : Error => 
        log(e.longMsg) 
        Server.errorResponse(e.longMsg)
      case e : Exception => 
        error("Exception occured : " + e.getStackTrace())
    }
  }
  
  private def getSaveResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val response = "TODO"
      Server.TextResponse(response).aact(tk)
    }
  }
  
  private def getPreviewResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val response = "TODO"
      Server.TextResponse(response).aact(tk)
    }
  }
  
  private def getAutocompleteResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val bodyS = reqBody.asString
      val params = JSON.parseRaw(bodyS) match {
         case Some(j : JSONObject) => j.obj
         case _ => throw ServerError("Invalid JSON " + bodyS)
      }
      val prefix = params.get("prefix").getOrElse(throw ServerError("No prefix found")).toString
      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString
      val mpath = Path.parseM(mpathS, mmt.mmtbase)
      val declPaths = controller.library.getDeclarationsInScope(OMMOD(mpath)) collect {
        case d : Declaration => d.path
      }
      
      val myPaths = declPaths.filter(_.name.toPath.startsWith(prefix))
      
      val response = new JSONArray(myPaths.map(_.toPath))
      Server.JsonResponse(response).aact(tk)
    }
  }
}
