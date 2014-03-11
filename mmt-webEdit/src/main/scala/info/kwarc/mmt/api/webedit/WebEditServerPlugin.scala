package scala.info.kwarc.mmt.api.webedit

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules.DeclaredTheory
import objects._
import libraries._
import scala.util.parsing.json._
import scala.concurrent._
import tiscaf._
import scala.collection.mutable._

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
        case "resolve" :: _ => getResolveResponse
        case "minIncludes" :: _ => getMinIncludes
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
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
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
 
  
  private def getResolveResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
      
      val symbol = params.get("symbol").getOrElse(throw ServerError("No symbol found")).toString
      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString
      val mpath = Path.parseM(mpathS, mmt.mmtbase)
      
      val respMap = new collection.mutable.HashMap[String, Any]()
      Names.resolveIncludes(OMMOD(mpath),symbol)(controller.library) match {
        case None => respMap("found") = true
        case Some(myPaths) => 
          respMap("found") = false
          respMap("options") = new JSONArray(myPaths.map(_.from.toPath))
      }
      Server.JsonResponse(new JSONObject(respMap.toMap)).aact(tk)
    }
  }
  
  
  private def minIncl(to: MPath) : List[MPath] = {
    val thy = controller.get(to) match {
      case theor: DeclaredTheory => theor
      case _ => throw ServerError("No theory found")
    }
    val incl = thy.getIncludes
    
    def remover(checked: List[MPath], rest : List[MPath]) : List[MPath] = rest match{
      case Nil => checked
      case (hd::tl) => 
        if(tl.exists(x=>controller.globalLookup.visible(OMMOD(x)).contains(OMMOD(hd))) ||
            checked.exists(x=>controller.globalLookup.visible(OMMOD(x)).contains(OMMOD(hd))))
        	remover(checked,tl)
    	else remover(hd::checked,tl)	
    }
    
   remover(Nil,incl)
  }
 
  
 
  private def getMinIncludes : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
      
      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString
      val mpath = Path.parseM(mpathS, mmt.mmtbase)		
    
      val newIncludes = minIncl(mpath)
       val response = new JSONArray(newIncludes.map(_.toPath))
      Server.JsonResponse(response).aact(tk)
    }
  }
  
  private def getAutocompleteResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
      
      val prefix = params.get("prefix").getOrElse(throw ServerError("No prefix found")).toString
      val mpathS = params.get("mpath").getOrElse(throw ServerError("No mpath found")).toString
      val mpath = Path.parseM(mpathS, mmt.mmtbase)
      
      val myPaths = Names.resolve(OMMOD(mpath), Nil, prefix)(controller.globalLookup)
      //mPaths.map(controller.get) collect {
      //  case c : Constant => c.not.toString
      // return in form of ((string,meaning),arguments,return type))
      //}
      val response = new JSONArray(myPaths.map(_.path.toPath))
      Server.JsonResponse(response).aact(tk)
    }
  }
}
