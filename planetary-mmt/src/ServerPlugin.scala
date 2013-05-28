package info.kwarc.mmt.planetary

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.documents._

import zgs.httpd._

case class PlanetaryError(val text : String) extends Error(text)


class PlanetaryPlugin extends ServerPlugin with Logger {
  
  override val logPrefix = "planetary"
    
     /** Server */   
   def isApplicable(uriComp : String) = {
     uriComp == "planetary"
   }
   
   def apply(uriComps: List[String]): Option[HLet] = {
     try {

       uriComps match {
         case "listModules" :: _ => Some(ListModulesResponse)
         case "getArchives" :: _ => Some(getArchivesResponse)
         case "getPaths" :: _ => Some(getPathsResponse)

         case _ => None
       }
     } catch {
       case e : Error => log(e.msg); Some(errorResponse(e.msg))
       case e : Exception => Some(errorResponse("Exception occured : " + e.getMessage()))
     }
   }
   
   private def CORS_AllowOrigin(origin : String) = true //for now
   
   private def checkCORS(tk : HTalk) : HTalk = tk.req.header("Origin")  match {
     case None => tk
     case Some(s) => CORS_AllowOrigin(s) match {
       case true => tk.setHeader(" Access-Control-Allow-Origin", s)
       case false => tk
     }
   }
   
   private def getArchivesResponse : HLet = new HLet {
     def act(tk : HTalk) = try {
        val archives = controller.backend.getArchives map { a => (a.id, DPath(a.narrationBase))}
        val result = archives.map(p =>  p._1 + " " +  p._2).mkString("\n")
        log("getArchives returning: " + result)
        TextResponse(result).act(tk)
     } catch {
       case e : Error => 
         log(e.msg)
         errorResponse(e.shortMsg).act(tk)
       case e : Exception => 
         errorResponse("Exception occured : " + e.getMessage()).act(tk)
     }
   }
   
   private def getPathsResponse : HLet = new HLet {
     def act(tk : HTalk) = try {
        val archives = controller.backend.getArchives map { a => (a.id, DPath(a.narrationBase))}
        val result = archives.map(p => ((p._1 + " " + p._2.toPath) :: getDescendants(p._2).tail).mkString("\n")).mkString("\n")
        log("getPaths returning: " + result)
        TextResponse(result).act(tk)
     } catch {
       case e : Error => 
         log(e.msg)
         errorResponse(e.shortMsg).act(tk)
       case e : Exception => 
         errorResponse("Exception occured : " + e.getMessage()).act(tk)
     }
   }
   
   private def getDescendants(p : Path) : List[String] = {
     getChildren(p) match {
       case Nil => List(p.last + " " + p.toString, "")
       case l => (p.last + " " + p.toString) :: l.map(getDescendants).flatten ::: List("")
     }
   }
   
   private def getChildren(p : Path) : List[Path] = {
     controller.get(p).components collect {
       case ref : XRef => ref.target
     }
   }
   
   
   private def ListModulesResponse : HLet = new HLet {
     def act(tk : HTalk) = try {
       val pathsIt = controller.library.getAllPaths
       val result = pathsIt.map(p => p.toPath).mkString("\n")
       log("Returning : " + result)
       TextResponse(result).act(tk)
     } catch {
       case e : Error => 
         log(e.msg)
         errorResponse(e.shortMsg).act(tk)
       case e : Exception => 
         errorResponse("Exception occured : " + e.getMessage()).act(tk)
     }
   }
   
   
   
  // Utils
  private def bodyAsString(tk: HTalk): String = {
    val bodyArray: Array[Byte] = tk.req.octets.getOrElse(throw  PlanetaryError("no body found"))
    new String(bodyArray, "UTF-8")
  }
  
  private def errorResponse(text : String) : HLet = {
    TextResponse(s"MMT Error in Planetary extension: $text ")
  }
  
  /**
   * A text response that the server sends back to the browser
   * @param text the message that is sent in the HTTP body
   */
  private def TextResponse(text: String): HLet = new HLet {    
    def act(tk: HTalk) {
      val out = text.getBytes("UTF-8")
      checkCORS(tk).setContentLength(out.size) // if not buffered
        .setContentType("text/plain; charset=utf8")
        .write(out)
        .close
    }
  }
}