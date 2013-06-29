package info.kwarc.mmt.planetary

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.stex._

import scala.util.parsing.json._
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
         case "getArchives" :: _ => Some(getArchivesResponse)
         case "getPaths" :: _ => Some(getPathsResponse)
         case "getPresentation" :: _ => Some(getPresentationResponse)
         case "getCompiled" :: _ => Some(getCompiledResponse)
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
   
   private def getCompiledResponse : HLet = new HLet {
     def act(tk : HTalk) = try {
       val reqS = bodyAsString(tk)
       val params = JSON.parseRaw(reqS) match {
         case Some(j : JSONObject) => j.obj
         case _ => throw ServerError("Invalid JSON " + reqS)
       }
       val bodyS = params.get("body").getOrElse(throw ServerError("No Body Found")).toString
       val dpathS = params.get("dpath").getOrElse(throw ServerError("No dpath Found")).toString
       println("bodyS: " + bodyS)
       println("dpathS: " + dpathS)
       
       val dpath = DPath(utils.URI(dpathS))
       val comp = new STeXImporter()
       comp.init(controller, Nil)
       val resp = comp.compileOne(bodyS, dpath)
       println("Response : " + resp)
       TextResponse(resp).act(tk)       
     } catch {
       case e : Error => 
         log(e.msg)
         errorResponse(e.shortMsg).act(tk)
       case e : Exception => 
         errorResponse("Exception occured : " + e.getMessage() + e.getStackTrace().mkString("\n")).act(tk)
  
     }
   }
   
   private def getPresentationResponse : HLet = new HLet {
     def act(tk : HTalk) = try {
       val reqS = bodyAsString(tk)
       val params = JSON.parseRaw(reqS) match {
         case Some(j : JSONObject) => j.obj
         case _ => throw ServerError("Invalid JSON " + reqS)
       }
       val bodyS = params.get("body").getOrElse(throw ServerError("No Body Found")).toString
       val dpathS = params.get("dpath").getOrElse(throw ServerError("No dpath Found")).toString
       println(dpathS)
       val dpath = DPath(utils.URI(dpathS))
       val styleS = params.get("style").getOrElse("xml").toString
       val presenter = controller.extman.getPresenter(styleS) getOrElse {
         val nset = Path.parseM(styleS, controller.getBase)
         new StyleBasedPresenter(controller, nset)
       }
       val reader = new XMLReader(controller)
       println("bodyS: " + bodyS)
       println("dpathS: " + dpathS)
       
       val bodyXML  = scala.xml.Utility.trim(scala.xml.XML.loadString(bodyS))
       val cont = new Controller
       reader.readDocument(dpath, bodyXML)(cont.add)
       val doc : Document = cont.getDocument(dpath, dp => "doc not found at path " + dp)
       val rb = new XMLBuilder()
       presenter.apply(doc, rb)
       val response = rb.get()
       /*
       println(bodyXML)
       println("\n\n\n")
       println(doc.toNode)
       println("\n\n\n")
       println(response)
       */
       TextResponse(response.toString).act(tk)
     } catch {
        case e : Error => 
         log(e.msg)
         errorResponse(e.shortMsg).act(tk)
       case e : Exception => 
         errorResponse("Exception occured : " + e.getMessage() + e.getStackTrace().mkString("\n")).act(tk)
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
     try {
       getChildren(p) match {
         case Nil => List(p.last + " " + p.toString, "")
         case l => (p.last + " " + p.toString) :: l.map(getDescendants).flatten ::: List("")
       }
     } catch {
       case x : Error => Nil //if MMT Error silently ignore
       case x : Throwable => Nil // also silently ignore
     }
   }
   
   private def getChildren(p : Path) : List[Path] = {
     p match {
       case d : DPath => 
         controller.get(d).components collect {
          case ref : XRef => ref.target
         }
       case _ => Nil
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