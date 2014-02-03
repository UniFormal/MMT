package info.kwarc.mmt.planetary

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.stex._

import scala.util.parsing.json._
import tiscaf._
import scala.concurrent._

case class PlanetaryError(val text : String) extends Error(text)


class PlanetaryPlugin extends ServerExtension("planetary") with Logger {
  
  override val logPrefix = "planetary"
     /** Server */   
   def apply(uriComps: List[String], query: String, body : Body): HLet = {
     try {
       uriComps match {
         case "getPaths" :: _ => getPathsResponse
         case "getPresentation" :: _ => getPresentationResponse
         case "getCompiled" :: _ => getCompiledResponse
         case "getContentPres" :: _ => getContentPres
         case _ => errorResponse("Invalid request: " + uriComps.mkString("/"), List(new PlanetaryError("Invalid Request")))
       }
     } catch {
       case e : Error => 
         log(e.longMsg) 
         errorResponse(e.longMsg, List(e))
       case e : Exception => 
         errorResponse("Exception occured : " + e.getStackTrace(), List(e))
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
   
   
   private def getContentPres : HLet = new HLet {
     def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
       val reqS = bodyAsString(tk)
       val params = JSON.parseRaw(reqS) match {
         case Some(j : JSONObject) => j.obj
         case _ => throw ServerError("Invalid JSON " + reqS)
       }
       log("Received ContentPres Request : " + params.toString)

       val pathS = params.get("path").getOrElse(throw ServerError("No path found")).toString       
       val styleS = params.get("style").getOrElse("xml").toString
       val presenter = controller.extman.getPresenter(styleS) getOrElse {
         val nset = Path.parseM(styleS, controller.getBase)
         new StyleBasedPresenter(controller, nset)
       }
       val path = Path.parse(pathS)
       val elem = controller.get(path)
       val rb = new StringBuilder()
       presenter.apply(elem)(rb)
       val response = rb.get
       
       log("Sending Response: " + response)
       TextResponse(response).aact(tk)

     }
   }
   
   private def getCompiledResponse : HLet = new HLet {
     def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
       val reqS = bodyAsString(tk)
       val params = JSON.parseRaw(reqS) match {
         case Some(j : JSONObject) => j.obj
         case _ => throw ServerError("Invalid JSON " + reqS)
       }
       log("Received Compilation Request : " + params.toString)
       val bodyS = params.get("body").getOrElse(throw ServerError("No Body Found")).toString
       val dpathS = params.getOrElse("dpath", "/tmp/").toString
       val format = params.get("format").getOrElse(throw ServerError("No dpath Found")).toString     
       val dpath = DPath(utils.URI(dpathS))
       format match {
         case "stex" => 
          val comp = new STeXImporter()
          comp.init(controller)
          val (response,errors) = comp.compileOne(bodyS, dpath)
          JsonResponse(response, errors.map(e => e.getStackTrace().mkString("\n")).mkString("\n\n"), errors).aact(tk)
         case "mmt" => 
          val reader = parser.Reader(bodyS)
          val (doc,state) = controller.textParser(reader, dpath) 
          val response = doc.toNodeResolved(controller.memory.content).toString    
          JsonResponse(response, "", Nil).aact(tk)
         case "elf" => 
           //val comp = new Twelf()
           //comp.init(controller, List("/home/mihnea/kwarc/data/twelf-mod/bin/twelf-server")) //TODO take from extension list
           //val (response,errors) = comp.compileOne(bodyS, dpath)       
           val response = controller.get(dpath).toNode.toString
           val errors : List[Error] = Nil
           JsonResponse(response, errors.map(e => e.getStackTrace().mkString("\n")).mkString("\n\n"), errors).aact(tk)
       }
     } catch {
       case e : Error => 
         log(e.longMsg)
         errorResponse(e.longMsg, List(e)).aact(tk)
       case e : Exception => 
         errorResponse("Exception occured : " + e.getStackTrace().mkString("\n"), List(e)).aact(tk)
     }
   }
   
   private def getPresentationResponse : HLet = new HLet {
     def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
       val reqS = bodyAsString(tk)
       val params = JSON.parseRaw(reqS) match {
         case Some(j : JSONObject) => j.obj
         case _ => throw ServerError("Invalid JSON " + reqS)
       }
       log("Received Presentation Request : " + params.toString)

       val bodyS = params.get("body").getOrElse(throw ServerError("No Body Found")).toString
       val dpathS = params.getOrElse("dpath","/tmp/").toString
       val dpath = DPath(utils.URI(dpathS))
       val styleS = params.get("style").getOrElse("xml").toString
       val presenter = controller.extman.getPresenter(styleS) getOrElse {
         val nset = Path.parseM(styleS, controller.getBase)
         val p = new StyleBasedPresenter(controller, nset)
         p.expandXRefs = true
         p
       }
       val reader = new XMLReader(controller.report)
       val bodyXML = scala.xml.Utility.trim(scala.xml.XML.loadString(bodyS))
       
       val cont = controller //new Controller
       reader.readDocument(dpath, bodyXML)(cont.add)
       val doc : Document = cont.getDocument(dpath, dp => "doc not found at path " + dp)
       val rb = new StringBuilder()
       presenter.apply(doc)(rb)
       val response = rb.get
       log("Sending Response: " + response)
       JsonResponse(response, "", Nil).aact(tk)
     } catch {
        case e : Error => 
         log(e.longMsg)
         errorResponse(e.longMsg, List(e)).aact(tk)
       case e : Exception => 
         errorResponse("Exception occured : "  + e.getStackTrace().mkString("\n"), List(e)).aact(tk)
     }
   }
   
   private def getPathsResponse : HLet = new HLet {
     def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {        
       val archives = controller.backend.getArchives map {a => DPath(a.narrationBase)}
       val pathList = archives.flatMap(getContentPaths)
       val lines = pathList map { p => 
         p.last + " " + p.toPath         
       }
       val response = lines.mkString("\n")
       TextResponse(response).aact(tk)
     } catch {
       case e : Error => 
         log(e.longMsg)
         errorResponse(e.shortMsg, List(e)).aact(tk)
       case e : Exception => 
         errorResponse("Exception occured : " + e.getStackTrace(), List(e)).aact(tk)
     }
   }
   
   private def getContentPaths(p : Path) : List[MPath] = {
     try {
       val paths = controller.get(p).components collect {
         case ref : DRef => getContentPaths(ref.target)
         case ref : MRef => ref.target :: Nil
       }
       paths.flatten
     } catch {
       case x : Error => Nil //if MMT Error silently ignore
       case x : Throwable => Nil // also silently ignore
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
  
  private def errorResponse(text : String, errors : List[Throwable]) : HSimpleLet = {
    JsonResponse("", s"MMT Error in Planetary extension: $text ", errors)
  }
  
  private def JsonResponse(content : String, info : String, errors : List[Throwable]) : HSimpleLet = {
      val response = new collection.mutable.HashMap[String, Any]()
      response("content") = content
      if (errors == Nil) { //no errors
        val status = new collection.mutable.HashMap[String, Any]()
        status("conversion") = 0 //success
        val messages = new collection.mutable.HashMap[String, Any]()
        if (info != "") {
          val message = new collection.mutable.HashMap[String, Any]()
          message("type") = "Info"
          message("shortMsg") = info
          message("longMsg") = info
          //no srcref
          messages("0") = JSONObject(message.toMap)
        }
        status("messages") = JSONObject(messages.toMap)
        response("status") = JSONObject(status.toMap)        
      } else {
        val status = new collection.mutable.HashMap[String, Any]()
        status("conversion") = 2 //failed with errors
        val messages = new collection.mutable.HashMap[String, Any]()

        errors.zipWithIndex foreach { p => 
          val message = new collection.mutable.HashMap[String, Any]()
          p._1 match {
            case se : SourceError =>
              message("type") = "Error"
              message("shortMsg") = se.mainMessage
              message("longMsg") = se.getStackTraceString
              message("srcref") = JSONObject(List("from" -> JSONObject(List("line" -> se.ref.region.start.line, "col" -> se.ref.region.start.column).toMap), 
                                   "to" -> JSONObject(List("line" -> se.ref.region.end.line, "col" -> se.ref.region.end.column).toMap)).toMap)
                
            case e =>
              message("type") = "Error"
              message("shortMsg") = e.getMessage
              message("longMsg") = e.getStackTraceString
              //no srcref :(
          }
          messages(p._2.toString) = JSONObject(message.toMap)
        }
        status("messages") = JSONObject(messages.toMap)
        response("status") = JSONObject(status.toMap)
      }
      log("Sending Response: " + response)
      JsonResponse(JSONObject(response.toMap))     
  }
  
  /**
   * A text response that the server sends back to the browser
   * @param text the message that is sent in the HTTP body
   */
  private def TextResponse(text: String): HSimpleLet = new HSimpleLet {    
    def act(tk: HTalk) {
      val out = text.getBytes("UTF-8")
      checkCORS(tk).setContentLength(out.size) // if not buffered
        .setContentType("text/plain; charset=utf8")
        .write(out)
    }
  }
  
  /**
   * A Json response that the server sends back to the browser
   * @param json the Json message that is sent in the HTTP body
   */
  def JsonResponse(json: JSONType): HSimpleLet = new HSimpleLet {
    def act(tk: HTalk) {
      val out: Array[Byte] = json.toString.getBytes("UTF-8")
      checkCORS(tk).setContentLength(out.size) // if not buffered
        .setContentType("application/json; charset=utf8")
        .write(out)
    }
  }
  
}