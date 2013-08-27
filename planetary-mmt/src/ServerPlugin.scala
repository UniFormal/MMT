package info.kwarc.mmt.planetary

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.stex._
import info.kwarc.mmt.lf._


import scala.util.parsing.json._
import tiscaf._
import scala.concurrent._


case class PlanetaryError(val text : String) extends Error(text)


class PlanetaryPlugin extends ServerPlugin("planetary") with Logger {
  
  override val logPrefix = "planetary"

    
     /** Server */   

   
   def apply(uriComps: List[String], query: String, body : Body): HLet = {
     try {
       log("got here" + uriComps)
       uriComps match {
         case "getArchives" :: _ => getArchivesResponse
         case "getPaths" :: _ => getPathsResponse
         case "getPresentation" :: _ => getPresentationResponse
         case "getCompiled" :: _ => getCompiledResponse
         case "getContentPres" :: _ => getContentPres
         case _ => errorResponse("Invalid request: " + uriComps.mkString("/"))
       }
     } catch {
       case e : Error => 
         log(e.longMsg) 
         errorResponse(e.longMsg)
       case e : Exception => 
         errorResponse("Exception occured : " + e.getMessage())
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
     def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
        val archives = controller.backend.getArchives map { a => (a.id, DPath(a.narrationBase))}
        val result = archives.map(p =>  p._1 + " " +  p._2).mkString("\n")
        log("getArchives returning: " + result)
        TextResponse(result).aact(tk)
     } catch {
       case e : Error => 
         log(e.longMsg)
         errorResponse(e.longMsg).aact(tk)
       case e : Exception => 
         errorResponse("Exception occured : " + e.getMessage()).aact(tk)
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
       val rb = new XMLBuilder()
       presenter.apply(elem, rb)
       val response = rb.get()
       
       
       log("Sending Response: " + response)
       TextResponse(response.toString).aact(tk)

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
          comp.init(controller, Nil)
          val (response,errors) = comp.compileOne(bodyS, dpath)
          JsonResponse(response, errors.mkString("\n")).aact(tk)
         case "mmt" => 
          val reader = parser.Reader(bodyS)
          val (doc,state) = controller.textParser(reader, dpath)
          
          val response = doc.toNodeResolved(controller.memory.content).toString    
          println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

          JsonResponse(response, "Success").aact(tk)
         case "elf" => 
           //val comp = new Twelf()
           //comp.init(controller, List("/home/mihnea/kwarc/data/twelf-mod/bin/twelf-server")) //TODO take from extension list
           //val (response,errors) = comp.compileOne(bodyS, dpath)
           
           val response = controller.get(dpath).toNode.toString
           val errors = Nil
           JsonResponse(response, errors.mkString("\n")).aact(tk)
       }
     } catch {
       case e : Error => 
         log(e.longMsg)
         errorResponse(e.longMsg).aact(tk)
       case e : Exception => 
         errorResponse("Exception occured : " + e.getMessage() + e.getStackTrace().mkString("\n")).aact(tk)
  
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
       val reader = new XMLReader(controller)
       val bodyXML  = scala.xml.Utility.trim(scala.xml.XML.loadString(bodyS))
       
       val cont = new Controller
       reader.readDocument(dpath, bodyXML)(cont.add)
       val doc : Document = cont.getDocument(dpath, dp => "doc not found at path " + dp)
       val rb = new XMLBuilder()
       presenter.apply(doc, rb)
       val response = rb.get()
       log("Sending Response: " + response)
       JsonResponse(response.toString, "Success").aact(tk)
     } catch {
        case e : Error => 
         log(e.longMsg)
         errorResponse(e.longMsg).aact(tk)
       case e : Exception => 
         errorResponse("Exception occured : " + e.getMessage() + e.getStackTrace().mkString("\n")).aact(tk)
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
        /* //old implementation, still around for documentation for now, to be removed soon
         
        val archives = controller.backend.getArchives map { a => (a.id, DPath(a.narrationBase))}
        val result = archives.map(p => ((p._1 + " " + p._2.toPath) :: getDescendants(p._2).tail).mkString("\n")).mkString("\n")
        log("getPaths returning: " + result)
        TextResponse(result).act(tk)
        * 
        */
     } catch {
       case e : Error => 
         log(e.longMsg)
         errorResponse(e.shortMsg).aact(tk)
       case e : Exception => 
         errorResponse("Exception occured : " + e.getMessage()).aact(tk)
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
  
  private def errorResponse(text : String) : HSimpleLet = {
    JsonResponse("", s"MMT Error in Planetary extension: $text ")
  }
  
  private def JsonResponse(content : String, errors : String) : HSimpleLet = {
      val response = new collection.mutable.HashMap[String, Any]()
      response("content") = content
      response("log") = errors
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