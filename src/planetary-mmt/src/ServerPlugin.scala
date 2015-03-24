package info.kwarc.mmt.planetary

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.flexiformal._
import info.kwarc.mmt.stex._
import symbols.{Constant}

import utils._
//import scala.util.parsing.json._
import tiscaf._
import scala.concurrent._

case class PlanetaryError(val text : String) extends Error(text)


class PlanetaryPlugin extends ServerExtension("planetary") with Logger {
  
  override val logPrefix = "planetary"
     /** Server */   
  def apply(uriComps: List[String], query: String, body : Body): HLet = {
    try {
      uriComps match {
        case "getPresentation" :: _ => getPresentationResponse
        case "getCompiled" :: _ => getCompiledResponse
        case "getRelated" :: _ => getRelated
        case "getNotations" :: _ => getNotations
        case "getDefinitions" :: _ => getDefinitions
        case "generateGlossary" :: _ => generateGlossary
        case _ => errorResponse("Invalid request: " + uriComps.mkString("/"), List(new PlanetaryError("Invalid Request" + uriComps)))
       }
    } catch {
      case e : Error => 
        log(e.shortMsg) 
        errorResponse(e.shortMsg, List(e))
      case e : Exception => 
        errorResponse("Exception occured : " + e.getStackTrace(), List(e))
    }
  }
  
  def getNotations : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
      val spathS = params.get("spath").getOrElse(throw ServerError("No spath found")).toString
      val languageO = params.get("language").map(_.toString)
      val dimensionO = params.get("dimension").map(_.toString)
      
      val spath = Path.parse(spathS)
      controller.get(spath) match {
        case c : Constant =>
          var notations = dimensionO match {
            case None => c.notC.getAllNotations
            case Some("parsing") => c.notC.parsingDim.notations.values.flatten
            case Some("presentation") => c.notC.presentationDim.notations.values.flatten
            case Some("verbalization") => c.notC.verbalizationDim.notations.values.flatten
            case Some(s) => throw ServerError("Invalid notation dimension: '" + s  + "'. Expected parsing, presentation or verbalization")
          }
          
          notations = languageO match {
            case None => notations
            case Some(lang) => notations.filter(_.scope.languages.contains(lang))
          }
          Server.JsonResponse(JSONArray(notations.map(n => JSONArray(toStringMarkers(n).map(s => JSONString(s)) : _*)).toSeq :_*)).aact(tk)
        case x => throw ServerError("Expected path pointing to constant, found :" + x.getClass())
      }
    }
  }
  
  private def toStringMarkers(not : TextNotation) : List[String] = {
   not.parsingMarkers flatMap {
      case a : Arg => Some("_")
      case a : SeqArg => Some("_...")
      case a : ImplicitArg => None
      case d : Delimiter => Some(d.text)
      case v : Var => Some("_")
      case _ => None
    }
  }
  
  private def getDefinitions : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
      val spathS = params.get("spath").getOrElse(throw ServerError("No spath found")).toString
      val languageO = params.get("language").map(_.toString)
      val spath = Path.parse(spathS)
      var resultSet = controller.depstore.getObjects(spath, isDefinedBy) collect { 
        case p if p.isPath => p.path
      }
      resultSet = languageO match {
        case None => resultSet
        case Some(_) => resultSet.filter(p => sTeX.getLanguage(p) == languageO)
      }
      
      //presenting
      val pres = controller.extman.getPresenter("planetary").getOrElse(throw ServerError("No presenter found"))
      val resultNodes = resultSet flatMap {p => 
        controller.get(p) match {
          case s : StructuralElement =>
            val rb = new presentation.StringBuilder
            pres(s)(rb)
            Some(rb.get)
          case _ => None
        }
      }
      
      Server.JsonResponse(JSONArray(resultNodes.map(s => JSONString(s)).toSeq :_*)).aact(tk)
    }
  }
  
  private def getRelated : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
      log("Received immt query request : " + params.toString)
      val subjectS = params.get("subject").getOrElse(throw ServerError("No subject found")).toString
      val relationS = params.get("relation").getOrElse(throw ServerError("No relation found")).toString
      val returnS = params.get("return").getOrElse(throw ServerError("No return type found")).toString
      val subject = Path.parse(subjectS)
      val relation = Binary.parse(relationS)
      log(subjectS + " " +  relationS + " " +returnS)
      val resultSet = controller.depstore.getObjects(subject, relation)
      val pres = controller.extman.getPresenter("planetary").getOrElse(throw ServerError("No presenter found"))
      val rb = new presentation.StringBuilder
      val resultNodes = new collection.mutable.HashMap[String,String]
      resultSet foreach {p =>
        controller.get(p) match {
          case s : StructuralElement =>
            val rb = new presentation.StringBuilder
            pres(s)(rb)
            val lang = sTeX.getLanguage(p.path).getOrElse("all")
            resultNodes(lang) = rb.get
          case _ => 
        }
      }
      
      val sb = new StringBuilder
      sb.write("<ul class=\"nav nav-tabs\" role=\"tablist\">")
      resultNodes.keys.zipWithIndex foreach { p => 
        sb.write(<li class={if (p._2 == 0) "active" else ""}><a data-target={ "#" + p._1 } style="cursor: pointer;" onclick={ "$(this).tab('show');" }> { p._1 } </a></li>.toString)
      }
      sb.write("</ul>")
      
            
      sb.write("<div class=\"tab-content\">")
      resultNodes.zipWithIndex foreach { p => 
        sb.write("<div class=\"tab-pane ")
        sb.write((if (p._2 == 0) "active" else ""))
        sb.write("\"")
        sb.write(" id=\"" + p._1._1 + "\">")
        sb.write(p._1._2)
        sb.write("</div>")
      }
      sb.write("</div>")
      
      
      val html = if (resultSet.isEmpty) {
        "<div class=\"error\"> No Results Found </div>" 
      } else {
         "<div>" + sb.get + "</div>"
      }
      
      
      
      val response_header = <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">{subject.last}</h4>
        </div>
      val response = response_header.toString + 
        "<div class=\"modal-body\" style=\"overflow:auto\"> " + html + " </div>"
      log("Sending Response: " + response)
      Server.XmlResponse(response).aact(tk)
    }
  }
  
  private def getCompiledResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
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
          val is = new java.io.InputStreamReader(new java.io.ByteArrayInputStream(bodyS.getBytes()))
          val ps = new parser.ParsingStream(utils.URI("tmp"),dpath, new java.io.BufferedReader(is))
          val doc = controller.textParser(ps)(new ErrorLogger(report))
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
        log(e.shortMsg)
        errorResponse(e.shortMsg, List(e)).aact(tk)
      case e : Exception => 
        errorResponse("Exception occured : " + e.getStackTrace().mkString("\n"), List(e)).aact(tk)
    }
  }
  
  private def generateGlossary : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val location = utils.File("/var/data/localmh/MathHub/glossary.html")
      val glossary = GlossaryGenerator.generate(controller)
      utils.File.write(location, glossary)
      Server.TextResponse("Success").aact(tk)
    } catch {
      case e : Exception => Server.TextResponse(e.getMessage() + "\n" + e.getStackTrace.mkString("\n")).aact(tk)
    }
  }
  
  
  private def getPresentationResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
      log("Received Presentation Request : " + params.toString)
      val bodyS = params.get("body").getOrElse(throw ServerError("No Body Found")).toString
      val dpathS = params.getOrElse("dpath","/tmp/").toString
      val dpath = DPath(utils.URI(dpathS))
      val styleS = params.get("style").getOrElse("xml").toString
      val presenter = controller.extman.getPresenter(styleS) getOrElse {
        throw ServerError("no presenter found")
      }
      val reader = new XMLReader(controller.report)
      val bodyXML = scala.xml.XML.loadString(bodyS)
      
      val cont = controller //new Controller
      reader.readDocument(dpath, bodyXML)(cont.add)
      val doc : Document = cont.getDocument(dpath, dp => "doc not found at path " + dp)
      val rb = new StringBuilder()
      presenter.apply(doc)(rb)
      val response = rb.get
      log("Sending Response: " + response)
      //val additional = GlossaryGenerator.generate(controller)
      //JsonResponse(response + additional, "", Nil).aact(tk)
      JsonResponse(response, "", Nil).aact(tk)
      
     } catch {
       case e : Error => 
         log(e.shortMsg)
         errorResponse(e.shortMsg, List(e)).aact(tk)
       case e : Exception => 
         errorResponse("Exception occured : "  + e.getStackTrace().mkString("\n"), List(e)).aact(tk)
    }
  }
  
  //utils
  private def errorResponse(text : String, errors : List[Throwable]) : HLet = {
    JsonResponse("", s"MMT Error in Planetary extension: $text ", errors)
  }
  
  private def JsonResponse(content : String, info : String, errors : List[Throwable]) : HLet = {
    val response = new collection.mutable.HashMap[String, JSON]()
    response("content") = JSONString(content)
    if (errors == Nil) { //no errors
      val status = new collection.mutable.HashMap[String, JSON]()
      status("conversion") = JSONInt(0) //success
      val messages = new collection.mutable.HashMap[String, JSON]()
      if (info != "") {
        val message = new collection.mutable.HashMap[String, JSON]()
        message("type") = JSONString("Info")
        message("shortMsg") = JSONString(info)
        message("longMsg") = JSONString(info)
        //no srcref
        messages("0") = JSONObject(message.toSeq : _*)
      }
      status("messages") = JSONObject(messages.toSeq : _*)
      response("status") = JSONObject(status.toSeq : _*)        
    } else { //there are errors
      val status = new collection.mutable.HashMap[String, JSON]()
      if (content == "") {
        status("conversion") = JSONInt(2) //failed with errors
      } else {
        status("conversion") = JSONInt(2) //success with errors
      }
      val messages = new collection.mutable.HashMap[String, JSON]()
      errors.zipWithIndex foreach { p => 
        val message = new collection.mutable.HashMap[String, JSON]()
        p._1 match {
          case se : SourceError =>
            message("type") = JSONString("Fatal")
            message("shortMsg") = JSONString(se.mainMessage)
            message("longMsg") = JSONString(se.getStackTrace.mkString("\n"))
            message("srcref") = JSONObject(List("from" -> JSONObject(List("line" -> JSONInt(se.ref.region.start.line), "col"-> JSONInt(se.ref.region.start.column)) : _*), 
                                 "to" -> JSONObject(List("line" -> JSONInt(se.ref.region.end.line), "col" -> JSONInt(se.ref.region.end.column)) : _*)) : _*)
          case e =>
            message("type") = JSONString("Fatal")
            message("shortMsg") = JSONString(e.getMessage)
            message("longMsg") = JSONString(e.getStackTrace.mkString("\n"))
            //no srcref :(
          }
          messages(p._2.toString) = JSONObject(message.toSeq : _*)
      }
      status("messages") = JSONObject(messages.toSeq : _*)
      response("status") = JSONObject(status.toSeq : _*)
    }
      log("Sending Response: " + response)
      Server.JsonResponse(JSONObject(response.toSeq : _*))     
  }
}