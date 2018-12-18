package info.kwarc.mmt.planetary

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.informal._
import info.kwarc.mmt.stex._
import parser._
import symbols.{Constant}

import utils._
//import scala.util.parsing.json._
import scala.concurrent._

case class PlanetaryError(val text : String) extends Error(text)


class PlanetaryPlugin extends ServerExtension("planetary") with Logger {

  override val logPrefix = "planetary"
     /** Server */
  def apply(request: ServerRequest): ServerResponse = {
    lazy val json = request.body.asJSON match {
      case j: JSONObject => j
      case _ => throw ServerError("body must be json object")
    }
    try {
      request.path match {
        case "getPresentation" :: _ => getPresentationResponse(json)
//        case "getCompiled" :: _ => getCompiledResponse
        case "getRelated" :: _ => getRelated(json)
        case "getNotations" :: _ => getNotations(json)
        case "getDefinitions" :: _ => getDefinitions(json)
        case "generateGlossary" :: _ => generateGlossary
        case _ => errorResponse("Invalid request: " + request.path.mkString("/"), List(new PlanetaryError("Invalid Request" + request.path)))
       }
    } catch {
      case e : Error =>
        log(e.shortMsg)
        errorResponse(e.shortMsg, List(e))
      case e : Exception =>
        errorResponse("Exception occured : " + e.getStackTrace(), List(e))
    }
  }

  def getNotations(params: JSONObject) = {
      val spathS = params("spath").getOrElse(throw ServerError("No spath found")).toString
      val languageO = params("language").map(_.toString)
      val dimensionO = params("dimension").map(_.toString)

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
          ServerResponse.JsonResponse(JSONArray(notations.map(n => JSONArray(toStringMarkers(n).map(s => JSONString(s)) : _*)).toSeq :_*))
        case x => throw ServerError("Expected path pointing to constant, found :" + x.getClass())
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

  private def getDefinitions(params: JSONObject) = {
      val spathS = params("spath").getOrElse(throw ServerError("No spath found")).toString
      val languageO = params("language").map(_.toString)
      val spath = Path.parse(spathS)
      var resultSet = controller.depstore.queryList(spath, ToObject(IRels.isDefinedBy))
      resultSet = languageO match {
        case None => resultSet
        case Some(_) => resultSet.filter(p => sTeX.getLanguage(p) == languageO)
      }

      //presenting
      val pres = controller.extman.get(classOf[Presenter]).find(_.isApplicable("planetary")).getOrElse(throw ServerError("No presenter found"))
      val resultNodes = resultSet flatMap {p =>
        controller.get(p) match {
          case s : StructuralElement =>
            val rb = new presentation.StringBuilder
            pres(s)(rb)
            Some(rb.get)
          case _ => None
        }
      }
      ServerResponse.JsonResponse(JSONArray(resultNodes.map(s => JSONString(s)).toSeq :_*))
  }

  private def getRelated(params: JSONObject) = {
      log("Received immt query request : " + params.toString)
      val subjectS = params("subject").getOrElse(throw ServerError("No subject found")).toString
      val relationS = params("relation").getOrElse(throw ServerError("No relation found")).toString
      val returnS = params("return").getOrElse(throw ServerError("No return type found")).toString
      val subject = Path.parse(subjectS)
      val relation = controller.relman.parseBinary(relationS)
      log(subjectS + " " +  relationS + " " +returnS)
      val resultSet = controller.depstore.queryList(subject, ToObject(relation))
      val pres = controller.extman.get(classOf[Presenter]).find(_.isApplicable("planetary")).getOrElse(throw ServerError("No presenter found"))
      val rb = new presentation.StringBuilder
      val resultNodes = new collection.mutable.HashMap[String,String]
      resultSet foreach {p =>
        controller.get(p) match {
          case s : StructuralElement =>
            val rb = new presentation.StringBuilder
            pres(s)(rb)
            val lang = sTeX.getLanguage(p).getOrElse("all")
            resultNodes(lang) = rb.get
          case _ =>
        }
      }

      val sb = new StringBuilder
      sb.write("<results data-subject=\"" + subject.last + "\">")
      resultNodes.zipWithIndex foreach { p =>
        sb.write("<result data-lang=\"" + p._1._1 + "\">")
        sb.write(p._1._2)
        sb.write("</result>")
      }
      sb.write("</results>")

      val response = sb.get

      log("Sending Response: " + response)
      ServerResponse.XmlResponse(response)
  }

  /*
  private def getCompiledResponse : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val params = bodyAsJSON(reqBody).obj
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
  */

  private def generateGlossary = {
    try {
      val location = utils.File("/var/data/localmh/MathHub/glossary.html")
      val glossary = GlossaryGenerator.generate(controller)
      utils.File.write(location, glossary)
      ServerResponse.TextResponse("Success")
    } catch {
      case e : Exception => ServerResponse.TextResponse(e.getMessage() + "\n" + e.getStackTrace.mkString("\n"))
    }
  }


  private def getPresentationResponse(params: JSONObject) = {
    try {
      log("Received Presentation Request : " + params.toString)
      val bodyS = params("body").getOrElse(throw ServerError("No Body Found")).toString
      val dpathS = params("dpath").getOrElse("/tmp/").toString
      val dpath = DPath(utils.URI(dpathS))
      val styleS = params("style").getOrElse("xml").toString
      val presenter = controller.extman.get(classOf[Presenter]).find(_.isApplicable(styleS)) getOrElse {
        throw ServerError("no presenter found")
      }
      val reader = new XMLReader(controller)
      val bodyXML = scala.xml.XML.loadString(bodyS)
      val cont = new StructureParserContinuations(ErrorThrower) {
        override def onElement(se: StructuralElement) {
          controller.add(se)
        }
      }
      reader.readDocument(dpath, bodyXML)(cont)
      val doc : Document = controller.getDocument(dpath, dp => "doc not found at path " + dp)
      val rb = new StringBuilder()
      presenter.apply(doc)(rb)
      val response = rb.get
      log("Sending Response: " + response)
      //val additional = GlossaryGenerator.generate(controller)
      //JsonResponse(response + additional, "", Nil).aact(tk)
      JsonResponse(response, "", Nil)

     } catch {
       case e : Error =>
         log(e.shortMsg)
         errorResponse(e.shortMsg, List(e))
       case e : Exception =>
         errorResponse("Exception occured : "  + e.getStackTrace().mkString("\n"), List(e))
    }
  }

  //utils
  private def errorResponse(text : String, errors : List[Throwable]) : ServerResponse = {
    JsonResponse("", s"MMT Error in Planetary extension: $text ", errors)
  }

  private def JsonResponse(content : String, info : String, errors : List[Throwable]) : ServerResponse = {
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
    ServerResponse.JsonResponse(JSONObject(response.toSeq : _*))
  }
}
