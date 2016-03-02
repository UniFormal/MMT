package info.kwarc.mmt.api.ontology

import scala.Option.option2Iterable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.matching.Regex
import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.Error
import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.SourceError
import info.kwarc.mmt.api.StructuralElement
//import info.kwarc.mmt.api.backend.XMLReader
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Logger
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.informal.IRels._
import info.kwarc.mmt.api.parser
import info.kwarc.mmt.api.presentation.StringBuilder
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.utils
import info.kwarc.mmt.api.web.Body
import info.kwarc.mmt.api.web.Server
import info.kwarc.mmt.api.web.ServerError
import info.kwarc.mmt.api.web.ServerExtension
import tiscaf.HLet
import info.kwarc.mmt.api._
import tiscaf.HTalk
import info.kwarc.mmt.api.objects._
import java.io.ByteArrayInputStream
import java.io.IOException
import java.io.ObjectInputStream
import scala.collection.mutable.HashMap
import scala.collection.Map
import scala.collection.JavaConversions._
import java.net.URLDecoder
import info.kwarc.mmt.api.utils._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.io.Source


case class PlanetaryError(val text: String) extends Error(text)

case class Alignment(alignmentType: String, from: String, to: String)
//class Alignment extends ServerExtension("align") {
//  private def alignments  = new HashSet
//  override def start(//path to alignments) {
//    // read the alignments 
//  }
//  // serve queries about alignments
//}

class AlignmentsServer extends ServerExtension("align") with Logger {

  override val logPrefix = "align"
  
  private var alignments = HashSet[Alignment]()
  
  override def start(args:List[String]/*path to alignments*/) {
    //read the alignments
    val jsonString = Source.fromFile(args(0))
                           .getLines()
                           .mkString(" ")
    val jsonObj: JSONObject = JSON.parse(jsonString) match {
      case obj: JSONObject => obj
      case _ => JSONObject()
    }
    val alignmentListByType = jsonObj.toList
    alignmentListByType foreach {
      case (jsonstring:JSONString, alignmentList:JSONArray) =>
        alignmentList foreach {
          case alignmentObject: JSONObject =>
            val alignmentMap = alignmentObject.toList.toMap
            alignments += Alignment(jsonstring.toString,
                          alignmentMap(JSONString("from")).toString,
                          alignmentMap(JSONString("to")).toString)
        }
      case _ =>
    }
    
    alignments foreach {
      case Alignment(alignmentType, from, to) =>
        println (List(alignmentType, from, to).mkString(","))
    }
  }

  /** Server */

  def apply(uriComps: List[String], query: String, body: Body): HLet = {
    try {
      uriComps match {
        //Here the post request is handled
        case "getAlignments" :: _ => getAlignmentsResponse
        case _                    => errorResponse("Invalid request: " + uriComps.mkString("/"), List(new PlanetaryError("Invalid Request" + uriComps)))
      }
    } catch {
      case e: Error =>
        log(e.shortMsg)
        errorResponse(e.shortMsg, List(e))
      case e: Exception =>
        errorResponse("Exception occured : " + e.getStackTrace(), List(e))
    }
  }

  //The post request response is defined here 
  def getAlignmentsResponse: HLet = new HLet {
    def aact(tk: HTalk)(implicit ec: ExecutionContext): Future[Unit] = {
      val reqBody = new Body(tk)
      val resp = info.kwarc.mmt.api.utils.JSONArray()
      //    val params = reqBody.asJSON
      Server.JsonResponse(resp).aact(tk)
    }
  }


// utils
  private def errorResponse(text : String, errors : List[Throwable]) : HLet = {
    JsonResponse("", s"MMT Error in Planetary extension: $text ", errors)
  }
  
  private def JsonResponse(content : String, info : String, errors : List[Throwable]) : HLet = {
    val response : HashMap[String, JSON] = new collection.mutable.HashMap[String, JSON]()
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

