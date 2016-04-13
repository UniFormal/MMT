package info.kwarc.mmt.marpa

import scala.Option.option2Iterable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.matching.Regex
import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.Error
import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.SourceError
import info.kwarc.mmt.api.StructuralElement
import info.kwarc.mmt.api.backend.XMLReader
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.Logger
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.notations.Arg
import info.kwarc.mmt.api.notations.Delimiter
import info.kwarc.mmt.api.notations.GroupMarker
import info.kwarc.mmt.api.notations.ImplicitArg
import info.kwarc.mmt.api.notations.Marker
import info.kwarc.mmt.api.notations.SeqArg
import info.kwarc.mmt.api.notations.TdMarker
import info.kwarc.mmt.api.notations.TextNotation
import info.kwarc.mmt.api.notations.PlaceholderDelimiter
import info.kwarc.mmt.api.notations.SymbolName
import info.kwarc.mmt.api.notations.InstanceName
import info.kwarc.mmt.api.notations.Var
import info.kwarc.mmt.api.ontology.Binary
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
import info.kwarc.mmt.stex.STeXImporter
import info.kwarc.mmt.stex.sTeX
import tiscaf.HLet
import info.kwarc.mmt.api._
import tiscaf.HTalk
import info.kwarc.mmt.api.objects._
import scala.collection.mutable.HashMap
import scala.collection.Map
import java.net.URLDecoder
import scala.collection.mutable.ListBuffer
import scalaj.http._
import net.liftweb.json._

abstract class ParseTree
case class Variants(treeList: List[ParseTree]) extends ParseTree
case class Notation(name: String, arguments: List[Variants], before: String, after: String) extends ParseTree
case class Argument(name: String, value: Variants) extends ParseTree
case class SeqArgument(name: String, value: List[Variants]) extends ParseTree
case class RawString(value: String) extends ParseTree

case class SemanticTreeError(message: String) extends Exception

object SemanticTree {
  val url = "http://localhost:3000"
  val detectNotation = url + "/detect_notations"

  def getSemanticTree: HLet = new HLet {
    def aact(tk: HTalk)(implicit ec: ExecutionContext): Future[Unit] = {
      println("-->getSemanticTree")
      val reqBody = new Body(tk)
      val input: String = reqBody.asString
      println("input = " + input)

      // inputLength is taken from perl, to avoid encoding issues
      val notations = sendGetNotationPosRequest(input)
      println(notations)

      val resp = info.kwarc.mmt.api.utils.JSONString("")
      tk.setHeader("Access-Control-Allow-Origin", "*")
      Server.JsonResponse(resp).aact(tk)
    }
  }

  def buildSemanticTree(input: String): ParseTree = {
    val notations: JValue = sendGetNotationPosRequest(input)
    // Case 1: No notations
    val semanticTree: ParseTree = notations match {
      case JNothing ⇒ RawString(input)
      case JObject(notationToParsesMap) ⇒ {
        val treeList: List[ParseTree] = notationToParsesMap flatMap {
          case JField(notation, JArray(parsesForNotation)) ⇒
            parsesForNotation.map((jvalue: JValue) ⇒ buildVariant(notation, jvalue))
        }
        Variants(treeList)
      }
    }
    semanticTree
  }

  def buildVariant(notation: String, parseTree: JValue): ParseTree = {
    val JArray(position) = parseTree \ "position";
    val JString(before) = position(3)
    val JString(after) = position(4)
   
    // Build argument maps
    
    RawString("")
  }

  def sendGetNotationPosRequest(input: String) = {
    val response: String = Http(detectNotation).postData(input).asString.body
    parse(response)
  }
}