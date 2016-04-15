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
case class Notation(name: String, arguments: List[Argument]) extends ParseTree
case class Argument(name: String, value: List[Variants]) extends ParseTree
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

      val Variants(inputParses) = buildSemanticTree(input)
      inputParses foreach {
        case Notation(name, argList) => 
          println("Notation = " + name)
          argList foreach {
            case Argument(argName, variantList) => 
              println("    Argument = " + argName)
              variantList foreach {
                case Variants(list) => 
                  if (variantList.size > 1) println("      Argument part (only for sequence args)")
                  list.foreach((variant) => println("        Variant = " + variant.toString))
              }
          } 
      }
      
      val resp = info.kwarc.mmt.api.utils.JSONString("")
      tk.setHeader("Access-Control-Allow-Origin", "*")
      Server.JsonResponse(resp).aact(tk)
    }
  }
  //  JObject(List(JField(message,JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)),
  //  JField(status,JString(OK)),
  //  JField(payload,JObject(List(JField(_natarith_multiplicationP7N58,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN58A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(10), JString(%3Cmi%3Ei%3C%2Fmi%3E))), JArray(List(JInt(20), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_intarith_modP7N155,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN155A1Arg,JArray(List(JArray(List(JInt(40), JInt(10), JString(%3Cmi%3E3%3C%2Fmi%3E)))))), JField(argRuleN155A2Arg,JArray(List(JArray(List(JInt(62), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_comparith_multiplicationP7N180,JArray(List(JObject(List(JField(argRuleN180A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(10), JString(%3Cmi%3Ei%3C%2Fmi%3E))), JArray(List(JInt(20), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E)))))), JField(position,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_intarith_additionP5N143,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(72), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN143A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))), JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_realarith_additionP5N205,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(72), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN205A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))), JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_intarith_multiplicationP7N149,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN149A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(10), JString(%3Cmi%3Ei%3C%2Fmi%3E))), JArray(List(JInt(20), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_arithmetics_additionP5N124,JArray(List(JObject(List(JField(argRuleN124A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))), JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(position,JArray(List(JArray(List(JInt(0), JInt(72), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_comparith_additionP5N174,JArray(List(JObject(List(JField(argRuleN174A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))), JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(position,JArray(List(JArray(List(JInt(0), JInt(72), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_realarith_multiplicationP7N211,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN211A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(10), JString(%3Cmi%3Ei%3C%2Fmi%3E))), JArray(List(JInt(20), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_natarith_modP7N64,JArray(List(JObject(List(JField(argRuleN64A2Arg,JArray(List(JArray(List(JInt(62), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E)))))), JField(position,JArray(List(JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN64A1Arg,JArray(List(JArray(List(JInt(40), JInt(10), JString(%3Cmi%3E3%3C%2Fmi%3E))))))))))), JField(_natarith_additionP5N52,JArray(List(JObject(List(JField(argRuleN52A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))), JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(position,JArray(List(JArray(List(JInt(0), JInt(72), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_arithmetics_multiplicationP7N132,JArray(List(JObject(List(JField(argRuleN132A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(10), JString(%3Cmi%3Ei%3C%2Fmi%3E))), JArray(List(JInt(20), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E)))))), JField(position,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_ratarith_multiplicationP7N24,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN24A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(10), JString(%3Cmi%3Ei%3C%2Fmi%3E))), JArray(List(JInt(20), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_ratarith_additionP5N18,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(72), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN18A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))), JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))))))))
  def buildSemanticTree(input: String): Variants = {
    val response: JValue = sendGetNotationPosRequest(input)
    val notations: JObject = (response \ "payload").asInstanceOf[JObject]
 
    // Case 1: No notations
    val semanticTree: Variants = notations match {
      case JObject(List()) ⇒ Variants(List(RawString(input)))
      case JObject(notationToParsesMap) ⇒ {
        val treeList: List[Notation] = notationToParsesMap flatMap {
          case JField(notation, JArray(parsesForNotation)) ⇒
            parsesForNotation.map((jvalue: JValue) ⇒ buildNotationVariant(notation, jvalue.asInstanceOf[JObject]))
        }
        Variants(treeList)
      }
    }
    semanticTree
  }

  def buildNotationVariant(notation: String, jObject: JObject): Notation = {
    val JObject(fieldListWithPosition) = jObject
    val fieldList = fieldListWithPosition.filter({
      case field@JField(argName, _) => argName != "position"
    })
    val arguments : List[Argument] = fieldList map {
      case JField(argName, jvalue) => buildArgVariants(argName, jvalue.asInstanceOf[JArray])
    }
    Notation(notation, arguments)
  }
  
  def buildArgVariants(argName: String, jArray: JArray): Argument = {
    val JArray(listOfArgumentInfo) = jArray
    val argSubstrings: List[String] = listOfArgumentInfo map {
      case JArray(List(_,_,JString(substring))) => substring
    }
    Argument(argName, argSubstrings map buildSemanticTree) 
  }

  def sendGetNotationPosRequest(input: String) = {
    val response: String = Http(detectNotation).postData(input).asString.body
    parse(response)
  }
}