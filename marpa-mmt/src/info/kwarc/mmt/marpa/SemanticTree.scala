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
import info.kwarc.mmt.marpa._
import org.apache.commons.lang3._
import scala.collection.mutable._

object SemanticTree {
  abstract class ParseTree {
    def toCML: List[String]
  }

  case class Variants(treeList: List[ParseTree]) extends ParseTree {
    override def toCML: List[String] = {
      treeList.flatMap(_.toCML)
    }
  }

  case class Notation(name: String, arguments: List[Argument]) extends ParseTree {
    override def toCML: List[String] = {
      val argNames: List[String] = arguments map {
        case Argument(name, _) ⇒ name
      }
      // For each argument, for each argument part, the list of all possible parses
      val argCML: List[List[List[String]]] = arguments.map(_.argToCML)
      // List(For each argument, for each argument part, one of the possible parses)
      val argCMLcombs = SemanticTree.argumentPossibilities(argCML)
      argCMLcombs.map((argComb) ⇒ {
        SemanticTree.toCML(name, (argNames zip argComb))
      })
    }
  }

  case class Argument(name: String, value: List[Variants]) extends ParseTree {
    override def toCML: List[String] = { throw SemanticTreeError("Argument::toCML is not defined"); List("") }
    // For each argument part, the list of possible parses
    def argToCML: List[List[String]] = {
      value.map(_.toCML)
    }
  }

  case class RawString(value: String) extends ParseTree {
    override def toCML: List[String] = {
      List(value)
    }
  }

  case class SemanticTreeError(message: String) extends Exception

  val url = "http://localhost:3000"
  val detectNotation = url + "/detect_notations"
  var grammarGenerator: MarpaGrammarGenerator = null

  def resetTermSharingState {
    toCMLShare = scala.collection.mutable.HashSet.empty[(String, List[(String, List[String])])]
  }

  def bodyToJson(b: Body): JValue = {
    var str = java.net.URLDecoder.decode(b.asString, "UTF-8")
    str = str.split("&")
      .map(x ⇒ x.split("=")
        .map(y ⇒ if (y == "false" || y == "true") y else "\"" + y + "\"")
        .mkString(":"))
      .mkString(",")
    str = "{ " + str + " }"
    println(str)
    return parse(str)
  }

  def getSemanticTree: HLet = new HLet {
    def aact(tk: HTalk)(implicit ec: ExecutionContext): Future[Unit] = {
      // clear up after previous request
      resetTermSharingState
      println("-->getSemanticTree")
      val reqBody = new Body(tk)
      val inputJSON: JValue = bodyToJson(reqBody)
      println(inputJSON)
      val input: String = (inputJSON \ "input").asInstanceOf[JString].values
      termSharing = (inputJSON \ "termSharing").asInstanceOf[JBool].values

      println("input = " + input)
      println("term sharing = " + termSharing.toString)

      val inputParses = buildSemanticTree(input)
      inputParses.treeList foreach {
        case Notation(name, argList) ⇒
          println("Notation = " + name)
          argList foreach {
            case Argument(argName, variantList) ⇒
              println("    Argument = " + argName)
              variantList foreach {
                case Variants(list) ⇒
                  if (variantList.size > 1) println("      Argument part (only for sequence args)")
                  list.foreach((variant) ⇒ println("        Variant = " + variant.toString))
              }
          }
      }
      println("END OF INPUT PARSES")
      val CMLlist = inputParses.toCML.toSet.toList // Get unique parses
      println("CML ->")
      var i = 1
      CMLlist foreach {
        case str ⇒
          println(i.toString + ") " + str)
          i = i + 1
      }
      val CMLlistJSON = CMLlist map info.kwarc.mmt.api.utils.JSONString
      val resp = info.kwarc.mmt.api.utils.JSONArray(CMLlistJSON: _*)
      tk.setHeader("Access-Control-Allow-Origin", "*")
      Server.JsonResponse(resp).aact(tk)
    }
  }

  var termSharing = false // Sharing is turned off for testing 

  def createShareHrefTo(cml: String): String = {
    if (!termSharing) {
      return cml
    }
    val p = """^<[^\s]+?\s*? id=\"([\d]+)\".*$""".r
    val p(idStr) = cml
    val id = idStr.toInt
    "<share href=\"#" + id + "\"/>"
  }
  var CMLTermId = 1
  val toCMLMemo = scala.collection.mutable.HashMap.empty[(String, List[(String, List[String])]), String]
  var toCMLShare = scala.collection.mutable.HashSet.empty[(String, List[(String, List[String])])]
  def toCML(notation: String, arguments: List[(String, List[String])]): String = {
    val vals = toCMLMemo
    var share = toCMLShare
    val argTuple = (notation, arguments)
    if (vals contains argTuple) {
      val result = vals(argTuple)
      if (share contains argTuple) {
        return createShareHrefTo(result)
      } else {
        share += argTuple
        return vals(argTuple)
      }
    }
    val argDecoded = arguments map {
      case (str, listStr) ⇒ (str, listStr map java.net.URLDecoder.decode)
    }
    val pattern = "(\\d+)$".r
    val argPattern = "A(\\d+)(ArgSeq|VarSeq|Arg|Var)$".r
    val ruleNr: Int = pattern.findFirstIn(notation).getOrElse("-1").toInt
    var argMap: Map[Int, String] = new HashMap[Int, String]();
    var varMap: Map[Int, String] = new HashMap[Int, String]();
    var seqArgMap: Map[Int, List[String]] = new HashMap[Int, List[String]]();
    var seqVarMap: Map[Int, List[String]] = new HashMap[Int, List[String]]();
    argDecoded foreach {
      case (argName, substringList) ⇒
        var argNr = -1
        var argType = ""
        argPattern.findAllIn(argName).matchData foreach { m ⇒ { argNr = m.group(1).toInt; argType = m.group(2).toString; } }
        substringList foreach {
          case value: String ⇒
            //Depending on key type add to one of the maps
            println("Argument substring = " + value)
            argType match {
              case "Arg" ⇒ {
                argMap += (argNr -> value)
              }
              case "Var" ⇒ {
                varMap += (argNr -> value)
              }
              case "ArgSeq" ⇒ {
                var currList: List[String] = List()
                if (seqArgMap.contains(argNr)) {
                  currList = seqArgMap(argNr)
                  seqArgMap -= argNr
                }
                currList = currList :+ value
                seqArgMap += (argNr -> currList)
              }
              case "VarSeq" ⇒ {
                var currList: List[String] = List()
                if (seqVarMap.contains(argNr)) {
                  currList = seqVarMap(argNr)
                  seqVarMap -= argNr
                }
                currList = currList :+ value
                seqVarMap += (argNr -> currList)
              }
              case _ ⇒ {
                println("Invalid argument type: " + argType)
              }
            }
        } // substringList foreach  
    } // arguments foreach
    val term = grammarGenerator.doNotationTerm(
      grammarGenerator.pairIndexNotation(ruleNr)._1._1,
      grammarGenerator.pairIndexNotation(ruleNr)._1._2,
      argMap, varMap, seqArgMap, seqVarMap)
    val escapedResult = term.toCML.toString
    val unescapedResult = StringEscapeUtils unescapeXml escapedResult
    var result = java.net.URLDecoder.decode(unescapedResult)
    // Insert term id between "<apply" and "/>..."
    val resultPat = """(^<[^>]*?)(>.*$)""".r
    val resultPat(s1, s2) = result
    result = s1 + " id=\"" + CMLTermId.toString + "\" " + s2
    CMLTermId += 1
    vals += ((argTuple, result))
    share += argTuple
    return result
  }

  //  JObject(List(JField(message,JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)),
  //  JField(status,JString(OK)),
  //  JField(payload,JObject(List(JField(_natarith_multiplicationP7N58,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN58A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(10), JString(%3Cmi%3Ei%3C%2Fmi%3E))), JArray(List(JInt(20), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_intarith_modP7N155,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN155A1Arg,JArray(List(JArray(List(JInt(40), JInt(10), JString(%3Cmi%3E3%3C%2Fmi%3E)))))), JField(argRuleN155A2Arg,JArray(List(JArray(List(JInt(62), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_comparith_multiplicationP7N180,JArray(List(JObject(List(JField(argRuleN180A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(10), JString(%3Cmi%3Ei%3C%2Fmi%3E))), JArray(List(JInt(20), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E)))))), JField(position,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_intarith_additionP5N143,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(72), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN143A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))), JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_realarith_additionP5N205,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(72), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN205A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))), JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_intarith_multiplicationP7N149,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN149A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(10), JString(%3Cmi%3Ei%3C%2Fmi%3E))), JArray(List(JInt(20), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_arithmetics_additionP5N124,JArray(List(JObject(List(JField(argRuleN124A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))), JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(position,JArray(List(JArray(List(JInt(0), JInt(72), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_comparith_additionP5N174,JArray(List(JObject(List(JField(argRuleN174A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))), JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(position,JArray(List(JArray(List(JInt(0), JInt(72), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_realarith_multiplicationP7N211,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN211A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(10), JString(%3Cmi%3Ei%3C%2Fmi%3E))), JArray(List(JInt(20), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_natarith_modP7N64,JArray(List(JObject(List(JField(argRuleN64A2Arg,JArray(List(JArray(List(JInt(62), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E)))))), JField(position,JArray(List(JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN64A1Arg,JArray(List(JArray(List(JInt(40), JInt(10), JString(%3Cmi%3E3%3C%2Fmi%3E))))))))))), JField(_natarith_additionP5N52,JArray(List(JObject(List(JField(argRuleN52A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))), JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(position,JArray(List(JArray(List(JInt(0), JInt(72), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_arithmetics_multiplicationP7N132,JArray(List(JObject(List(JField(argRuleN132A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(10), JString(%3Cmi%3Ei%3C%2Fmi%3E))), JArray(List(JInt(20), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E)))))), JField(position,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_ratarith_multiplicationP7N24,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN24A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(10), JString(%3Cmi%3Ei%3C%2Fmi%3E))), JArray(List(JInt(20), JInt(10), JString(%3Cmi%3E5%3C%2Fmi%3E))))))))))), JField(_ratarith_additionP5N18,JArray(List(JObject(List(JField(position,JArray(List(JArray(List(JInt(0), JInt(72), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E%3Cmo%3E%2B%3C%2Fmo%3E%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E)))))), JField(argRuleN18A1ArgSeq,JArray(List(JArray(List(JInt(0), JInt(30), JString(%3Cmi%3Ei%3C%2Fmi%3E%3Cmo%3E%E2%81%A2%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))), JArray(List(JInt(40), JInt(32), JString(%3Cmi%3E3%3C%2Fmi%3E%3Cmo%3Emod%3C%2Fmo%3E%3Cmi%3E5%3C%2Fmi%3E))))))))))))))))
  var buildSemanticTreeMemo = scala.collection.mutable.HashMap.empty[String, Variants]
  def buildSemanticTree(input: String): Variants = {
    val vals = buildSemanticTreeMemo
    if (vals.contains(input)) {
      return vals(input)
    }

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
    vals += ((input, semanticTree))
    semanticTree
  }

  val buildNotationVariantMemo = scala.collection.mutable.HashMap.empty[(String, JObject), Notation]
  def buildNotationVariant(notation: String, jObject: JObject): Notation = {
    val vals = buildNotationVariantMemo
    val argTuple = (notation, jObject)
    if (vals.contains(argTuple)) {
      return vals(argTuple)
    }
    val JObject(fieldListWithPosition) = jObject
    val fieldList = fieldListWithPosition.filter({
      case field @ JField(argName, _) ⇒ argName != "position"
    })
    val arguments: List[Argument] = fieldList map {
      case JField(argName, jvalue) ⇒ buildArgVariants(argName, jvalue.asInstanceOf[JArray])
    }
    val result = Notation(notation, arguments)
    vals += ((argTuple, result))
    return result
  }

  val buildArgVariantsMemo = scala.collection.mutable.HashMap.empty[(String, JArray), Argument]
  def buildArgVariants(argName: String, jArray: JArray): Argument = {
    val vals = buildArgVariantsMemo
    val argTuple = (argName, jArray)
    if (vals.contains(argTuple)) {
      return vals(argTuple)
    }
    val JArray(listOfArgumentInfo) = jArray
    val argSubstrings: List[String] = listOfArgumentInfo map {
      case JArray(List(_, _, JString(substring))) ⇒ substring
    }
    val result = Argument(argName, argSubstrings map buildSemanticTree)
    vals += ((argTuple, result))
    return result
  }

  val sendGetNotationPosRequestMemo = scala.collection.mutable.HashMap.empty[String, JValue]
  def sendGetNotationPosRequest(input: String): JValue = {
    val vals = sendGetNotationPosRequestMemo
    if (vals.contains(input)) {
      vals(input)
    } else {
      println("sendGetNotationPosRequest input = " + input)
      val response: String = Http(detectNotation).postData(input).asString.body
      val result = parse(response)
      vals += ((input, result))
      result
    }
  }

  // For each argument, for each argument part, the list of all possible parses
  // List(For each argument, for each argument part, one of the possible parses)
  // List(List(List(1,2),List(3)), List(List(4,5))) =>
  // ListArg(ListPart(ListParse(1,2),ListParse(3)), ListPart(ListParse(4,5))) =>
  /* ListPossibilities(   
      ListArg( ListPart(1,3), ListPart(4) )
      ListArg( ListPart(1,3), ListPart(5) )
      ListArg( ListPart(2,3), ListPart(4) )
      ListArg( ListPart(2,3), ListPart(5) )
     )   
  */
  /* ListPossibilities( Nil )
  */
  /* List(
       ListArg( ListPart(4) )
       ListArg( ListPart(5) )
     )
  */
  def argumentPossibilities[A](listArg: List[List[List[A]]]): List[List[List[A]]] = listArg match {
    case Nil ⇒ List(Nil)
    case arg :: otherArgs ⇒ {
      val possibilities = argumentPossibilities(otherArgs)
      // Add the current argument to each parse possibility
      val argCombs = combs(arg)
      possibilities.flatMap((possibility) ⇒ {
        argCombs.map((argCombs) ⇒ {
          argCombs :: possibility
        })
      })
    }
  }

  def combs[A](xss: List[List[A]]): List[List[A]] = xss match {
    case Nil       ⇒ List(Nil)
    case xs :: rss ⇒ for (x ← xs; cs ← combs(rss)) yield x :: cs
  }

}