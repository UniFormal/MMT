package info.kwarc.mmt.marpa
/**
 * Marpa Server Plugin
 * ------------------------------------------
 * Toloaca Ion  <i.toloaca@jacobs-university.de>
 * ------------------------------------------
 * 		General information:
 * 	New notations are written in sTeX, afterwards LaTeXML is used to convert those to .omdoc ,
 * then MMT is used to parse the .omdoc documents and to store the relevant notations.
 *
 *   	Purpose:
 * 	The code below uses the notations stored in MMT as Markers (Scala datatypes) to create a Marpa
 * grammar and make it available via a post request.
 *
 *  	 Details:
 *    To convert from Markers to a Marpa grammar an intermediate format is used (List[String]).
 * Although the format might have been omitted, using tokenized strings make the recursion and
 * the transformation from Markers to the grammar easier, and creating unique rules also becomes easier.
 *    To create the rules, a recursion is used that starts creating the rules first from the
 * deepest nested Markers. This means that when adding a rule - it is enough to check whether a rule
 * with the same content is already in the grammar or not (which wouldn't be the case if the rules would
 * be created in inverse order).
 *    For each top level rule an event is created and for each argument of such rule a relevant action
 * is added to the grammar.
 */

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
import info.kwarc.mmt.api.web.ServerRequest$
import info.kwarc.mmt.stex.STeXImporter
import info.kwarc.mmt.stex.sTeX
import tiscaf.HLet
import info.kwarc.mmt.api._
import tiscaf.HTalk
import info.kwarc.mmt.api.objects._
import scala.collection.mutable.HashMap
import scala.collection.Map
import java.net.URLDecoder
import info.kwarc.mmt.api.utils._
import scala.collection.mutable.ListBuffer

case class PlanetaryError(val text: String) extends Error(text)

class MarpaGrammarGenerator extends ServerExtension("marpa") with Logger {
  var pairIndexNotation: List[((info.kwarc.mmt.api.GlobalName, info.kwarc.mmt.api.notations.TextNotation), Int)] = List();
  override val logPrefix = "marpa"
  /** Server */
  def apply(request: ServerRequest): HLet = {
    try {
      uriComps match {
        //Here the post request is handled
        case "getGrammar" :: _       ⇒ getGrammarResponse
        case "getContentMathML" :: _ ⇒ getContentMathML
        case "getSemanticTree" :: _ ⇒ {
          SemanticTree.grammarGenerator = this
          SemanticTree.getSemanticTree
        }
        case _ ⇒ errorResponse("Invalid request: " + request.path.mkString("/"),
          List(new PlanetaryError("Invalid Request" + request.path)))
      }
    } catch {
      case e: Error ⇒
        log(e.shortMsg)
        errorResponse(e.shortMsg, List(e))
      case e: Exception ⇒uriComps
        errorResponse("Exception occured : " + e.getStackTrace(), List(e))
    }
  }

  //The post request response is defined here 
  def getGrammarResponse: HLet = new HLet {
    def aact(tk: HTalk)(implicit ec: ExecutionContext): Future[Unit] = {
      val reqBody = new Body(tk)
      val notations = controller.library.getModules flatMap {
        case t: DeclaredTheory //if t.path.toPath == "http://mathhub.info/smglom/calculus/onesidedlimit.omdoc?onesidedlimit?leftsided-limit" 
        ⇒
          val not = t.getDeclarations collect {
            case c: Constant ⇒ c.notC.presentationDim.notations.values.flatten.map(not ⇒ c.path -> not) //produces (name, notation) pairs
          }
          not.flatten
        case _ ⇒ Nil
      } //notations is now an iterable of (name, notation) pairs

      pairIndexNotation = notations.toList.zipWithIndex
      pairIndexNotation.foreach(x ⇒
        if (x._1._2.presentationMarkers != Nil) {
          val path = x._1._1.toPath
          val ruleNumber = x._2.toString
          val markers = x._1._2.presentationMarkers
          val prec = x._1._2.precedence
          Grammar.addTopRule(path, ruleNumber, markers, prec) //adding rules to the grammar
        })

      val grammarAsStringList = Grammar.getMarpaGrammar.map(x ⇒ info.kwarc.mmt.api.utils.JSONString(x))
      val resp = info.kwarc.mmt.api.utils.JSONArray(grammarAsStringList: _*)
      //		val params = reqBody.asJSON
      Server.JsonResponse(resp).aact(tk)
    }
  }

  def unescape(text: String): String = {
    def recUnescape(textList: List[Char], acc: String, escapeFlag: Boolean): String = {
      textList match {
        case Nil ⇒ acc
        case '&' :: tail ⇒ recUnescape(tail, acc, true)
        case ';' :: tail if (escapeFlag) ⇒ recUnescape(tail, acc, false)
        case 'a' :: 'm' :: 'p' :: tail if (escapeFlag) ⇒ recUnescape(tail, acc + "\\", true)
        case 'q' :: 'u' :: 'o' :: 't' :: tail if (escapeFlag) ⇒ recUnescape(tail, acc + "\"", true)
        case 'l' :: 't' :: tail if (escapeFlag) ⇒ recUnescape(tail, acc + "<", true)
        case 'g' :: 't' :: tail if (escapeFlag) ⇒ recUnescape(tail, acc + ">", true)
        case x :: tail ⇒ recUnescape(tail, acc + x, true)
        case _ ⇒ acc
      }
    }
    recUnescape(text.toList, "", false)
  }

  def getContentMathML: HLet = new HLet {
    def aact(tk: HTalk)(implicit ec: ExecutionContext): Future[Unit] = {
      println("In getConentMathML")
      val reqBody = new Body(tk)
      val paramsJSON: scala.util.parsing.json.JSONObject = bodyAsJSON(reqBody)

      val params = paramsJSON.obj
      val status = params("status").toString
      val key = params("key").toString
      val input = params("input").toString

      val payloadUnparsed = params("payload") match {
        case scala.util.parsing.json.JSONObject(x) ⇒ x
      }
      def parseJSONtoList(arr: scala.util.parsing.json.JSONArray): List[Any] = {
        val scala.util.parsing.json.JSONArray(list) = arr
        list map {
          case x: Int            ⇒ x
          case x: Double         ⇒ x.toInt
          case x: Float          ⇒ x.toInt
          case x: String         ⇒ x
          case JSONString(value) ⇒ value
          case x                 ⇒ throw ServerError("parseJSONtoList mismatch")
        }
      }
      def parseJSONtoNestedList(arr: scala.util.parsing.json.JSONArray): List[List[Any]] = {
        val scala.util.parsing.json.JSONArray(list) = arr
        list.map({
          case innerList: scala.util.parsing.json.JSONArray ⇒ parseJSONtoList(innerList)
        })
      }

      val payload = payloadUnparsed map {
        case (k: String, v: scala.util.parsing.json.JSONArray) ⇒ (k, parseJSONtoNestedList(v))
      }

      println("Params = " + params.toString())
      println("Payload = " + payload.toString())
      println("Input = " + input + " length " + input.length())

      var ruleNr: Int = -1;
      val pattern = "N(\\d+)$".r
      val argPattern = "A(\\d+)(ArgSeq|VarSeq|Arg|Var)$".r
      pattern.findAllIn(key).matchData foreach { m ⇒ ruleNr = m.group(1).toInt; }
      println("MarpaSubst rule number = " + ruleNr.toString())
      println("MarpaSubst key = " + key)
      println("MarpaSubst payload = " + payload.toString())
      println("MarpaSubst input = " + input)
      var argMap: Map[Int, String] = new HashMap[Int, String]();
      var varMap: Map[Int, String] = new HashMap[Int, String]();
      var seqArgMap: Map[Int, List[String]] = new HashMap[Int, List[String]]();
      var seqVarMap: Map[Int, List[String]] = new HashMap[Int, List[String]]();
      //Construct argument maps
      val notPosArr: List[List[Any]] = payload.getOrElse("position", List(List()))
      val notStart: Int = notPosArr(0)(0).asInstanceOf[Int]
      val notLength: Int = notPosArr(0)(1).asInstanceOf[Int]
      payload foreach (p ⇒ {
        val key = p._1;
        val posArr = p._2;
        if (key != "position") {
          var argNr: Int = -1
          var argType: String = "Invalid"
          argPattern.findAllIn(key).matchData foreach {
            m ⇒
              {
                argNr = m.group(1).toInt
                argType = m.group(2).toString
              }
          }
          println("ArgNr = " + argNr.toString + " ArgType = " + argType)
          p._2 foreach (pos ⇒ {
            //Depending on key type add to one of the maps
            val start = pos(0)
            val length = pos(1)
            println("Argument substring = " + pos(2).toString)
            var value = pos(2).toString
            println("key = " + key + " value = " + value)
            if (argType == "Arg") {
              argMap += (argNr -> value)
            } else if (argType == "Var") {
              varMap += (argNr -> value)
            } else if (argType == "ArgSeq") {
              var currList: List[String] = List()
              if (seqArgMap.contains(argNr)) {
                currList = seqArgMap(argNr)
                seqArgMap -= argNr
              }
              currList = currList :+ value
              seqArgMap += (argNr -> currList)

            } else if (argType == "VarSeq") {
              var currList: List[String] = List()
              if (seqVarMap.contains(argNr)) {
                currList = seqVarMap(argNr)
                seqVarMap -= argNr
              }
              currList = currList :+ value
              seqVarMap += (argNr -> currList)
            } else {
              println("Invalid argument type")
            }
          })
        }
      })

      val result = doNotationTerm(
        pairIndexNotation(ruleNr)._1._1,
        pairIndexNotation(ruleNr)._1._2,
        argMap, varMap, seqArgMap, seqVarMap)
      println("Creating CML response")
      var data: Map[String, String] = new HashMap[String, String]()
      data += ("status" -> "OK")
      val payloadMap = payload.toMap
      println("payload.toString = " + payloadMap)
      //data += ("payload" -> new JSONObject(payloadMap))
      var cml = unescape(result.toCML.toString)
      println("HTML Escaped cml  = " + cml)
      cml = URLDecoder.decode(cml, "UTF-8")
      println("URL Escaped cml = " + cml)
      data += ("cml" -> java.net.URLEncoder.encode(cml, "UTF-8"))
      data += ("input" -> java.net.URLEncoder.encode(input, "UTF-8"))
      //      Ok(Json.toJson(response.toMap))
      val dataJSONString: Map[String, JSONString] = data.map(pair ⇒ (pair._1 -> JSONString(pair._2.toString)))
      val resp = info.kwarc.mmt.api.utils.JSONObject(dataJSONString.toSeq: _*)
      println("Sending Content Math ML response = " + resp.toString())
      tk.setHeader("Access-Control-Allow-Origin", "*")
      Server.JsonResponse(resp).aact(tk)
    }
  }

  def doNotationTerm(spath: GlobalName,
                     not: TextNotation,
                     argMap: Map[Int, String],
                     varMap: Map[Int, String],
                     seqArgMap: Map[Int, List[String]],
                     seqVarMap: Map[Int, List[String]]): Term = {

    println("path = " + spath.toString + "\n notation = " + not.toString)

    def getArg(i: Int): List[Term] = {
      val sArgs: List[String] = seqArgMap.getOrElse(i, argMap.get(i).toList) //should throw exception instead of Nil
      sArgs.map(a ⇒ try {
        scala.xml.XML.loadString(a) match {
          case <mn>{ value }</mn> ⇒ RawNode(<cn>{ value }</cn>)
          case <mi>{ value }</mi> ⇒ OMV(value.text)
          case _                  ⇒ RawNode(a)
        }
      } catch {
        case _: Throwable ⇒ RawNode("<mrow>" + a + "</mrow>")
      })
      /*
       *  <mn>x</mn> => OMI(x) for integers
       *  <mi<x</mi> => OMV(x) 
       *  <mo>s<mo> => OMS(s.toPath) 
       *  CML (apply/csymbol) => eventually literal OMLit(CML) or OMFOREIGN
       */
    }
    def getVarDecl(i: Int): List[VarDecl] = {
      val sArgs: List[String] = seqVarMap.getOrElse(i, varMap.get(i).toList) //should throw exception instead of Nil
      sArgs.map(a ⇒ try{scala.xml.XML.loadString(a) match {
        case <mn>{ value }</mn> ⇒ VarDecl(LocalName(value.text), None, None, None)
        case <mi>{ value }</mi> ⇒ VarDecl(LocalName(value.text), None, None, None)
        case <mo>{ value }</mo> ⇒ VarDecl(LocalName(value.text), None, None, None)
        case _                  ⇒ VarDecl(LocalName(a), None, None, None)
      }} catch{
        case _:Throwable => VarDecl(LocalName("<mrow>" + a + "</mrow>"), None, None, None)
      })
      // Strip a of <mn> / <mi> / <mo>
    }

    val arity = not.arity
    //arity.canHandle(numberOfSubstitution, nrOfVariables, nrOfArguments)
    //val sub = Substitution(arity.subargs.map(sa => Sub(OMV.anonymous, OMV( argumentValues(sa.number-1) ))) : _*)
    val sub = Substitution()
    val con = Context(arity.variables.flatMap(v ⇒ getVarDecl(v.number)): _*)
    val args = arity.arguments flatMap { a ⇒ getArg(a.number) }

    val term = ComplexTerm(spath, sub, con, args)
    //    println("subs = " + arity.subargs.size)
    //    println("con = " + arity.variables.size)
    //    println("args = " + arity.arguments.size)
    println("Term = " + term.toCML.toString + "\n")
    term
  }
  //utils
  private def errorResponse(text: String, errors: List[Throwable]): HLet = {
    JsonResponse("", s"MMT Error in Planetary extension: $text ", errors)
  }

  private def JsonResponse(content: String, info: String, errors: List[Throwable]): HLet = {
    val response: HashMap[String, JSON] = new collection.mutable.HashMap[String, JSON]()
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
        messages("0") = JSONObject(message.toSeq: _*)
      }
      status("messages") = JSONObject(messages.toSeq: _*)
      response("status") = JSONObject(status.toSeq: _*)
    } else { //there are errors
      val status = new collection.mutable.HashMap[String, JSON]()
      if (content == "") {
        status("conversion") = JSONInt(2) //failed with errors
      } else {
        status("conversion") = JSONInt(2) //success with errors
      }
      val messages = new collection.mutable.HashMap[String, JSON]()
      errors.zipWithIndex foreach { p ⇒
        val message = new collection.mutable.HashMap[String, JSON]()
        p._1 match {
          case se: SourceError ⇒
            message("type") = JSONString("Fatal")
            message("shortMsg") = JSONString(se.mainMessage)
            message("longMsg") = JSONString(se.getStackTrace.mkString("\n"))
            message("srcref") = JSONObject(List("from" -> JSONObject(List("line" -> JSONInt(se.ref.region.start.line), "col" -> JSONInt(se.ref.region.start.column)): _*),
              "to" -> JSONObject(List("line" -> JSONInt(se.ref.region.end.line), "col" -> JSONInt(se.ref.region.end.column)): _*)): _*)
          case e ⇒
            message("type") = JSONString("Fatal")
            message("shortMsg") = JSONString(e.getMessage)
            message("longMsg") = JSONString(e.getStackTrace.mkString("\n"))
          //no srcref :(
        }
        messages(p._2.toString) = JSONObject(message.toSeq: _*)
      }
      status("messages") = JSONObject(messages.toSeq: _*)
      response("status") = JSONObject(status.toSeq: _*)
    }
    log("Sending Response: " + response)
    Server.JsonResponse(JSONObject(response.toSeq: _*))
  }

  private def toStringMarkers(not: TextNotation): List[String] = {
    not.parsingMarkers flatMap {
      case a: Arg         ⇒ Some("_")
      case a: SeqArg      ⇒ Some("_...")
      case a: ImplicitArg ⇒ None
      case d: Delimiter   ⇒ Some(d.text)
      case v: Var         ⇒ Some("_")
      case _              ⇒ None
    }
  }
  def bodyAsJSON(b: Body): scala.util.parsing.json.JSONObject = {
    val bodyS = java.net.URLDecoder.decode(b.asString, "UTF-8")
    scala.util.parsing.json.JSON.parseRaw(bodyS) match {
      case Some(j: scala.util.parsing.json.JSONObject) ⇒ j
      case _ ⇒ throw ServerError("Invalid JSON " + bodyS)
    }
  }
}