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
 *   	Purpose of this code:
 * 	The code below uses the notations stored in MMT as Markers (Scala datatypes) to create a Marpa 
 * grammar and make it available via a post request.
 *  	
 *  	 Details about the code:
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
import scala.util.parsing.json.JSONArray
import scala.util.parsing.json.JSONObject
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
import info.kwarc.mmt.api.ontology.isDefinedBy
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
import info.kwarc.sally4.nnexus.factories.comm.MarpaSubst
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import scala.collection.mutable.HashMap;
import scala.collection.Map;
import scala.collection.JavaConversions._
import java.net.URLDecoder

class MarpaBody(tk: HTalk) extends Body(tk: HTalk) {
	def asMarpaSubst : MarpaSubst = {
	val byteArray: Array[Byte] = tk.req.octets.getOrElse(throw ServerError("no body found in MarpaBody"))
			var bis : ByteArrayInputStream = null
			var ois : ObjectInputStream = null
			var obj : Object = null
			try {
				bis = new ByteArrayInputStream(byteArray)
				ois = new ObjectInputStream(bis)
				obj =  ois.readObject()
			} finally {
				if (bis != null) bis.close
				if (ois != null) ois.close
			}
	obj.asInstanceOf[MarpaSubst]
}
} 

case class PlanetaryError(val text : String) extends Error(text)

class MarpaGrammarGenerator extends ServerExtension("marpa") with Logger {
	var pairIndexNotation : List[((info.kwarc.mmt.api.GlobalName, info.kwarc.mmt.api.notations.TextNotation), Int)] = List();
override val logPrefix = "marpa"
/** Server */   
def apply(uriComps: List[String], query: String, body : Body): HLet = {
		try {
			uriComps match {
				//Here the post request is handled
			case "getGrammar" :: _ => getGrammarResponse
			case "getContentMathML" :: _ => getContentMathML
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


//The post request response is defined here 
def getGrammarResponse : HLet  = new HLet {
	def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = {
		val reqBody = new Body(tk)
		val notations = controller.library.getModules flatMap {
		case t : DeclaredTheory 
		//if t.path.toPath == "http://mathhub.info/smglom/calculus/onesidedlimit.omdoc?onesidedlimit?leftsided-limit" 
		=> 
		val not = t.getDeclarations collect {
		case c : Constant => c.notC.presentationDim.notations.values.flatten.map(not => c.path -> not) //produces (name, notation) pairs
		} 
		not.flatten
		case _ => Nil
		} //notations is now an iterable of (name, notation) pairs

		pairIndexNotation = notations.toList.zipWithIndex
				pairIndexNotation.foreach( x => 
				if (x._1._2.presentationMarkers != Nil) {
					Grammar.addTopRule(x._1._1.toPath+"N"+x._2.toString, x._1._2.presentationMarkers)}) //adding rules to the grammar
					val resp = new JSONArray( Grammar.getMarpaGrammar );
		val params = reqBody.asJSON
				Server.JsonResponse(resp).aact(tk)
	}
}

def unescape(text:String):String = {
		def recUnescape(textList:List[Char],acc:String,escapeFlag:Boolean):String= {
				textList match {
				case Nil => acc
				case '&'::tail => recUnescape(tail,acc,true)
				case ';'::tail if (escapeFlag) => recUnescape(tail,acc,false)
				case 'a'::'m'::'p'::tail if (escapeFlag) => recUnescape(tail,acc+"\\",true)
				case 'q'::'u'::'o'::'t'::tail if (escapeFlag) => recUnescape(tail,acc+"\"",true)
				case 'l'::'t'::tail if (escapeFlag) => recUnescape(tail,acc+"<",true)
				case 'g'::'t'::tail if (escapeFlag) => recUnescape(tail,acc+">",true)
        case x::tail => recUnescape(tail,acc+x,true)
        case _ => acc
				}
		}
		recUnescape(text.toList,"",false)

}

def getContentMathML : HLet  = new HLet {
		def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = {
			println("In getConentMathML")  
			val reqBody : MarpaSubst = new MarpaBody(tk).asMarpaSubst
			val status : String = reqBody.getStatus();
			val payload = reqBody.getPayload();
			val key : String = reqBody.getKey();
			val input : String = reqBody.getInput();
			println("MarpaSubst.key = " + key)
      var ruleNr : Int = -1;
      val pattern = "N(\\d+)$".r
      val argPattern = "A(\\d+)(ArgSeq|VarSeq|Arg|Var)$".r
      pattern.findAllIn(key).matchData foreach {
        m => ruleNr = m.group(1).toInt;
      }
      println("MarpaSubst rule number = " + ruleNr.toString())
      println("MarpaSubst payload = " + payload.toString())
      println("MarpaSubst input = " + input)
      var argMap : Map[Int, String] = new HashMap[Int, String]();
      var varMap : Map[Int, String] = new HashMap[Int, String]();
      var seqArgMap : Map[Int, List[String]] = new HashMap[Int, List[String]]();
      var seqVarMap : Map[Int, List[String]] = new HashMap[Int, List[String]]();
      //Construct argument maps
      val notPosArr : Array[Array[Integer]] = payload.getOrElse("position", Array(Array()))
      val notStart : Integer = notPosArr(0)(0)
      val notLength : Integer = notPosArr(0)(1)
      payload foreach (p => {
        val key = p._1;
        val posArr = p._2;
        if (key != "position") {
          var argNr : Int = -1
          var argType : String = "Invalid"
          argPattern.findAllIn(key).matchData foreach {
              m => {argNr = m.group(1).toInt
                    argType = m.group(2).toString
              }
          }
          println("ArgNr = " + argNr.toString + " ArgType = " + argType)
          p._2 foreach (pos => {
             //Depending on key type add to one of the maps
            val start = pos(0)
            val length = pos(1)
            var value = input.substring(notStart + start, notStart + start + length)
            println("key = " + key + " value = " + value)
            if (argType == "Arg") {
              argMap += (argNr -> value)
            } else if (argType == "Var") {
              varMap += (argNr -> value)
            } else if (argType == "ArgSeq") {
              var currList : List[String]= List()
              if (seqArgMap.contains(argNr)) {
                currList = seqArgMap(argNr)
                seqArgMap -= argNr
              }
              currList = currList :+ value
              seqArgMap += (argNr -> currList)
              
            } else if (argType == "VarSeq") {
              var currList : List[String]= List()
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
			       pairIndexNotation(ruleNr)._1._1 ,
			       pairIndexNotation(ruleNr)._1._2 , 
             argMap, varMap, seqArgMap, seqVarMap)
      println("Creating CML response")
      var data : Map[String, AnyRef] = new HashMap[String, AnyRef]()
      data += ("status" -> "OK")
      val payloadMap = payload.toMap
      println("payload.toString = " + payloadMap)
      //data += ("payload" -> new JSONObject(payloadMap))
      var cml = unescape(result.toCML.toString)
      println("HTML Escaped cml  = " + cml)
      cml = URLDecoder.decode(cml, "UTF-8")
      println("URL Escaped cml = " + cml)
      data += ("cml" -> cml)
      data += ("input" -> input)
//      Ok(Json.toJson(response.toMap))
      val resp = new JSONObject(data.toMap)
      println("Sending Content Math ML response = " + resp.toString())
			Server.JsonResponse(resp).aact(tk)
		}
	}

	def doNotationTerm(spath : GlobalName, 
			not : TextNotation,
			argMap : Map[Int, String], 
      varMap : Map[Int, String],
      seqArgMap : Map[Int, List[String]],
      seqVarMap : Map[Int, List[String]]) : Term = {
      
      println( "path = " + spath.toString + "\n notation = " + not.toString)
			
      def getArg(i : Int) : List[Term] = {
        val sArgs : List[String] = seqArgMap.getOrElse(i, argMap.get(i).toList) //should throw exception instead of Nil
        sArgs.map(a => OMV(a))
      }
      def getVarDecl(i : Int) : List[VarDecl] = {
        val sArgs : List[String] = seqArgMap.getOrElse(i, argMap.get(i).toList) //should throw exception instead of Nil
        sArgs.map(a => VarDecl(LocalName(a),  None, None, None))
      }
      
      val arity = not.arity
					//arity.canHandle(numberOfSubstitution, nrOfVariables, nrOfArguments)
			//val sub = Substitution(arity.subargs.map(sa => Sub(OMV.anonymous, OMV( argumentValues(sa.number-1) ))) : _*)
      val sub = Substitution()
      val con = Context(arity.variables.flatMap(v => getVarDecl(v.number)) :_*)
      val args = arity.arguments flatMap {a => getArg(a.number)}
      
      val term = ComplexTerm(spath, sub, con, args)
			println ("subs = " + arity.subargs.size)
			println ("con = " + arity.variables.size)
			println ("args = " + arity.arguments.size)
			println( "\nTerm = " + term.toCML.toString)
			term
	}

	//utils
	private def errorResponse(text : String, errors : List[Throwable]) : HLet = {
			JsonResponse("", s"MMT Error in Planetary extension: $text ", errors)
	}

	private def JsonResponse(content : String, info : String, errors : List[Throwable]) : HLet = {
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
											message("shortMsg") = info
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
								message("shortMsg") = se.getStackTraceString
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
			Server.JsonResponse(JSONObject(response.toMap))
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

}




//  def getNotations : HLet = new HLet {
//    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
//      val reqBody = new Body(tk)
//      val params = reqBody.asJSON.obj
//      val spathS = params.get("spath").getOrElse(throw ServerError("No spath found")).toString
//      val languageO = params.get("language").map(_.toString)
//      val dimensionO = params.get("dimension").map(_.toString)
//      
//      val spath = Path.parse(spathS)
//      controller.get(spath) match {
//        case c : Constant =>
//          var notations = dimensionO match {
//            case None => c.notC.getAllNotations
//            case Some("parsing") => c.notC.parsingDim.notations.values.flatten
//            case Some("presentation") => c.notC.presentationDim.notations.values.flatten
//            case Some("verbalization") => c.notC.verbalizationDim.notations.values.flatten
//            case Some(s) => throw ServerError("Invalid notation dimension: '" + s  + "'. Expected parsing, presentation or verbalization")
//          }
//          
//          notations = languageO match {
//            case None => notations
//            case Some(lang) => notations.filter(_.scope.languages.contains(lang))
//          }
//          Server.JsonResponse(JSONArray(notations.map(n => JSONArray(toStringMarkers(n))).toList)).aact(tk)
//        case x => throw ServerError("Expected path pointing to constant, found :" + x.getClass())
//      }
//    }
//  }
