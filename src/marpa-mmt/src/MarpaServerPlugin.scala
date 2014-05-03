package info.kwarc.mmt.marpa
import scala.util.matching.Regex
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.modules.{DeclaredTheory}
import info.kwarc.mmt.api.flexiformal._
import info.kwarc.mmt.stex._
import info.kwarc.mmt.api.objects._
import symbols.{Constant}

import scala.util.parsing.json._
import tiscaf._
import scala.concurrent._

case class PlanetaryError(val text : String) extends Error(text)


class MarpaGrammarGenerator extends ServerExtension("marpa") with Logger {
  
  override val logPrefix = "marpa"
     /** Server */   
  def apply(uriComps: List[String], query: String, body : Body): HLet = {
    try {
      uriComps match {
        case "getGrammar" :: _ => getGrammarResponse
        case "getPresentation" :: _ => getPresentationResponse
        case "getCompiled" :: _ => getCompiledResponse
        case "getRelated" :: _ => getRelated
        case "getNotations" :: _ => getNotations
        case "getDefinitions" :: _ => getDefinitions
        case "getActions" :: _ => getActionsResponse
        case _ => errorResponse("Invalid request: " + uriComps.mkString("/"), List(new PlanetaryError("Invalid Request" + uriComps)))
       }
    } catch {
      case e : Error => 
        log(e.longMsg) 
        errorResponse(e.longMsg, List(e))
      case e : Exception => 
        errorResponse("Exception occured : " + e.getStackTrace(), List(e))
    }
  }
  
  def getGrammarResponse : HLet  = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val notations = controller.library.getModules flatMap {
        case t : DeclaredTheory 
        	if t.path.toPath == "http://mathhub.info/smglom/mv/equal.omdoc?equal"
        	=> 
          val not = t.getDeclarations collect {
            case c : Constant => c.notC.presentationDim.notations.values.flatten
          } 
          not.flatten
        case _ => Nil
      }
      
     
//      val symbol = notations.head.name
//      //val prototype = OMA(OMS(symbol), List(args))
      //prototype.toNode
 
//        val xmlNotations = notations.map(n => (n.name.toPath, n.presentationMarkers))
//       val resp = new JSONArray(xmlNotations.map(_.toString))
    
      val xmlNotations = notations.map(n => Grammar.addTopRule(n.name.toPath, n.presentationMarkers)) 
     val resp = new JSONArray( Grammar.getMarpaGrammar );
      val params = reqBody.asJSON
      Server.JsonResponse(resp).aact(tk)
    }
  }
  def getActionsResponse : HLet  = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val notations = controller.library.getModules flatMap {
        case t : DeclaredTheory 
        	//if t.path.toPath == "http://mathhub.info/smglom/mv/structure.omdoc?structure"
        => 
          val not = t.getDeclarations collect {
            case c : Constant => c.notC.presentationDim.notations.values.flatten
          } 
          not.flatten
        case _ => Nil
      }
      
      //val xmlNotations = notations.map(n => (n.name.toPath, n.presentationMarkers))
      val xmlNotations = notations.map(n => 
        Grammar.addTopRule(n.name.toPath
            ,n.presentationMarkers))
     
      val symbol = notations.head.name
      //val prototype = OMA(OMS(symbol), List(args))
      //prototype.toNode
      
      //val resp = new JSONArray(xmlNotations.map(_.toString))
      
      val resp = new JSONArray("#TODO...My Actions"::Nil)
        //val resp = new JSONArray(List(Grammar.rules.toString))
     
      // val resp = new JSONArray(xmlNotations)
      val params = reqBody.asJSON
      
      Server.JsonResponse(resp).aact(tk)
    }
  }

  
  
  def doMarkers(markers : List[Marker]) : scala.xml.Node = {
    <m:mrow> {markers.map(doMarker)} </m:mrow>
  }
  def doMarker(marker : Marker) : scala.xml.Node = marker match {
    case Arg(argNr,precedence) => {
		      val precS = precedence match {
		        case Some(x) => x.toString
		        case None    => ""
		      }
		      <omdoc:render name={"arg" + argNr} precedence={precS} />
	      }
    case SeqArg(argNr,delim,precedence) => {
    		val precS = precedence match {
    		  case Some(x) => x.toString
    		  case None => ""
    		}
    		<omdoc:iterate name={"arg"+argNr} precedence={precS}>
    		<omdoc:separator>
    			{doMarker(delim)}
    		</omdoc:separator>
    			 <omdoc:render name={"arg" + argNr} precedence={precS} />
    		</omdoc:iterate>
    }		//{doMarker(Arg(argNr,precedence))}
  	case m : TdMarker => <td> <todo/> </td> 
    case d : Delimiter => <m:mo> {d.text} </m:mo>
    case m : GroupMarker => doMarkers(m.elements)
    
    case _ => <todo/>		
  }
  

  abstract class GenericRule
  case class Rule(name:String, content: List[String]) extends GenericRule
  object Grammar {
	  var rules = Set.empty[Rule]
	  var NotationContent = List.empty[String]
	  var index:Int = 0
	  //var test = List.empty[String]
	  def createRuleName(name:String):String = {
	    if (rules.exists(x => x match {case Rule(n,c) => n==name} )) {
	    	createRuleName(name + "_")
	    } else {
	    	name
	    }
	  }
	  def addTopRule(rawName:String, markers:List[Marker]) {
		val content:List[String] = "rowB"::markers.map(addRule):::"rowE"::Nil
	    val topPatt = """\?.*""".r
	    val q = new Regex(("""\?"""),"q")
	    val Some(suff) = topPatt findFirstIn rawName 
	    val notUniqueName = q replaceAllIn (suff, m => "_")
		printf("\nCall for " + notUniqueName +"\n")
		val uniqueName = createRuleName(notUniqueName)
	    NotationContent = uniqueName :: NotationContent
	    rules = rules | Set((Rule(uniqueName,content)))     //uniqueness of top level notations is assumed
	  }
	  def createRule(content:List[String]):String = { //returns the name of the rule
		 val filteredRules = rules.filter(r => r match { case Rule(n,c) => c == content})
	     if (filteredRules.isEmpty) {
	        index = index + 1
	        val name = "rule" + index.toString
	        rules = rules | Set(Rule(name, content))
	        //test = (name+" NEW")::test
	        name
	     } else {
	        val Rule(name,_) = filteredRules.head
	        //test = (name+" OLD")::test
	        name
	     }
	  }
	  def addRuleL(markers:List[Marker]):String  = { //returns the name of the rule
	     val content:List[String] = "rowB"::markers.map(addRule):::"rowE"::Nil
	     createRule(content)
	  }
	  def addRule(marker:Marker):String = marker match { //returns the name of the rule
			case Arg(argNr,precedence) => {
						       precedence match {
						        case Some(x) => 
				 val content = "renderB"::"nrB"::argNr.toString::"nrE"::"prB"::x.toString::"prE"::"renderE"::Nil
				 createRule(content)
						        case None    =>
				 val content = "renderB"::"nrB"::argNr.toString::"nrE"::"renderE"::Nil
				 createRule(content)
						      }
						
				    }
		    case SeqArg(argNr,delim,precedence) => {
				    		val delimName = addRule(delim)
				    		val argName = addRule(Arg(argNr,precedence))
				    		precedence match {
				    		  case Some(x) => 
				     		    createRule(
				     		    "iterateB"::"nrB"::argNr.toString::"nrE"::"prB"::x.toString::"prE"::
				    		    "separatorB"::delimName::"separatorE"::argName::"iterateE"::Nil)
				    		  case None => 
				    		    createRule(
				    		    "iterateB"::"nrB"::argNr.toString::"nrE"::"separatorB"::delimName::
				    		    "separatorE"::argName::"iterateE"::Nil)
				    		}
				    }		
		  	case m : TdMarker => createRule("<TODO>"::Nil)
		    case d : Delimiter => createRule("moB"::"'"+d.text+"'"::"moE"::Nil)
		    case m : GroupMarker => addRuleL(m.elements)
		    case _ => "<TODO>"
	  }
	  
	  def toBNF(rule:Rule):String = {
	    val Rule(name, content) = rule
	    
	    content match {
	      case "rowB"::tl => name + "::= " + content.mkString(" ")  
	      case "moB"::tl => name + "::= " + content.mkString(" ")  
	      case "renderB"::tl => name + "::= Expression"
	      case "iterateB"::tl => 
	        			val Some(delim) = content.find(x=> 
	        			   if (content.indexOf(x)>0) {
	        			      content(content.indexOf(x)-1) == "separatorB"
	        			   } else false)
						name + "::= Expression \n"+ "| " + name + " " + delim + " Expression" 
	      case _ => "<TODO>"
	    }
	  }
	  
	  def getMarpaGrammar:List[String] = {
		    val pref = ":default ::= action => do_print"::
		    			"lexeme default = latm => 1"::
		    			"Error = anyToken"::
		    			"     || anyToken Error"::
		    			":lexeme ~ <anyToken> priority => -1"::
		    			":start ::= Script"::
		    			"Script ::= Notation"::
		    			"         ||Error"::
		    			"Any = zeroPriorityAnyToken"::
		    			"     || zeroPriorityAnyToke Any"::
		    			"""zeroPriorityAnyToken ~ [\s\S]"""::
		    			"Expression = '<' Any '>'"::
		    			"moB ~ '<m:mo>'"::
		    			"moE ~ '</m:mo>'"::
		    			"rowB ~ '<m:mrow>'"::
		    			"rowE ~ '</m:mrow>'"::	
		    			Nil
		    		
		   val extractedRules = Grammar.rules.toList.map(x=>Grammar.toBNF(x)).map(_.toString)
		   NotationContent match {
		      case Nil => "#No notations found"::Nil 
		      case _ => val tl = NotationContent.tail
			   val Notation = tl match {
			      case Nil => "Notation ::= " + NotationContent.head :: Nil
			      case _   => "Notation ::= " + NotationContent.head :: tl.map(x => "| "+x)
			    }
			   pref:::Notation:::extractedRules
		   } 	
	  } 
}
  
  
  
  def getNotations : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
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
          Server.JsonResponse(JSONArray(notations.map(n => JSONArray(toStringMarkers(n))).toList)).aact(tk)
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
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
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
      
      
      Server.JsonResponse(JSONArray(resultNodes.toList)).aact(tk)
    }
  }
  
  private def getRelated : HLet = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
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
      val resultNodes = resultSet foreach {p =>
        controller.get(p) match {
          case s : StructuralElement =>
            pres(s)(rb)
          case _ => 
        }
      }
      val html = if (resultSet.isEmpty) {
        "<div class=\"error\"> No Results Found </div>" 
      } else {
         "<div>" + rb.get + "</div>"
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
      val reqBody = new Body(tk)
      val params = reqBody.asJSON.obj
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
      Server.JsonResponse(JSONObject(response.toMap))     
  }
}