package info.kwarc.mmt.marpa

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

import tiscaf.HTalk

case class PlanetaryError(val text : String) extends Error(text)


class MarpaGrammarGenerator extends ServerExtension("marpa") with Logger {
  override val logPrefix = "marpa"
     /** Server */   
  def apply(uriComps: List[String], query: String, body : Body): HLet = {
    try {
      uriComps match {
        case "getGrammar" :: _ => getGrammarResponse
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
  
  def getGrammarResponse : HLet  = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val notations = controller.library.getModules flatMap {
        case t : DeclaredTheory 
        	//if t.path.toPath == "http://mathhub.info/smglom/mv/equal.omdoc?equal"
        	=> 
          val not = t.getDeclarations collect {
            case c : Constant => c.notC.presentationDim.notations.values.flatten.map(not => c.path -> not) //@Ion, changed it to produce (name, notation) pairs
          } 
          not.flatten
        case _ => Nil
      } //notations is now iterable of (name, notation) pairs
      
      
     
//      val symbol = notations.head.name
//      //val prototype = OMA(OMS(symbol), List(args))
      //prototype.toNode
// 
//       val xmlNotations = notations.map(n => (n.name.toPath, n.presentationMarkers))
//       val resp = new JSONArray(xmlNotations.map(_.toString))
   // notations.foreach( x => println(x.arity.length)) PRINT NOTATION ARGUMENT NUMBER
      notations.foreach(pair => Grammar.addTopRule(pair._1.toPath, pair._2.presentationMarkers)) //@Ion see modification here and in notation variable above 
      val resp = new JSONArray( Grammar.getMarpaGrammar );
//     val resp = new JSONArray(Grammar.rules.toList.map(_.toString));
     val params = reqBody.asJSON
      Server.JsonResponse(resp).aact(tk)
    }
  }
 

 
  abstract class GenericRule
  case class Rule(name:String, content: List[String]) extends GenericRule
  object Grammar {
	  //private
	  var rules = Set.empty[Rule]
	  private var NotationContent = List.empty[String]
	  private var eventList = List.empty[String];
	  private var index:Int = 0
	  //private var test = List.empty[String]
	  def createRuleName(name:String):String = {
	    if (rules.exists(x => x match {case Rule(n,c) => n==name} )) {
	    	createRuleName(name + "_")
	    } else {
	    	name
	    }
	  }
	  def addTopRule(rawName:String, markers:List[Marker]) {
		val content:List[String] = "topLevel"::markers.map(addRule)
	    val topPatt = """\?.*""".r
	    val q = """[\?-]""".r
	    val Some(suff) = topPatt findFirstIn rawName 
	    val notUniqueName = q replaceAllIn (suff, m => "_")
		val uniqueName = createRuleName(notUniqueName)
	    NotationContent ::= uniqueName 
	    eventList ::= "event '"+uniqueName+"' = completed "+uniqueName
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
		  	case m : TdMarker => 
		  	  val contentRule = createRule("contentRule"::(m.content map addRule))
		  	  createRule("mtdB"::contentRule::"mtdE"::Nil)
		  	case m : TrMarker => 
		  	  val contentRule = createRule("contentRule"::(m.content map addRule))
		  	  createRule("mtrB"::contentRule::"mtrE"::Nil)
		  	case m : TableMarker =>
		  	  val contentRule = createRule("contentRule"::(m.content map addRule))
		  	  createRule("mtableB"::contentRule::"mtableE"::Nil)
		    case d : Delimiter => 
		      if ( d.text == "") {
		        createRule("moB"::"moE"::Nil)
		      } else
		      createRule("moB"::"'"+d.text+"'"::"moE"::Nil)
		    
		    case m : GroupMarker => {
		    				  val content:List[String] = m.elements.map(addRule)
		    				  createRule("Group"::content)
		    		}
		    case f : FractionMarker => 
		      val above = addRule(GroupMarker(f.above))
		      val below = addRule(GroupMarker(f.below))
		      createRule(List("mfracB",above,below,"mfracE"))
		   case s : ScriptMarker => 
		     	val mainRule = addRule(s.main)
		        var hasSup = false
		        var hasSub = false
		        var hasUnder = false
		        var hasOver = false
		     	val subRule = s.sub match {
		     		  case Some(m) =>  { hasSup = true
		     		  					addRule(m)}
		     		  case _ => ""
		     		}
		     	val supRule = s.sup match {
		     		  case Some(m) => { hasSub = true
		     				  		   addRule(m)
		     		  }
		     		  case _ =>  ""
		     	}
		        
		     	val overRule =  s.over match {
		     		  case Some(m) => { hasOver = true
		     			  				addRule(m)}  
		     		  case _ => ""
		        }
		     		
		     	val underRule = s.under match {
		     		  case Some(m) => { hasUnder
		     			  				addRule(m)}
		     		  case _ => "" 
		     	}
		     	
		     	
		  
		       if (hasSup && !hasSub ) {
		          createRule("msubB"::mainRule::subRule::"msubE"::Nil)
		       } else if (!hasSup && hasSub) {
		         createRule("msupB"::mainRule::subRule::"msupE"::Nil)
		       } else if (hasSup && hasSub) {
		          val v1 = createRule("msubB"::
		             "msupB"::mainRule::supRule::"msupE"::
		             subRule::"msubE"::Nil)
		          val v2 = createRule("msupB"::
		             "msubB"::mainRule::subRule::"msubE"::
		             supRule::"msupE"::Nil)
		          val v3 = createRule("msubsupB"::
		              mainRule::subRule::supRule::"msubsupE"::Nil)
		          createRule( "alternatives"::v1::v2::v3::Nil)
		           
		       } else if (hasOver && !hasUnder ) {
		          createRule("moverB"::mainRule::underRule::"moverE"::Nil)
		       } else if (!hasOver && hasUnder) {
		          createRule("munderB"::mainRule::underRule::"munderE"::Nil)
		       } else  { //(hasOver && hasUnder) {
		          val v1 = createRule("munderB"::
		             "moverB"::mainRule::overRule::"moverE"::
		             underRule::"munderE"::Nil)
		          val v2 = createRule("moverB"::
		             "munderB"::mainRule::underRule::"munderE"::
		             overRule::"moverE"::Nil)
		          val v3 = createRule(
		 "munderoverB"::mainRule::underRule::overRule::"munderoverE"::Nil)
		          createRule( "alternatives"::v1::v2::v3::Nil)
		           
		       }
		    case _ =>
		      println("TODO Marker: " + marker.toString)
		      "'TODO'"
	  }
	  
	  def toBNF(rule:Rule):String = {
	    val Rule(name, content) = rule
	    
	    content match {
	      case "topLevel"::tl => name + "::= " + tl.mkString(" ") 
	      case "contentRule"::tl => name + "::= " + tl.mkString(" ") 
	      case "Group"::tl => name + "::= " + tl.mkString(" ") 
	      case "moB"::tl => name + "::= " + content.mkString(" ") 
	      case "renderB"::tl => name + "::= Expression || texts" //todo -> think of a better base case for arguments
	      case "iterateB"::tl => 
	        			val Some(delim) = content.find(x=> 
	        			   if (content.indexOf(x)>0) {
	        			      content(content.indexOf(x)-1) == "separatorB"
	        			   } else false)
	        			val Some(argName) = content.find(x=> 
	        			   if (content.indexOf(x)>0) {
	        			      content(content.indexOf(x)-1) == "separatorE"
	        			   } else false)
						name + "::= " +  argName + " " + delim + " " + name + " || " + argName + " " + delim + " " + argName
		 //TODO: msupB msubB (/) moverB munderB (/) msubsupB munderoverB alternatives
	      case "msubB"::mainRule::subRule::"msubE"::Nil => 
	        name + "::= " + (content mkString " ")
	      case "msupB"::mainRule::subRule::"msupE"::Nil =>
	        name + "::= " + (content mkString " ")
	      case "msubB"::"msupB"::mainRule::supRule::"msupE"::
		     subRule::"msubE"::Nil   =>
		     name + "::= " + (content mkString " ")
	      case "msupB"::"msubB"::mainRule::subRule::"msubE"::
		     supRule::"msupE"::Nil   =>
		      name + "::= " + (content mkString " ")
	      case "msubsupB"::mainRule::subRule::supRule::"msubsupE"::Nil =>
	        name + "::= " + (content mkString " ")
	      case "moverB"::mainRule::underRule::"moverE"::Nil =>
	        name + "::= " + (content mkString " ")
	      case "munderB"::mainRule::underRule::"munderE"::Nil =>
	        name + "::= " + (content mkString " ")
	      case "munderB"::"moverB"::mainRule::overRule::"moverE"::
		       underRule::"munderE"::Nil =>
		         name + "::= " + (content mkString " ")
	      case "moverB"::"munderB"::mainRule::underRule::"munderE"::
		       overRule::"moverE"::Nil =>
		         name + "::= " + (content mkString " ")
	      case "munderoverB"::mainRule::underRule::overRule::"munderoverE"::Nil =>
	        name + "::= " + (content mkString " ")
	      case  "alternatives"::v1::v2::v3::Nil => 
	         name + "::= " + (List(v1,v2,v3) mkString (" | "))
	      case "mtdB"::contentRule::"mtdE"::Nil => name + "::= " + content.mkString(" ") 
	      case "mtrB"::contentRule::"mtrE"::Nil => name + "::= " + content.mkString(" ") 
	      case "mtableB"::contentRule::"mtableE"::Nil => name + "::= " + content.mkString(" ") 
	      case List("mfracB",above,below,"mfracE") => name + "::= " + content.mkString(" ") 
	      case _ => println ("TODO BNF:"+ (content mkString " ")) 
	      			"'TODO'"
	    }
	  }
	  
	  def getMarpaGrammar:List[String] = {
		    val pref =  "#Manually generated part"::
		    			":default ::= action => getString":: // by default any rule will return the string it matched
		    			"lexeme default = latm => 1"::       
//		    			"Error ::= anyChar"::                  
//		    			"       || anyChar Error"::
//		    			"""anyChar ~ [\s\S]"""::
		    			//":lexeme ~ <anyChar> priority => -1":: //otherwise nothing other than Error will ever match
		    			":start ::= Expression "::                   // :start does not allow alternatives
		    			"ExpressionList ::= Expression ExpressionList "::
		    			"| Expression "::
		    			"Expression ::= Notation "::
		    			"            || Presentation "::
     	    	//		" 			  | Content "::
		    			"Presentation ::= mrowB ExpressionList mrowE"::
		    			" | moB '(' moE ExpressionList moB ')' moE "::
		    			" | moB Expression moE "::
		    			" | miB Expression miE "::
		    			" | mnB Expression mnE "::
		    			" | mfracB ExpressionList mfracE"::
		    			" | msqrtB Expression msqrtE"::
		    			" | msupB ExpressionList msupE" ::
		    			" | msubB ExpressionList msubE" ::
		    			" | msubsupB ExpressionList msubsupE"::
		    			" | munderB ExpressionList munderE "::
		    			" | moverB ExpressionList moverE "::
		    			" | munderoverB ExpressionList munderoverE "::
		    			" | mtdB ExpressionList mtdE" ::
		    			" | mtrB ExpressionList mtrE" ::
		    			" | mtableB ExpressionList mtableE "::
		    			" | mathB ExpressionList mathE "::
		    			" | mtextB ExpressionList mtextE "::
		    			" | emB ExpressionList emE "::
		    			" | mstyleB ExpressionList mstyleE "::
		    			" | miSingle "::
		    			" || texts "::
		    			"mfracB ::= ws '<mfrac' attribs '>' ws"::"mfracE ::= ws '</mfrac>' ws"::
		    			"msqrtB ::= ws '<msqrt' attribs '>' ws"::"msqrtE ::= ws '</msqrt>' ws"::
		    			"msupB ::= ws '<msup' attribs '>' ws"::"msupE ::= ws '</msup>' ws"::
		    			"msubB ::= ws '<msub' attribs '>' ws"::"msubE ::= ws '</msub>' ws"::
		    			"munderB ::= ws '<munder' attribs '>' ws"::"munderE ::= ws '</munder>' ws"::
		    			"moverB ::= ws '<mover' attribs '>' ws"::"moverE ::= ws '</mover>' ws"::
		    			"mnB ::= ws '<mn' attribs '>' ws"::"mnE ::= ws '</mn>' ws"::
		    			"miB ::= ws '<mi' attribs '>' ws"::"miE ::= ws '</mi>' ws"::
		    			"moB ::= ws '<mo' attribs '>' ws"::"moE ::= ws '</mo>' ws"::
		    			"mstyleB ::= ws '<mstyle' attribs '>' ws"::"mstyleE ::= ws '</mstyle>' ws"::
		    			"mtextB ::= ws '<mtext' attribs '>' ws"::"mtextE ::= ws '</mtext>' ws"::
		    			"emB ::= ws '<em' attribs '>' ws"::"emE ::= ws '</em>' ws"::
		    			"mtdB ::= ws '<mtd' attribs '>' ws"::"mtdE ::= ws '</mtd>' ws"::
		    			"mtrB ::= ws '<mtr' attribs '>' ws"::"mtrE ::= ws '</mtr>' ws"::
		    			"mtableB ::= ws '<mtable' attribs '>' ws"::"mtableE ::= ws '</mtable>' ws"::
		    			"msubsupB ::= ws '<msubsup' attribs '>' ws"::"msubsupE ::= ws '</msubsup>' ws"::
		    			"munderoverB ::= ws '<munderover' attribs '>' ws"::"munderoverE ::= ws '</munderover>' ws"::
		    			"""mrowB ::= ws '<mrow' attribs '>' ws""":: """mrowE ::= ws '</mrow>' ws"""::
		    			"mathB ::= ws '<math' attribs '>' ws"::"mathE ::= ws '</math>' ws"::
		    			"miSingle ::= ws '<mi' attribs '/>' ws "::
		    			"ws ::= spaces"::
		    			"ws ::= # empty"::
		    		    """spaces ~ space+"""::
		    			"""space ~ [\s] """::
		    			"attribs ::= ws || attrib || attrib attribs "::
		    			""" attrib  ::= ws notEqSignS '=' ws '"' notQuoteS '"' ws"""::
		    			"notEqSignS ~ notEqSign+ "::
		    			"notEqSign ~ [^=<>/]"::
		    			"notQuoteS ~ notQuote+ "::
		    			"""notQuote ~ [^"<>]"""::
		    			""" texts ~ text+"""::
		    			""" text ~ [^<>]"""::
		    			"Notation ::= my_opPlus #my_plus" ::
		    		    "my_opPlus ::= Expression  my_plus Expression | Expression my_plus my_opPlus"::
		    		    "event 'my_opPlus' = completed my_opPlus"::
		    		    "my_plus ::= moB '+' moE"::
		    			"#Automatically generated part"::
		    			Nil
		    			
		    		
		   val extractedRules = Grammar.rules.toList.map(x=>Grammar.toBNF(x)).map(_.toString)
		   NotationContent match {
		      case Nil => "#No notations found"::Nil 
		      case _ => val tl = NotationContent.tail
			   val Notation = tl match {
			      case Nil => "Notation ::= " + NotationContent.head :: Nil
			      case _   => "Notation ::= " + NotationContent.head :: tl.map(x => "| "+x)
			    }
		   val result = pref:::Notation:::extractedRules:::eventList
		   //clean up   
		   eventList = List[String]();
		   rules = Set[Rule]();
		   NotationContent = List.empty[String]
		   index = 0;
		   result
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
}