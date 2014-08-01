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
import info.kwarc.mmt.api.notations.PlaceholderDelimiter
import info.kwarc.mmt.api.notations.SymbolName
import info.kwarc.mmt.api.notations.Subs
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

import tiscaf.HTalk

case class PlanetaryError(val text : String) extends Error(text)


class MarpaGrammarGenerator extends ServerExtension("marpa") with Logger {
  override val logPrefix = "marpa"
     /** Server */   
  def apply(uriComps: List[String], query: String, body : Body): HLet = {
    try {
      uriComps match {
        //Here the post request is handled
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
  
  //The post request response is defined here 
  def getGrammarResponse : HLet  = new HLet {
    def aact(tk : HTalk)(implicit ec : ExecutionContext) : Future[Unit] = try {
      val reqBody = new Body(tk)
      val notations = controller.library.getModules flatMap {
        case t : DeclaredTheory 
        	//if t.path.toPath == "http://mathhub.info/smglom/mv/equal.omdoc?equal" 
        	=> 
          val not = t.getDeclarations collect {
            case c : Constant => c.notC.presentationDim.notations.values.flatten.map(not => c.path -> not) //produces (name, notation) pairs
          } 
          not.flatten
        case _ => Nil
      } //notations is now an iterable of (name, notation) pairs
      
      
     
//      val symbol = notations.head.name
//      //val prototype = OMA(OMS(symbol), List(args))
      //prototype.toNode
// 
//       val xmlNotations = notations.map(n => (n.name.toPath, n.presentationMarkers))
//       val resp = new JSONArray(xmlNotations.map(_.toString))
   // 	notations.foreach( x => println(x.arity.length)) PRINT NOTATION ARGUMENT NUMBER
        //	notations.foreach( pair => Grammar.addTopRule(pair._1.toPath, pair._2.presentationMarkers)) //adding rules to the grammar
       
      val raw = notations.toList.zipWithIndex
       raw.foreach( x => Grammar.addTopRule(x._1._1.toPath+"N"+x._2.toString, x._1._2.presentationMarkers)) //adding rules to the grammar
       val resp = new JSONArray( Grammar.getMarpaGrammar );
//     val resp = new JSONArray(Grammar.rules.toList.map(_.toString));
      	val params = reqBody.asJSON
      	Server.JsonResponse(resp).aact(tk)
    }
  }
 

 
  abstract class GenericRule
  case class Rule(name:String, content: List[String]) extends GenericRule
  object Grammar {
      var currentTopRuleNr = ""
	  var rules = Set.empty[Rule]  
	  private var NotationContent = List.empty[String]
	  private var eventList = List.empty[String];
	  private var index:Int = 0  //used to ensure the uniqueness of rule names
	  
	  //Top Rule Name uniqueness cannot be assumed because the path of the notation
	  //can be common for multiple notations - hence "_" is added to differentiate between those
	  //TODO: This is enough for now, however when the original notation is needed - further improvement  will be necessary
	  def createTopRuleName(name:String):String = {
	    if (rules.exists(x => x match {case Rule(n,c) => n==name} )) {
	    	createTopRuleName(name + "_")
	    } else {
	    	name
	    }
	  }
	  def getRuleNr(name:String):String = {
	    val regex = """\d*""".r
	    val Some(pref) = regex findFirstIn name.reverse 
	    pref.reverse
	  }
	  //adds the Top Rule and all of the necessary subrules to the Grammar
	  def addTopRule(rawName:String, markers:List[Marker]) {
		val topPatt = """\?.*""".r
	    val q = """[^a-zA-Z0-9]""".r
	    //rawName is the path of the notation - it is used to ensure topLevelRule name uniqueness
	    val Some(suff) = topPatt findFirstIn rawName 
	    val notUniqueName = q replaceAllIn (suff, m => "_")
		val uniqueName = createTopRuleName(notUniqueName)
	    NotationContent ::= uniqueName 
	    if (!(uniqueName.endsWith("N482")||uniqueName.startsWith("_coset_"))){
	    	eventList ::= "event '"+uniqueName+"' = completed "+uniqueName 
	    }
	    currentTopRuleNr = getRuleNr(uniqueName)
	    val content:List[String] = "topLevel"::markers.map(
	        x => x match { 
	          case Delim(text) => addRule(Delim("#seq_"+text)) 
	          case _ => addRule(x)
	        })
	    rules = rules | Set((Rule(uniqueName,content)))     //uniqueness of top level notations is assumed
	  }
	  
	  
	  //returns the name of the rule, adds the rule to the Set[Rule] only if it is not already there
	  def createRule(content:List[String]):String = { 
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
	  
	  def createArgRule(topRuleNr:String, suff:String):String = { 
		val name = "argRule" + "N"+topRuleNr+"A"+suff
		val filteredRules = rules.filter(r => r match { case Rule(n,c) => n==name})
	     if (filteredRules.isEmpty) {
	        rules = rules | Set(Rule(name, "renderB"::Nil))
	      //  eventList ::= "event '"+name+"' = completed "+name
	        name
	     } else {
	       name
	     }
	  }
	  
	  //returns the name of the rule, adds the rule to the grammar by calling CreateRule
	  //this function performs the recursion on Markers, while CreateRule just checks for uniqueness
	  def addRule(marker:Marker):String = marker match { 
	        
			case Arg(argNr,precedence) => {
						   val content =  precedence match {
						        case Some(x) => 
				"renderB"::"nrB"::argNr.toString::"nrE"::"prB"::x.toString::"prE"::"renderE"::Nil
				
						        case None    =>
				 "renderB"::"nrB"::argNr.toString::"nrE"::"renderE"::Nil
						   }
						   val result = createArgRule(currentTopRuleNr, argNr.toString + "Arg")
						   result
						
				    }
		    case SeqArg(argNr,delim,precedence) => {
		    				val Delim(text) = delim
				    		val delimName = addRule(Delim("#seq_"+text))
				    		val argName :String = createArgRule(currentTopRuleNr, argNr.toString +"Seq")
				    		val content = precedence match {
				    		  case Some(x) => 
				     		   
				     		    "iterateB"::"nrB"::argNr.toString::"nrE"::"prB"::x.toString::"prE"::
				    		    "separatorB"::delimName::"separatorE"::argName ::"iterateE"::Nil
				    		  case None => 
				    	
				    		    "iterateB"::"nrB"::argNr.toString::"nrE"::"separatorB"::delimName::
				    		    "separatorE"::argName::"iterateE"::Nil
				    		}
				    		val result = createRule(content)
				    		result
				    }		
		  	case m : TdMarker => 
		  	  val content = m.content map addRule
		  	  if (content.isEmpty) {
		  	    createRule("mtdSingle"::Nil)
		  	  } else {
		  	      val contentRule = createRule("contentRule"::content)
		  		  createRule("mtdB"::contentRule::"mtdE"::Nil)
		  	  }
		  	  
		  	case m : TrMarker => 
		  	  val content = m.content map addRule
		  	  if (content.isEmpty) {
		  	    createRule("mtrSingle"::Nil)
		  	  } else {
		  	      val contentRule = createRule("contentRule"::content)
		  		  createRule("mtrB"::contentRule::"mtrE"::Nil)
		  	  }
		  	case m : TableMarker =>
		  	  val content = m.content map addRule
		  	  if (content.isEmpty) {
		  	    createRule("mtableSingle"::Nil)
		  	  } else {
		  	      val contentRule = createRule("contentRule"::content)
		  		  createRule("mtableB"::contentRule::"mtableE"::Nil)
		  	  }	  
		  	  //////////////////////////////////////////////////////////////
		  	case NumberMarker(d) =>
		  	  var text = d.text
		  	  var flag = ""
		  	  text = if (text.startsWith("#seq_")) {
		  	    flag = "#seq_" 
		  	    text.substring(5)
		  	  } else {
		  	    text
		  	  }
		  	   if ( text == "" ) {
		         text = " "
		  	   }
		  
		        if (text == "⁢"//unicode 
		            && flag == "") { //handling the invisible unicode char used for function application
		        val v1 = createRule("empty"::Nil)
		        val v2 = createRule("mnB"::text.toString::"mnE"::Nil)
		        createRule("alternatives"::v1::v2::Nil)
		      } else {
		    	  createRule("mnB"::text.toString::"mnE"::Nil)
		      }
		  	case Delim(rawW) =>
		  	  //first check if this Delim is from a Sequence Argument - if so the rule
		  	  //cannot be nullable
		  	  var flag = ""
		  	  var w = if (rawW.startsWith("#seq_")) {
		  	    flag = "#seq_" 
		  	    rawW.substring(5)
		  	  } else {
		  	    rawW
		  	  }
		  	  if (w=="&#40;") { w = "(" }
		  	  if (w=="&#41;") { w = ")" }
		  	  if (w.startsWith("#num_")) {
		  	    addRule(NumberMarker(Delim(flag+w.substring(5))))
		  	  } else if (w.startsWith("#id_")){
		  	    addRule(IdenMarker(Delim(flag+w.substring(4))))
		  	  } else {
			  	  var text = w
			  	  if ( text == "") {
			        text = " "
			          
			      } 
			       if (text == """⁢""" //unicode
			         && flag == "") { //handling the invisible unicode char used for function application
			        val v1 = createRule("empty"::Nil)
			         val v2 = createRule("moB"::text.toString::"moE"::Nil)
			        createRule("alternatives"::v1::v2::Nil)
			      } else {
			    	  createRule("moB"::text.toString::"moE"::Nil)
			      }
		  	  }
		  	case IdenMarker(d) =>
		  	  var text = d.text
		  	  var flag = ""
		  	  text = if (text.startsWith("#seq_")) {
		  	    flag = "#seq_" 
		  	    text.substring(5)
		  	  } else {
		  	    text
		  	  }
		  	   if ( text == "" ) {
		         text = " "
		  	   }
		      if (text == """⁢""" //unicode
		        && flag == "") { //handling the invisible unicode char used for function application
		        val v1 = createRule("empty"::Nil)
		         val v2 = createRule("miB"::text.toString::"miE"::Nil)
		        createRule("alternatives"::v1::v2::Nil)
		      } else {
		    	  createRule("miB"::text.toString::"miE"::Nil)
		      }
              /////////////////////////////////////////////////////
		    case m : GroupMarker => 
		    				  val content:List[String] = m.elements.map(addRule)
		    				  if (content.isEmpty) {
			    				  createRule("mrowSingle"::Nil)
		    				  } else {
		    				      val result = createRule("Group"::content)
			    				  result
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
		     		  case Some(m) =>  { hasSub = true
		     		  					addRule(m)}
		     		  case _ => "none_sub"
		     		}
		     	val supRule = s.sup match {
		     		  case Some(m) => { hasSup = true
		     				  		   addRule(m)
		     		  }
		     		  case _ =>  "none_sup"
		     		    
		     	}
		        
		     	val overRule =  s.over match {
		     		  case Some(m) => { hasOver = true
		     			  				addRule(m)}  
		     		  case _ => "none_over"
		        }
		     		
		     	val underRule = s.under match {
		     		  case Some(m) => { hasUnder = true
		     			  				addRule(m)}
		     		  case _ => "none_under" 
		     	}
		       val result = if (hasSup && !hasSub ) {
		          createRule("msupB"::mainRule::supRule::"msupE"::Nil)
		       } else if (!hasSup && hasSub) {
		         createRule("msubB"::mainRule::subRule::"msubE"::Nil)
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
		          createRule("moverB"::mainRule::overRule::"moverE"::Nil)
		       } else if (!hasOver && hasUnder) {
		          createRule("munderB"::mainRule::underRule::"munderE"::Nil)
		       } else if ( hasOver && hasUnder) {
		          val v1 = createRule("munderB"::
		             "moverB"::mainRule::overRule::"moverE"::
		             underRule::"munderE"::Nil)
		          val v2 = createRule("moverB"::
		             "munderB"::mainRule::underRule::"munderE"::
		             overRule::"moverE"::Nil)
		          val v3 = createRule(
		        "munderoverB"::mainRule::underRule::overRule::"munderoverE"::Nil)
		          createRule( "alternatives"::v1::v2::v3::Nil)
		           
		       } else if (!hasSub && !hasSup && !hasUnder && !hasOver){
		         mainRule
		       } else {
		         "SCRIPTMARKER_FAIL"
		       }
		       
		       result
		   case  Var(nr, false, None, precOp) =>  addRule(Arg(nr,precOp))
		   case Var(nr,false,Some(delim),precOp) => addRule(SeqArg(nr,delim,precOp))
		   case Subs(nr,precOp) =>  addRule(Arg(nr,precOp))
		   case SqrtMarker(ml) => 
		      createRule("msqrtB"::ml.map(addRule):::"msqrtE"::Nil)
		    case _ =>
		      println("ERROR: TODO Marker: " + marker.toString)
		      "'TODO'"
	  }
	  
	  //converts from Set(Rule(String,List[String])) (Rules stores as tokenized strings) to List[String] (List of real grammar rules)
 
	  def toBNF(rule:Rule):String = {
	    val Rule(name, content) = rule
	    
	    content match {
	      case "ref"::to::Nil => name + "::= " + to
	      case "topLevel"::tl => 
	        val rest = tl.mkString(" ")
	        if (rest == "") {
	        	"#Ignored top level rule (no rendering)"
	        } else {
	        	name + "::= " + tl.mkString(" ")
	        }
	      case "contentRule"::tl => name + "::= " + tl.mkString(" ") 
	      case "Group"::tl => name + "::= " + tl.mkString(" ") 
	      case "empty"::Nil => name + "::= " + "#Empty rule" 
	      case List("moB",text,"moE") => name + "::= " + "moB '" + text + "' moE" 
	      case List("miB",text,"miE") => name + "::= " + "miB '" + text + "' miE" 
	      case List("mnB",text,"mnE") => name + "::= " + "mnB '" + text + "' mnE" 
	      case "renderB"::tl => name + "::= argRule" 
	      case "iterateB"::tl => 
	        			val Some(delim) = content.find(x=> 
	        			   if (content.indexOf(x)>0) {
	        			      content(content.indexOf(x)-1) == "separatorB"
	        			   } else false)
	        			val Some(argName) = content.find(x=> 
	        			   if (content.indexOf(x)>0) {
	        			      content(content.indexOf(x)-1) == "separatorE"
	        			   } else false)
					//	name + "::= " +  argName + " " + delim + " " + name + " || " + argName + " " + delim + " " + argName
						name + "::= " + argName + " "+delim+" "+ name +"_  \n"+ name +"_::= " +argName+" | "+argName + " " + delim + " "  + name+"_"
						//name + "::= " +  "Expression" + " " + delim + " " + name + " || " + "Expression" + " " + delim + " " + "Expression"
		
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
	      case  "alternatives"::tl => 
	         name + "::= " + (tl mkString (" | "))
	      case "mtdB"::contentRule::"mtdE"::Nil => name + "::= " + content.mkString(" ") 
	      case "mtrB"::contentRule::"mtrE"::Nil => name + "::= " + content.mkString(" ") 
	      case "mtableB"::contentRule::"mtableE"::Nil => name + "::= " + content.mkString(" ")
	      case "mtdSingle"::Nil => name + "::= mtdSingle"
	      case "mtrSingle"::Nil => name + "::= mtrSingle"
	      case "mtableSingle"::Nil => name + "::= mtableSingle"
	      case "mrowSingle"::Nil => name + "::= mrowSingle"
	      case "msqrtB"::tl => name + "::= " + content.mkString(" ") 
	      case List("mfracB",above,below,"mfracE") => name + "::= " + content.mkString(" ") 
	      case _ => println ("TODO BNF:"+ (content mkString " ")) 
	      			"'TODO'"
	    }
	  }
	  
	  //creates the grammar as a List[String] by adding a manually written prefix that gives the Grammar 
	  //an overall shape and also includes presentation MathML parsing
	  //the actual grammar is assembled in Perl by inserting '\n' in between the elements of the List.
	  def getMarpaGrammar:List[String] = {
		    val pref =  "#Manually generated part"::
		    			":default ::= action => [name, start, length, values]":: // by default any rule will return the string it matched
		    			"lexeme default = latm => 1"::       
//		    			"Error ::= anyChar"::                  2
//		    			"       || anyChar Error"::
//		    			"""anyChar ~ [\s\S]"""::
		    			//":lexeme ~ <anyChar> priority => -1":: //otherwise nothing other than Error will ever match
		    			":start ::= Expression "::                   // :start does not allow alternatives
		    			"ExpressionList ::= Expression+"::
		    		//	"| Expression "::
		    			"Expression ::= Notation   "::
		    			"             | Presentation "::
     	    	//		" 			  | Content "::
		    			"Presentation ::= mrowB ExpressionList mrowE "::
		    			" | moB '(' moE ExpressionList moB ')' moE "::
		    			" | moB text moE "::
		    			" | miB text miE "::
		    			" | mnB text mnE "::
		    			" | emB text emE "::
		    			" | mtextB ExpressionList mtextE"::
		    			" | mtextB text mtextE "::
		    			" | msB text msE "::
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
		    			" | emB ExpressionList emE "::
		    			" | mstyleB ExpressionList mstyleE "::
		    			" | mspaceB mspaceE "::
		    			" | miSingle "::
		    			" | moSingle "::
		    			" | mtdSingle " ::
		    			" | mtrSingle " ::
		    			" | mtableSingle "::
		    			" | mrowSingle "::
		    		//	" || texts "::
//		    			"mfracB ::= ws '<mfrac' attribs '>' ws"::"mfracE ::= ws '</mfrac>' ws"::
//		    			"msqrtB ::= ws '<msqrt' attribs '>' ws"::"msqrtE ::= ws '</msqrt>' ws"::
//		    			"msupB ::= ws '<msup' attribs '>' ws"::"msupE ::= ws '</msup>' ws"::
//		    			"msubB ::= ws '<msub' attribs '>' ws"::"msubE ::= ws '</msub>' ws"::
//		    			"munderB ::= ws '<munder' attribs '>' ws"::"munderE ::= ws '</munder>' ws"::
//		    			"moverB ::= ws '<mover' attribs '>' ws"::"moverE ::= ws '</mover>' ws"::
//		    			"mnB ::= ws '<mn' attribs '>' ws"::"mnE ::= ws '</mn>' ws"::
//		    			"miB ::= ws '<mi' attribs '>' ws"::"miE ::= ws '</mi>' ws"::
//		    			"msB ::= ws '<ms' attribs '>' ws"::"msE ::= ws '</ms>' ws"::
//		    			"mspaceB ::= ws '<mspace' attribs '>' ws"::"mspaceE ::= ws '</mspace>' ws"::
//		    			"moB ::= ws '<mo' attribs '>' ws"::"moE ::= ws '</mo>' ws"::
//		    			"mstyleB ::= ws '<mstyle' attribs '>' ws"::"mstyleE ::= ws '</mstyle>' ws"::
//		    			"mtextB ::= ws '<mtext' attribs '>' ws"::"mtextE ::= ws '</mtext>' ws"::
//		    			"emB ::= ws '<em' attribs '>' ws"::"emE ::= ws '</em>' ws"::
//		    			"mtdB ::= ws '<mtd' attribs '>' ws"::"mtdE ::= ws '</mtd>' ws"::
//		    			"mtrB ::= ws '<mtr' attribs '>' ws"::"mtrE ::= ws '</mtr>' ws"::
//		    			"mtableB ::= ws '<mtable' attribs '>' ws"::"mtableE ::= ws '</mtable>' ws"::
//		    			"msubsupB ::= ws '<msubsup' attribs '>' ws"::"msubsupE ::= ws '</msubsup>' ws"::
//		    			"munderoverB ::= ws '<munderover' attribs '>' ws"::"munderoverE ::= ws '</munderover>' ws"::
//		    			"""mrowB ::= ws '<mrow' attribs '>' ws""":: """mrowE ::= ws '</mrow>' ws"""::
//		    			"mathB ::= ws '<math' attribs '>' ws"::"mathE ::= ws '</math>' ws"::
//		    			"miSingle ::= ws '<mi' attribs '/>' ws "::
//////////////////////////////////////////////////////////////
		    		    "mfracB ::=  '<mfrac' attribs '>' "::"mfracE ::=  '</mfrac>' "::
		    			"msqrtB ::=  '<msqrt' attribs '>' "::"msqrtE ::=  '</msqrt>' "::
		    			"msupB ::=  '<msup' attribs '>' "::"msupE ::=  '</msup>' "::
		    			"msubB ::=  '<msub' attribs '>' "::"msubE ::=  '</msub>' "::
		    			"munderB ::=  '<munder' attribs '>' "::"munderE ::=  '</munder>' "::
		    			"moverB ::=  '<mover' attribs '>' "::"moverE ::=  '</mover>' "::
		    			"mnB ::=  '<mn' attribs '>' "::"mnE ::=  '</mn>' "::
		    			"miB ::=  '<mi' attribs '>' "::"miE ::=  '</mi>' "::
		    			"msB ::=  '<ms' attribs '>' "::"msE ::=  '</ms>' "::
		    			"mspaceB ::=  '<mspace' attribs '>' "::"mspaceE ::=  '</mspace>' "::
		    			"moB ::=  '<mo' attribs '>' "::"moE ::=  '</mo>' "::
		    			"mstyleB ::=  '<mstyle' attribs '>' "::"mstyleE ::=  '</mstyle>' "::
		    			"mtextB ::=  '<mtext' attribs '>' "::"mtextE ::=  '</mtext>' "::
		    			"emB ::=  '<em' attribs '>' "::"emE ::=  '</em>' "::
		    			"mtdB ::=  '<mtd' attribs '>' "::"mtdE ::=  '</mtd>' "::
		    			"mtrB ::=  '<mtr' attribs '>' "::"mtrE ::=  '</mtr>' "::
		    			"mtableB ::=  '<mtable' attribs '>' "::"mtableE ::=  '</mtable>' "::
		    			"msubsupB ::=  '<msubsup' attribs '>' "::"msubsupE ::=  '</msubsup>' "::
		    			"munderoverB ::=  '<munderover' attribs '>' "::"munderoverE ::=  '</munderover>' "::
		    			"""mrowB ::=  '<mrow' attribs '>' """:: """mrowE ::=  '</mrow>' """::
		    			"mathB ::=  '<math' attribs '>' "::"mathE ::=  '</math>' "::
		    			"miSingle ::=  '<mi' attribs '/>'  "::
		    			"moSingle ::=  '<mo' attribs '/>'  "::
		    			"mtdSingle ::=  '<mtd' attribs '/>'  "::
		    			"mtrSingle ::=  '<mtr' attribs '/>'  "::
		    			"mtableSingle ::=  '<mtable' attribs '/>'  "::
		    			"mrowSingle ::=  '<mrow' attribs '/>'  "::
/////////////////////////////////////////////////////////////
		    			"ws ::= spaces"::
		    			"ws ::= # empty"::
		    		    """spaces ~ space+"""::
		    			"""space ~ [\s] """::
		    			"attribs ::=  ws"::
                        "attribs::= attribRule"::
                        "attribRule ::=  attrib || attrib attribRule"::
		    			""" attrib  ::= ws notEqSignS '=' ws '"' notQuoteS '"' ws"""::
		    			"notEqSignS ~ notEqSign+ "::
		    			"notEqSign ~ [^=<>/]"::
		    			"notQuoteS ~ notQuote+ "::
		    			"""notQuote ~ [^"<>]"""::
		    			""" text ::= textRule""":: 
		    			"""textRule::= char | char textRule """::
		    			""" text ::= #empty"""::
		    			""" char ~ [^<>]"""::
		    		//	":lexeme ~ <text> priority => -1":: 
		    			""" argRule ::= Expression # Presentation |Content """::
// argRule is not defined as Expression since this would increase the ambiguity of the grammar
// + we don't care whether an argument contains a notation		    			
//		    			"Notation ::= my_opPlusN12345 #my_plus" ::
//		    			"argRuleNmyPlus ::= Expression "::
//		    		    "my_opPlusN12345 ::= argRuleNmyPlus  my_plus argRuleNmyPlus | argRuleNmyPlus my_plus my_opPlusN12345":: //TODO: remove when the errors related to generating it from omdocs is fixed
//		    		    "event 'my_opPlusN12345' = completed my_opPlusN12345"::
//		    		    "my_plus ::= moB '+' moE"::
		    			"#Automatically generated part"::
		    			Nil
		    			
		    		
		   val extractedRules = Grammar.rules.toList.map(x=>Grammar.toBNF(x)).map(_.toString)

		   NotationContent match {
		      case Nil => "#No notations found"::Nil 
		      case _ => val tl = NotationContent.tail
			   val Notation = tl match {
			      case Nil => "Notation ::= " + NotationContent.head :: Nil
			      case _   => val r = Grammar.rules.toList 
					         "Notation ::= " + NotationContent.head :: tl.map(x => 
					          if (x.endsWith("N482")||
					              x.startsWith("_coset_")) {
					           "#Ommited top level rule"
					         } else {
					        	 "| " + x
					         })
			    }
		   val result = pref:::Notation:::extractedRules :::eventList
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