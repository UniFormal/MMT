package info.kwarc.mmt.api.parser


import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.frontend._
import scala.collection.immutable.{HashMap}

/** a ParsingUnit represents a term that is to be parsed
 * @param component the URI of the Term, if any; may be null
 * @param scope the theory against which to parse
 * @param context the context against which to parse
 * @param term the term to be parse
 * */
case class ParsingUnit(component: CPath, scope: Term, context: Context, term: String)

/** a TermParser parses Term's. Instances are maintained by the ExtensionManager and retrieved and called by the structural parser. */
abstract class TermParser {
   def apply(pu: ParsingUnit): Term
}

/** A default parser that parses any string into an OMSemiFormal object. */
object DefaultParser extends TermParser {
   def apply(pu: ParsingUnit): Term = OMSemiFormal(Text(pu.scope.toMPath.toPath, pu.term))
   
   def getNotations(controller : Controller, scope : Term) : List[TextNotation] = {
     val includes = controller.library.visible(scope)
     val decls = includes.toList flatMap {tm => 
       controller.globalLookup.get(tm.toMPath).components
     }
     decls collect {
       case c : Constant => c.not match {
         case Some(not) => not 
         case None => new TextNotation(c.path, List(Delim(c.name.toPath)), 0)
       }
     }
   }
}

/**
 * Generic Error Class
 */
object ParsingError {
  def apply(msg : String, sr : SourceRegion) = SourceError("object-parser", SourceRef(null, sr), msg) //TODO container missing
   //("Parsing Error: " + msg + " at (" + start + ","+ end + ")") {
  //def stackTrace : String = getStackTrace.map(x => x.toString).mkString("","\n","")
}


/**
 * Default implementation for a notation based parser
 */
class NotationParser(controller : Controller) extends TermParser {
    
  /**
   * builds parsers notation context based on content in the controller 
   * and the scope (base path) of the parsing unit
   */
  def buildNotations(scope : Term) : List[(Int,List[TextNotation])] = {
    val notations = DefaultParser.getNotations(controller, scope)  
    val qnotations = notations.groupBy(x => x.priority)
    qnotations.toList
  } 
  
  /**
   * transforms a TokenList (from scanner) to a MMT Term by using parser dictionary 
   */
  def makeTerm(tl : TokenList) : Term = {
    val tms = tl.getTokens.map(makeTerm)
    tms match {
      case Nil => throw ImplementationError("Parser->makeTerm : Empty token list")
      case hd :: Nil => hd
      case l => OMSemiFormal(l.map(Formal))
    }
  }
  
  /**
   * transforms a token list element to an MMT Term
   */
  def makeTerm(tk : TokenListElem) : Term = tk match {
    case Token(word, _, _) => OMSemiFormal(Text("unknown", word))
    case ml : MatchedList => 
      println(ml.an.getFound)
      ml.tokens match {
      case Nil => OMID(ml.an.notation.name)
      case _ => OMA(OMID(ml.an.notation.name), ml.tokens.map(makeTerm))
    }
    case ul : UnscannedList => makeTerm(ul.tl)
  }
  
  def apply(pu : ParsingUnit) : Term = {    
    //gathering notations in scope
    val qnotations = buildNotations(pu.scope)    
    
    //logging
    println("Started parsing " + pu.term + " with operators : ")
    qnotations.map(o => println(o.toString))
    
    //scanning
    val sc = new Scanner(TokenList(pu.term))
    qnotations reverseMap {
         case (priority,nots) => sc.scan(nots)
    }
    println("#####   scanning finished   #####")
    println(sc.tl)
    
    //structuring
    val tm = makeTerm(sc.tl)
    println("##### term :"  + tm.toNode)
    tm
  }
}