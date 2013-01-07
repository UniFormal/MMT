package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.frontend._
import utils.MyList._

import scala.collection.immutable.{HashMap}

/** a ParsingUnit represents a term that is to be parsed
 * @param component the URI of the Term, if any; may be null
 * @param scope the theory against which to parse
 * @param context the context against which to parse
 * @param term the term to be parse
 * */
case class ParsingUnit(component: CPath, scope: Term, context: Context, term: String)

/**
 * simplified construction of typical parsing errors
 * @return the corresponding SourceError
 */
object ParsingError {
  def apply(msg : String, sr : SourceRegion) = SourceError("object-parser", SourceRef(null, sr), msg) //TODO container missing
   //("Parsing Error: " + msg + " at (" + start + ","+ end + ")") {
  //def stackTrace : String = getStackTrace.map(x => x.toString).mkString("","\n","")
}


/** a TermParser parses Term's. Instances are maintained by the ExtensionManager and retrieved and called by the structural parser. */
abstract class TermParser {
   def apply(pu: ParsingUnit): Term
}

/** helper object */
object TermParser {
  def getNotations(controller : Controller, scope : Term) : List[TextNotation] = {
     val includes = controller.library.visible(scope)
     val decls = includes.toList flatMap {tm => 
       controller.globalLookup.get(tm.toMPath).components //TODO: should not use .components
     }
     decls collect {
       case c : Constant => c.not match {
         case Some(not) => not 
         case None => new TextNotation(c.path, List(Delim(c.name.toPath)), 0)
       }
     }
   }
}

/** A default parser that parses any string into an OMSemiFormal object. */
object DefaultParser extends TermParser {
   def apply(pu: ParsingUnit): Term = OMSemiFormal(Text(pu.scope.toMPath.toPath, pu.term))
}


/**
 * Default implementation for a notation based parser
 */
class NotationParser(controller : Controller) extends TermParser with Logger {
   val report = controller.report
   val logPrefix = "parser"  
  
   /**
    * builds parsers notation context based on content in the controller 
    * and the scope (base path) of the parsing unit
    * returned list is sorted (in increasing order) by priority
    */
   def buildNotations(scope : Term) : List[(Int,List[TextNotation])] = {
      val notations = TermParser.getNotations(controller, scope)  
      val qnotations = notations.groupBy(x => x.priority).toList
      qnotations.sortWith((p1,p2) => p1._1 < p2._1)
   } 
  
   /**
    * transforms a TokenList (usually obtained from a [[info.kwarc.mmt.api.parser.Scanner]]) to an MMT Term 
    */
   def makeTerm(te : TokenListElem, boundVars: List[String]) : Term = {
      te match {
         case Token(word, _, _) =>
            if (boundVars contains word)
               //single Tokens may be bound variables
               OMV(LocalName(word))
            else
               //in all other cases, we don't know
               OMSemiFormal(Text("unknown", word))
         case ml : MatchedList =>
            log("constructing term for notation: " + ml.an)
            val found = ml.an.getFound
            // compute the names of all bound variables, in abstract syntax order
            val newBVars: List[String] = found.mapPartial {
               case FoundVar(n,_,name,_) => Some((n,name))
               case _ => None
            }.sortBy(_._1).map(_._2.word)
            //stores the argument list in concrete syntax order, together with their position in the abstract syntax
            var args : List[(Int,Term)] = Nil 
            //stores the variable declaration list (name + type) in concrete syntax order together with their position in the abstract syntax
            var vars : List[(Int,String,Option[Term])] = Nil
            //stores the scope list in concrete syntax order together with their positions in the abstract syntax
            var scopes : List[(Int, Term)] = Nil
            // We walk through found, computing arguments/variable declarations/scopes
            // by recursively processing the respective TokenListElem in ml.tokens
            var i = 0 //the position of the next TokenListElem in ml.tokens
            found foreach {
               case _:FoundDelim =>
               case FoundArg(_,n) =>
                  if (n>0)
                     //a normal argument
                     args ::= (n,makeTerm(ml.tokens(i), boundVars))
                  else
                     //a scope, all newBVars are added to the context
                     scopes ::= (-n,makeTerm(ml.tokens(i), boundVars ::: newBVars))
                  i += 1
               case FoundSeqArg(n, fas) =>
                  // a sequence argument, so take as many TokenListElement as the sequence has elements
                  val toks = ml.tokens.slice(i, i+fas.length)
                  args = args ::: toks.map(t => (n,makeTerm(t, boundVars)))
                  i += fas.length
               case FoundVar(n, pos, name, tpOpt) =>
                  // a variable declaration, so take one optional TokenListElement for the type
                  val t = tpOpt map {_ =>
                     i += 1
                     //the preceding newBVars are added to the context
                     makeTerm(ml.tokens(i-1), boundVars ::: newBVars.take(n-1))
                  }
                  vars ::= (n, name.word, t) 
            }
            // the head of the produced Term
            val head = OMID(ml.an.notation.name)
            //construct a Term according to args, vars, and scopes
            //this includes sorting args and vars according to the abstract syntax
            val result = if (vars == Nil && scopes == Nil) {
               if (args == Nil)
                  //no args, vars, scopes --> OMID
                  head
               else {
                  // no vars, scopes --> OMA
                  val orderedArgs = args.sortBy(_._1).map(_._2)
                  OMA(head, orderedArgs)
               }
            } else if (args == Nil && vars != Nil && scopes.length == 1) {
                  // no args, some vars, and 1 scope --> OMBIND
                  val orderedVars = vars.sortBy(_._1).map {
                     case (_, name, tp) => VarDecl(LocalName(name), tp, None)
                  }
                  val context = Context(orderedVars : _*)
                  OMBIND(head, context, scopes(0)._2)
            } else
                  //not all combinations correspond to Terms
                  //this should only happen for ill-formed notations
                  throw ParseError("ill-formed notation")
            log("result: " + result)
            result
         case ul : UnmatchedList =>
            if (ul.tl.length == 1)
               // process the single TokenListElement
               makeTerm(ul.tl(0), boundVars)
            else
             /* TODO This case arises if
               - the Term is ill-formed
               - the matching TextNotation is missing
               - a subterm has no delimiters as in LF application
               - a semi-formal subterm consists of multiple text Tokens 
              Consequently, it is not obvious how to proceed.
             */
             throw ParseError("unmatched list: " + ul.tl)
      }
   }
  
  def apply(pu : ParsingUnit) : Term = {    
    //gathering notations in scope
    val qnotations = buildNotations(pu.scope)    
    
    log("Started parsing " + pu.term + " with operators : ")
    logGroup {
       qnotations.map(o => log(o.toString))
    }
    
    //scanning
    val sc = new Scanner(TokenList(pu.term), controller.report)
    qnotations reverseMap {
         case (priority,nots) => sc.scan(nots)
    }
    log("#####   scan result: " + sc.tl.toString)
    
    //structuring
    sc.tl.length match {
       case 1 =>
          val tm = logGroup {
             makeTerm(sc.tl(0), Nil)
          }
          log("##### parse result: "  + tm.toNode)
          tm
       case _ => throw ParseError("unmatched list: " + sc.tl)
    }
  }
}