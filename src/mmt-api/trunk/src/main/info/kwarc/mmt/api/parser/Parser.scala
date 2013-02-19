package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import objects._
import symbols._
import frontend._
import presentation._
import utils.MyList._

import scala.collection.immutable.{HashMap}

/** a ParsingUnit represents a term that is to be parsed
 * @param component the URI of the Term, if any; may be null
 * @param scope the theory against which to parse
 * @param context the context against which to parse
 * @param term the term to be parse
 * @param first the position of the first character, used for back references, 0 by default
 */
case class ParsingUnit(source: SourceRef, scope: Term, context: Context, term: String)

/** a TermParser parses Term's. Instances are maintained by the ExtensionManager and retrieved and called by the structural parser. */
trait AbstractObjectParser {
   def apply(pu: ParsingUnit): Term
}

/** helper object */
object AbstractObjectParser {
  def getNotations(controller : Controller, scope : Term) : List[TextNotation] = {
     val closer = new libraries.Closer(controller)
     closer(scope.toMPath)
     val includes = controller.library.visible(scope)
     val decls = includes.toList flatMap {tm =>
        controller.localLookup.getDeclaredTheory(tm.toMPath).getConstants
     }
     decls.flatMap {c =>
        c.not.toList ::: List(new TextNotation(c.path, List(Delim(c.path.last)), presentation.Precedence.neginfinite))
     }
   }
  val unknown = utils.mmt.mmtcd ? "unknown"
  def splitOffUnknowns(t: Term) = t match {
     case OMBIND(OMID(AbstractObjectParser.unknown), us, s) => (us, s)
     case _ => (Context(), t)
  }
}

//TODO: notations should not be computed separately for each ParsingUnit; they must be cached theory-wise

/** A default parser that parses any string into an OMSemiFormal object. */
object DefaultObjectParser extends AbstractObjectParser {
   def apply(pu: ParsingUnit): Term = OMSemiFormal(objects.Text(pu.scope.toMPath.toPath, pu.term))
}

/**
 * A notation based parser
 */
class ObjectParser(controller : Controller) extends AbstractObjectParser with Logger {
   val report = controller.report
   val logPrefix = "parser"  
   private val prag = controller.pragmatic   
   
   /**
    * builds parsers notation context based on content in the controller 
    * and the scope (base path) of the parsing unit
    * returned list is sorted (in increasing order) by priority
    */
   protected def buildNotations(scope : Term) : List[(Precedence,List[TextNotation])] = {
      val notations = TextNotation.bracketNotation :: AbstractObjectParser.getNotations(controller, scope)  
      val qnotations = notations.groupBy(x => x.precedence).toList
//      log("notations in scope: " + qnotations)
      qnotations.sortWith((p1,p2) => p1._1 < p2._1)
   } 

   /** constructs a SourceError and throws it, all errors go through this method */
   private def makeError(msg: String, reg: SourceRegion)(implicit pu: ParsingUnit) = {
      val ref = SourceRef(pu.source.container, reg)
      val err = SourceError(logPrefix, ref, msg)
      throw err
   }
   
   /**
    * declarations of the implicit variables
    */
   // due to the recursive processing, variables in children are always found first
   // so adding to the front yields the right order
   private var vardecls: List[VarDecl] = Nil
   private var counter = 0
   private def newArgument: OMV = {
     val name = LocalName("") / "argument" / counter.toString
     //val tname = LocalName("") / "argumentType" / counter.toString
     vardecls = VarDecl(name,None,None) :: vardecls
     counter += 1
     OMV(name)
   }
   private def newType(name: LocalName): OMV = {
      val tname = name / "type"
      vardecls ::= VarDecl(tname,None,None)
      OMV(tname)
   }
   
   /**
    * recursively transforms a TokenListElem (usually obtained from a [[info.kwarc.mmt.api.parser.Scanner]]) to an MMT Term
    * @param te the element to transform
    * @param boundVars the variable names bound in the context
    * @param pu the original ParsingUnit (constant during recursion)
    */
   private def makeTerm(te : TokenListElem, boundVars: List[LocalName])(implicit pu: ParsingUnit) : Term = {
      val term = te match {
         case Token(word, _, _) =>
            val name = LocalName.parse(word.replace(".", "/"))
            if (boundVars contains name) {
               //single Tokens may be bound variables
               OMV(name)
            } else if (word.count(_ == '?') > 0) {
                val p = Path.parseS(word, pu.scope.toMPath)
                //or identifiers
                val t = OMID(p)
                t
            } else
               //in all other cases, we don't know
               makeError("unbound token: " + word, te.region) //actually, this is recoverable
               //OMSemiFormal(objects.Text("unknown", word))
         case Escaped(begin,text,_,pos) =>
            //TODO clean up
            val r = Reader(text)
            val (format,_) = r.readToken
            val (rest,_) = r.readAll
            r.close
            OMSemiFormal(objects.Text(format, rest))
         case ml : MatchedList =>
            //log("constructing term for notation: " + ml.an)
            val found = ml.an.getFound
            // compute the names of all bound variables, in abstract syntax order
            val newBVars: List[LocalName] = found.mapPartial {
               case FoundVar(n,_,name,_) => Some((n,LocalName(name.word)))
               case _ => None
            }.sortBy(_._1).map(_._2)
            //stores the argument list in concrete syntax order, together with their position in the abstract syntax
            var args : List[(Int,Term)] = Nil 
            //stores the variable declaration list (name + type) in concrete syntax order together with their position in the abstract syntax
            var vars : List[(Int,Token,Option[Term])] = Nil
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
                  vars ::= (n, name, t) 
            }
            val con = ml.an.notation.name
            // hard-coded special case for a bracketed subterm
            if (con == utils.mmt.brackets)
               args(0)._2
            else {
            // the head of the produced Term
            val head = OMID(con)
            //construct a Term according to args, vars, and scopes
            //this includes sorting args and vars according to the abstract syntax
            if (args == Nil && vars == Nil && scopes == Nil) {
                  //no args, vars, scopes --> OMID
                  head
            } else {
                  // order the arguments
                  val orderedArgs = args.sortBy(_._1)
                  var finalArgs : List[Term] = Nil
                  orderedArgs.foreach {case (i,arg) =>
                     // add implicit arguments as needed
                     while (i > finalArgs.length + 1) {
                        //generate fresh meta-variable
                        val metaVar = newArgument
                        val implArg = if (boundVars == Nil)
                           metaVar
                        else {
                           //under a binder, apply meta-variable to all bound variables
                           prag.strictApplication(con.module.toMPath, metaVar, boundVars.map(OMV(_)), true)
                        }
                        finalArgs ::= implArg
                     }
                     finalArgs ::= arg
                  }
                  // order the variables
                  val orderedVars = vars.sortBy(_._1).map {
                     case (_, vr, tp) =>
                        val vname = LocalName(vr.word)
                        val finalTp = tp.getOrElse {
                           //new meta-variable for unknown type
                           val metaVar = newType(vname) 
                           if (boundVars == Nil)
                              metaVar
                           else
                           //under a binder, apply meta-variable to all bound variables
                           prag.strictApplication(con.module.toMPath, metaVar, boundVars.map(OMV(_)), true)
                        }
                        val vd = VarDecl(vname, Some(finalTp), None)
                        SourceRef.update(vd, pu.source.copy(region = vr.region))
                        vd
                  }
                  if (orderedVars == Nil && scopes == Nil) {
                     // no vars, scopes --> OMA
                     prag.strictApplication(con.module.toMPath, head, finalArgs.reverse)
                  } else if (orderedVars != Nil && scopes.length == 1) {
                     val context = Context(orderedVars : _*)
                     if (args == Nil) {
                        //some vars, and 1 scope --> OMBIND
                        prag.strictBinding(con.module.toMPath, head, context, scopes(0)._2)
                     } else {
                        //some args, some vars, and 1 scope --> OMBIND with OMA
                        val binder = prag.strictApplication(con.module.toMPath, head, finalArgs.reverse)
                        prag.strictBinding(con.module.toMPath, binder, context, scopes(0)._2)
                     }
                  } else
                     //not all combinations correspond to Terms
                     //this should only happen for ill-formed notations
                     makeError("ill-formed notation", te.region)
               }}
         case ul : UnmatchedList =>
            if (ul.tl.length == 1)
               // process the single TokenListElement
               makeTerm(ul.tl(0), boundVars)
            else {
               /* TODO This case arises if
               - the Term is ill-formed
               - the matching TextNotation is missing
               - a subterm has no delimiters as in LF application
               - a semi-formal subterm consists of multiple text Tokens 
               Consequently, it is not obvious how to proceed.
               Current behavior: application of first to rest
               //throw ParseError("unmatched list: " + ul.tl)
               */
               val terms = ul.tl.getTokens.map(makeTerm(_,boundVars))
               prag.strictApplication(pu.scope.toMPath, terms.head, terms.tail)
            }
      }
      //log("result: " + term)
      SourceRef.update(term, pu.source.copy(region = te.region))
      term
   }
  
  def apply(pu : ParsingUnit) : Term = {    
    //gathering notations in scope
    val qnotations = buildNotations(pu.scope)
    // reset generated variables
    vardecls = Nil
    log("parsing: " + pu.term)
    log("notations:")
    logGroup {
       qnotations.map(o => log(o.toString))
    }

    //TODO get escape handlers from meta-theory
    val escMan = new EscapeManager(List(GenericEscapeHandler))
    val tl = TokenList(pu.term, pu.source.region.start, escMan)
    if (tl.length == 0) makeError("no tokens found: " + pu.term, pu.source.region)(pu)
    
    //scanning
    val sc = new Scanner(tl, controller.report)
    qnotations reverseMap {
         case (priority,nots) => sc.scan(nots)
    }
    log("scan result: " + sc.tl.toString)
    
    //structuring
    val varnames = pu.context.variables.map(_.name).toList
    val scanned = sc.tl.length match {
       case 1 => sc.tl(0)
       case _ => new UnmatchedList(sc.tl)
    }
    val tm = logGroup {
       makeTerm(scanned, varnames)(pu)
    }
    log("parse result: "  + tm.toString)
    if (vardecls == Nil)
       tm
    else
       OMBIND(OMID(AbstractObjectParser.unknown), Context(vardecls: _*),tm)
  }
}