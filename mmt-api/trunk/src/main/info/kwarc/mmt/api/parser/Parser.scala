package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api._
import objects._
import symbols._
import frontend._
import presentation._
import utils.MyList._

import scala.collection.immutable.{HashMap}

/** a ParsingUnit represents a term that is to be parsed
 * @param source the source reference of the string to parse
 * @param scope the theory against which to parse
 * @param context the context against which to parse
 * @param term the term to parse
 * @param top an optional notation that the whole input must match; this can be used to parse a term that is known/required to have a certain form
 */
case class ParsingUnit(source: SourceRef, scope: Term, context: Context, term: String, top: Option[TextNotation] = None)

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
        controller.localLookup.getDeclaredTheory(tm.toMPath).getDeclarations
     }
     decls.flatMap {
        case c: Constant =>
           var names = (c.name :: c.alias.toList).map(_.toString) //the names that can refer to this constant
           if (c.name.last == "_") names ::= c.name.init.toString
           //the unapplied notations consisting just of the name 
           val nots = names map (n => new TextNotation(c.path, Mixfix(List(Delim(n))), presentation.Precedence.infinite))
           c.not.toList ::: nots
        case p: patterns.Pattern =>
           p.not.toList
        case _ => Nil
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
            val name = LocalName.parse(word)
            if (boundVars contains name) {
               //single Tokens may be bound variables
               OMV(name)
            } else if (word.count(_ == '?') > 0) {
                try {
                   Path.parse(word, pu.scope.toMPath) match {
                      //or identifiers
                      case p: ContentPath => OMID(p)
                      case p => makeError("content path expected: " + p, te.region) 
                   }
                } catch {
                   case ParseError(msg) => makeError(msg, te.region)
                }
            } else
               //in all other cases, we don't know
               makeError("unbound token: " + word, te.region) //actually, this is recoverable
               //OMSemiFormal(objects.Text("unknown", word))
         case e: ExternalToken =>
            e.parse(pu, boundVars, this)
         case ml : MatchedList =>
            val notation = ml.an.notation
            val arity = notation.getArity
            //log("constructing term for notation: " + ml.an)
            val found = ml.an.getFound
            // compute the names of all bound variables, in abstract syntax order
            val newBVars: List[(Var,LocalName)] = found.flatMap {
               case fv: FoundVar => fv.getVars map {
                  case SingleFoundVar(_, name, _) =>
                     val ln = LocalName.parse(name.word)
                     (fv.marker, ln)
               }
               case _ => Nil
            }.sortBy(_._1.number)
            val newBVarNames = newBVars.map(_._2)
            // the prefix of newBVarNames just before a certain variable identified by its Var marker and within that FoundVar's sequence of variables by its name 
            def newBVarNamesBefore(vm: Var, name: LocalName) : List[LocalName] = {
               val pos = newBVars.indexWhere(_ == (vm, name))
               newBVarNames.take(pos)
            }
            //stores the argument list in concrete syntax order, together with their position in the abstract syntax
            var args : List[(Int,Term)] = Nil 
            //stores the variable declaration list (name + type) in concrete syntax order together with their position in the abstract syntax
            var vars : List[(Var,SourceRegion,LocalName,Option[Term])] = Nil
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
                     scopes ::= (-n,makeTerm(ml.tokens(i), boundVars ::: newBVarNames))
                  i += 1
               case FoundSeqArg(n, fas) =>
                  // a sequence argument, so take as many TokenListElement as the sequence has elements
                  val toks = ml.tokens.slice(i, i+fas.length)
                  args = args ::: toks.map(t => (n,makeTerm(t, boundVars)))
                  i += fas.length
               case fv: FoundVar =>
                  fv.getVars foreach {case SingleFoundVar(pos, nameToken, tpOpt) =>
                     val name = LocalName(nameToken.word)
                     // a variable declaration, so take one TokenListElement for the type
                     val stp = tpOpt map {_ =>
                        i += 1
                        val ptp = makeTerm(ml.tokens(i-1), boundVars ::: newBVarNamesBefore(fv.marker, name))
                        prag.pragmaticHead(ptp) match {
                           case OMA(OMS(p), List(value)) => prag.strictAttribution(p.module.toMPath, OMS(p), value)
                           case _ => makeError("not a valid type attribution: " + ptp.toString, te.region) 
                        }
                     }
                     vars = vars ::: List((fv.marker, nameToken.region, name, stp))
                  }
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
                  if (arity.isConstant)
                    head
                  else
                    //no args but an empty sequence argument ---> OMA(_,Nil)
                    OMA(head, Nil)
            } else {
                  // order the arguments
                  val orderedArgs = args.sortBy(_._1)
                  // order the variables
                  val orderedVars = vars.sortBy(_._1.number)
                  // compute the arguments, first insert 'null' for each implicit argument
                  var finalArgs : List[Term] = Nil
                  orderedArgs.foreach {case (i,arg) =>
                     // add implicit arguments as needed
                     while (i > finalArgs.length + 1) {
                        finalArgs ::= null
                     }
                     finalArgs ::= arg
                  }
                  // number of implicit arguments that have to be inserted after the explicit arguments
                  val numLastImplArgs = arity.arguments.reverse.takeWhile(_.isInstanceOf[ImplicitArg]).length
                  Range(0, numLastImplArgs).foreach(_ => finalArgs ::= null)
                  // replace each 'null' with a fresh meta-variable
                  finalArgs = finalArgs.map {
                     case null =>
                        val metaVar = newArgument
                        if (boundVars == Nil)
                           metaVar
                        else {
                           //under a binder, apply meta-variable to all bound variables
                           prag.strictApplication(con.module.toMPath, metaVar, boundVars.map(OMV(_)), true)
                        }
                     case a => a
                  }
                  finalArgs = finalArgs.reverse
                  // compute the variables
                  val finalVars = orderedVars.map {
                     case (vm, reg, vname, tp) =>
                        val finalTp = if (! vm.typed) None else tp.orElse {
                           //new meta-variable for unknown type
                           val metaVar = newType(vname) 
                           //under a binder, apply the meta-variable to all governing bound variables
                           //these are the boundVars and all preceding vars of the current binder
                           val governingBVars = boundVars ::: newBVarNamesBefore(vm, vname)
                           val t = if (governingBVars == Nil)
                              metaVar
                           else
                              prag.strictApplication(con.module.toMPath, metaVar, governingBVars.map(OMV(_)), true)
                           Some(t)
                        }
                        val vd = VarDecl(vname, finalTp, None)
                        SourceRef.update(vd, pu.source.copy(region = reg))
                        vd
                  }
                  if (finalVars == Nil && scopes == Nil) {
                     // no vars, scopes --> OMA
                     prag.strictApplication(con.module.toMPath, head, finalArgs)
                  } else if (finalVars != Nil) {
                     val context = Context(finalVars : _*)
                     if (finalArgs == Nil) {
                        //some vars --> OMBINDC
                        prag.strictBinding(con.module.toMPath, head, context, scopes.map(_._2))
                     } else {
                        //some args and some vars --> OMBINDC with OMA
                        val binder = prag.strictApplication(con.module.toMPath, head, finalArgs)
                        prag.strictBinding(con.module.toMPath, binder, context, scopes.map(_._2))
                     }
                  } else
                     //some args, no vars, some scopes
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
  
  
  /**
   * @param pu the parsing unit
   */
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

    //TODO get escape handlers from meta-theory, remove PrefixEscapeHandler, ?SyntaxPlugin
    val escMan = new EscapeManager(controller.extman.lexerExtensions)
    val tl = TokenList(pu.term, pu.source.region.start, escMan)
    if (tl.length == 0) makeError("no tokens found: " + pu.term, pu.source.region)(pu)
    
    //scanning
    val sc = new Scanner(tl, controller.report)
    // scan once with the top notation and make sure it matches the whole input
    pu.top foreach { n =>
       log("scanning top notation: " + n)
       sc.scan(List(n))
       if (sc.tl.length == 1 && sc.tl(0).isInstanceOf[MatchedList])
          ()
       else
          makeError("top notation did not match whole input: " + n.toString, pu.source.region)(pu)
    }
    // now scan with all notations in increasing order of precedence
    qnotations map {
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