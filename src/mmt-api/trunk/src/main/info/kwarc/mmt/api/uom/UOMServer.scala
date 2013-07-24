package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._

class UOMState(val t : Term, val path : List[Int]) {
  def enter(i : Int) : UOMState = new UOMState(t, i :: path)
  def exit(i : Int) : UOMState = new UOMState(t, path.tail)
  override def toString = t.toString + "@" + path.mkString("_")  
}

/** A UOM applies DepthRule's and BreadthRule's exhaustively to simplify a Term */
class UOM(controller: frontend.Controller) extends Traverser[UOMState] with frontend.Logger {
  val report = controller.report
  val logPrefix = "uom"

  /* code for saving a log of the applied operations, can be used later for interactive systems */
  var simplificationLog : List[(UOMState,Term,Rule)] = Nil
  var saveLog = true //TODO for now
  
  def saveSimplificationResult(start : UOMState, end : Term) = {
    log("Saving result" +  start + " -> " + end)
    val (_,_,rule) = simplificationLog.head
    simplificationLog = (start, end, rule) :: simplificationLog.tail
    //log(simplificationLog.toString)
  }
  
  def saveSimplificationRule(rule : Rule) = {
    log("Saving rule " +  rule)
    simplificationLog ::= (null, null, rule)
    //log(simplificationLog.toString)
  }
  /* end of simplification log functions */
  
  private val rs = controller.extman.ruleStore
  private val StrictOMA = controller.pragmatic.StrictOMA

   /** applies all DepthRule's that are applicable at toplevel of an OMA
    * for each arguments, all rules are tried
    * if a rule leads to a GlobalChange, we stop; otherwise, we go to the next argument
    * @param outer the toplevel symbol
    * @param before the arguments that have been checked already
    * @param after the arguments that still need to be checked
    * @return GlobalChange if a rule led to it; LocalChange otherwise (even if no rule was applicable)
    */
   private def applyDepthRules(outer: GlobalName, before: List[Term], after: List[Term]): Change = {
      after match {
         case Nil => LocalChange(before) //we don't know if 'before' has a change; but if not, returning NoChange would not actually help in applyAux anyway
         case arg::afterRest =>
            //auxiliary pattern-matcher to unify the cases for OMA and OMS
            //in order to permit distinguishing OMA(OMS(p),Nil) and OMS(p), a boolean is returned
            //that indicates which case applied
            object OMAorOMS {
               def unapply(t: Term) = t match {
                  case StrictOMA(strApps, p, args) => Some((p, args, false))
                  case OMS(p) => Some((p, Nil, true))
                  case _ => None
               } 
            }
            arg match {
               case OMAorOMS(inner, inside, isOMS) =>
                  rs.depthRules.getOrElse((outer,inner), Nil) foreach {rule =>
                     val ch = rule.apply(before, inside, afterRest)
                     log("rule " + rule + ": " + ch)
                     ch match {
                        case NoChange =>
                        case LocalChange(args) =>
                           saveSimplificationRule(rule)
                           return applyDepthRules(outer, before, args ::: afterRest)
                        //return immediately upon GlobalChange
                        case GlobalChange(tS) =>
                           saveSimplificationRule(rule)
                           return GlobalChange(tS)
                     }
                  }
                  applyDepthRules(outer, before ::: List(arg), afterRest)
               case _ =>
                 applyDepthRules(outer, before ::: List(arg), afterRest)
            }
      }
   }
   /** applies all BreadthRule's that are applicable at toplevel of an OMA
    * @param outer the toplevel symbol
    * @param args the arguments
    * */
   private def applyBreadthRules(op: GlobalName, inside: List[Term]): Change = {
      var insideS = inside
      var changed = false
      rs.breadthRules.getOrElse(op,Nil) foreach {rule =>
         val ch = rule.apply(insideS)
         log("rule " + rule + ": " + ch)
         ch match {
            case NoChange =>
            case LocalChange(args) =>
               insideS = args
               changed = true
               saveSimplificationRule(rule)

            case GlobalChange(t) => 
              saveSimplificationRule(rule)
              return GlobalChange(t)
         }
      }
      //we have to check for insideS == inside here in case a BreadthRule falsely thinks it changed the term (such as commutativity when the arguments are already in normal order)
      if (! changed || insideS == inside) NoChange else LocalChange(insideS)
   }
   
   /** the main simplification method
    * @param t the term to simplify
    * @param context its context, if non-empty
    * @return the simplified Term (if a sensible collection of rules is used that make this method terminate)
    * 
    * The input term must be fully strictified, and so will be the output term.
    * Applicability of rules is determined based on the pragmatic form (using controller.pragmatic.StrictOMA).
    * Rules are passed strict terms and are expected to return strict terms.
    */
   def simplify(t: Term, context: Context = Context()) = {
     simplificationLog = Nil
     val initState = new UOMState(t, Nil)
     val tS = apply(t,initState, context)
     tS
   }
   
   /** the simplification method that is called internally during the traversal of a term
    * users should not call this method (call simplify instead) */
   def traverse(t: Term)(implicit con : Context, init: UOMState) : Term =  t match {
      case OMA(OMS(_), _) =>
         log("simplifying " + controller.presenter.asString(t))
         log("state is" + init.t + "\n at " + init.path.toString)
         val (tS, globalChange) = logGroup {
            applyAux(t)
         }
         log("simplified  " + controller.presenter.asString(tS))
         if (globalChange)
           saveSimplificationResult(init, tS)
         val tSM = tS.from(t)
         if (globalChange)
            Changed(tSM)
         else
            tSM
      case OMS(p) =>
         rs.abbrevRules(p).headOption match {
           case Some(ar) => ar.term.from(t)
           case None => t
         }
      case _ => Traverser(this, t)
   }
   /** an auxiliary method of apply that applies simplification rules
    * This method exhaustively applies rules as follows:
    *  (1) --depth rules--> (2) --simplify arguments--> (3) --breadth rules--> (return)
    * If any of the operations causes changes, the automaton goes back to state (1),
    * but some optimizations are used to avoid traversing a previously-simplified term again.
    * @param t the term to simplify (rules apply only to terms OMA(OMS(_),_)) 
    * @param globalChange true if there has been a GlobalChange so far
    * @return the simplified term and a Boolean indicating whether a GlobalChange occurred
    */
   private def applyAux(t: Term, globalChange: Boolean = false)(implicit con : Context, init: UOMState) : (Term, Boolean) = t match {
      case StrictOMA(strictApps, outer, args) =>
         // state (1)
         log("applying depth rules to   " + t)
         applyDepthRules(outer, Nil, args) match {
            case GlobalChange(tS) =>
               // go back to state (1), remember that a global change was produced
               applyAux(tS, true)
            case NoChange =>
               // applyDepthRules returns a LocalChange if no depth rule was applicable
               throw ImplementationError("impossible case")
            case LocalChange(argsS) =>
               // state (2) 
               // by marking with and testing for Simplified(_), we avoid recursing into a term twice
               val argsSS = argsS.zipWithIndex map {
                  case (Simplified(a),i) => a 
                  case (a,i) => Simplified(traverse(a)(con, init.enter(i + 1))) // +1 adjusts for the f in OMA(f, args)
               }
               val tS = StrictOMA(strictApps, outer, argsSS)
               // if any argument changed globally, go back to state (1)
               if (argsSS exists {
                   case Changed(t) => Changed.eraseMarker(t); true
                   case _ => false
                })
                   applyAux(tS, globalChange)
               else {
                  //state (3)
                  log("applying breadth rules to " + outer(argsSS))
                  applyBreadthRules(outer, argsSS) match {
                     case GlobalChange(tSS) =>
                        // go back to state (1), remember that a global change was produced
                        applyAux(tSS, true)
                     case LocalChange(argsSSS) =>
                        // go back to state (1)
                        applyAux(StrictOMA(strictApps, outer, argsSSS), globalChange)
                     case NoChange =>
                        // state (4)
                        (tS, globalChange)
                  }
               }
         }
      // no rules applicable
      case _ => (t, globalChange)
   }
}

/**
 * apply/unapply methods that encapsulate functionality for attaching a Boolean clientProperty to a Term
 */
class BooleanTermProperty(val property: utils.URI) {
   def apply(t: Term) : Term = {
     t.clientProperty(property) = true
     t
   }
   def unapply(t: Term): Option[Term] =
      t.clientProperty.get(this.property) match {
          case Some(true) => Some(t)
          case _ => None
      }
   def eraseMarker(t: Term) {
      t.clientProperty -= property
   }
}

/**
 * UOM uses this to remember that a Term has been simplified already to avoid recursing into it again 
 */
object Simplified extends BooleanTermProperty(utils.mmt.baseURI / "clientProperties" / "uom" / "simplified")

/**
 * UOM uses this to remember whether a Term underwent a GlobalChange during simplification
 * this information is passed upwards during recursive simplification
 */
object Changed extends BooleanTermProperty(utils.mmt.baseURI / "clientProperties" / "uom" / "changed")