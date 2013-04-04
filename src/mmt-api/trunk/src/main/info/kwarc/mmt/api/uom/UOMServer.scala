package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._

import java.net._
import java.io._
import java.util.jar._


/** A UOM applies DepthRule's and BreadthRule's exhaustively to simplify a Term */
class UOM(controller: frontend.Controller) extends StatelessTraverser with frontend.Logger {

  val report = controller.report
  val logPrefix = "uom"

  private val rs = controller.extman.ruleStore

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
         case Nil => LocalChange(before)
         case arg::afterRest =>
            arg match {
               case OMAMaybeNil(OMS(inner), inside) =>
                  rs.depthRules.getOrElse((outer,inner), Nil) foreach {rule =>
                     val ch = rule.apply(before, inside, afterRest)
                     log("rule " + rule + ": " + ch)
                     ch match {
                        case NoChange =>
                        case LocalChange(args) =>
                           return applyDepthRules(outer, before, args ::: afterRest)
                        //return immediately upon GlobalChange
                        case GlobalChange(tS) =>
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
            case GlobalChange(t) => return GlobalChange(t)
         }
      }
      //we have to check for insideS == inside here in case a BreadthRules falsely thinks it changed the term (such as commutativity when the arguments are already in normal order)
      if (! changed || insideS == inside) NoChange else LocalChange(insideS)
   }
   
   /** the main simplification method
    * @param t the term to simplify
    * @param context its context, if non-empty
    * @return the simplified Term (if a sensible collection of rules is used that make this method terminate)
    */
   def simplify(t: Term, context: Context = Context()) = apply(t,context)
   
   /** the simplification method that is called internally during the traversal of a term
    * users should not call this method (call simplify instead) */
   def traverse(t: Term)(implicit con : Context, init: Unit) : Term =  t match {
      case OMA(OMS(_), _) =>
         log("simplifying " + t)
         report.indent
         val (tS, globalChange) = applyAux(t)
         report.unindent
         log("simplified  " + tS)
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
    * but some optimizations are used to avoid simplifying a previously-simplified term again.
    * @param t the term to simplify (rules apply only to terms OMA(OMS(_),_)) 
    * @param globalChange true if there has been a GlobalChange so far
    * @return the simplified term and a Boolean indicating whether a GlobalChange occurred
    */
   private def applyAux(t: Term, globalChange: Boolean = false)(implicit con : Context, init: Unit) : (Term, Boolean) = t match {
      case OMA(OMS(outer), args) =>
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
               val argsSS = argsS map {
                  case Simplified(a) => a 
                  case a => Simplified(traverse(a))
               }
               // if any argument changed globally, go back to state (1)
               if (argsSS exists {
                   case Changed(t) => Changed.eraseMarker(t); true
                   case _ => false
                })
                    applyAux(outer(argsSS), globalChange)
               else {
                  //state (3)
                  log("applying breadth rules to " + outer(argsSS))
                  applyBreadthRules(outer, argsSS) match {
                     case GlobalChange(tSS) =>
                        // go back to state (1), remember that a global change was produced
                        applyAux(tSS, true)
                     case LocalChange(argsSSS) =>
                        // go back to state (1)
                        applyAux(outer(argsSSS), globalChange)
                     case NoChange =>
                        // state (4)
                        (outer(argsSS), globalChange)
                  }
               }
         }
      // no rules applicable
      case _ => (t, globalChange)
   }
}

/** apply/unapply methods that encapsulate functionality for attaching a Boolean clientProperty to a Term
 * UOM uses it to remember that a Term has been simplified already to avoid recursing into it again 
 */
object Simplified {
   private val simplifyProperty = utils.mmt.baseURI / "clientProperties" / "uom" / "simplified"
   def apply(t: Term) : Term = {
     t.clientProperty(simplifyProperty) = true
     t
   }
   def unapply(t: Term): Option[Term] =
      t.clientProperty.get(simplifyProperty) match {
          case Some(true) => Some(t)
          case _ => None
      }
}

/** apply/unapply methods that encapsulate functionality for attaching a Boolean clientProperty to a Term
 * UOM uses it to remember whether a Term underwent a GlobalChange during simplification
 * this information is passed upwards during recursive simplification
 */
object Changed {
   private val changeProperty = utils.mmt.baseURI / "clientProperties" / "uom" / "changed"
   def apply(t: Term) : Term = {
     t.clientProperty(changeProperty) = true
     t
   }
   def unapply(t: Term): Option[Term] =
      t.clientProperty.get(changeProperty) match {
        case Some(true) => Some(t)
        case _ => None
      }
   def eraseMarker(t: Term) {
      t.clientProperty -= changeProperty
   }
}