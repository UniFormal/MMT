package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._
import frontend._

/** exhaustively applies [[TermTransformationRule]]s
 *  
 *  will not traverse the same term multiple times if called multiple times
 *  
 *  preserves structure sharing
 */
class TermTransformer(id: String, controller: Controller, rules: RuleSet, use: TermTransformationRule => Boolean)
   extends StatelessTraverser with Logger {
  
   val logPrefix = id
   val report = controller.report
   private def presentObj(t: Term) = controller.presenter.asString(t)
  
   private val matcher = new Matcher(controller, rules)
   private val transformRules = rules.get(classOf[TermTransformationRule]).filter(use)
   
   /**
    * used to remember that a Term is the result of transformation to avoid recursing into it again 
    */
   private object CachedResult extends BooleanTermProperty(utils.mmt.baseURI / "clientProperties" / "uom" / id)

   /** exhaustively apply all rules to this term, return result if changed */
   private def tryRules(t: Term, changed: Boolean = false)(implicit context: Context): Option[Term] = {
     transformRules.foreach {r =>
        r(matcher, context, t).foreach {tC =>
          log(presentObj(t) + " ---> " + presentObj(tC))
          return tryRules(tC, true)
        }
     }
     if (changed) Some(t) else None
   }
  
   /** thrown to bubble up a change to the previous traversal level */
   private case class TransformedTo(result: Term) extends Throwable 
   
   def traverse(t: Term)(implicit con : Context, state: Unit) = {
     //log("traversing " + presentObj(t))
     try {
       traverseAux(t)
      } catch {case TransformedTo(tC) =>
        // any change bubbles up all the way until here
        tC
      }
   }
  
   private def traverseAux(t: Term)(implicit con : Context, state: Unit): Term = {
     t match {
       case CachedResult(tC) =>
         // already done
         tC
       case t => tryRules(t) match {
         case Some(tC) =>
           // bubble up change
           throw TransformedTo(tC)
         case None => try {
           // apply to subterms
           val tC = Traverser(this, t)
           // no change, so remember that we are done
           CachedResult(tC)
         } catch {case TransformedTo(tC) =>
           // there was a change in a subterm, repeat
             traverseAux(tC)
           }
         }
       }
    }
}
