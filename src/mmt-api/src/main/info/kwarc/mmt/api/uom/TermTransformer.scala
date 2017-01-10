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
   private val transformRules = rules.getOrdered(classOf[TermTransformationRule]).filter(use)
   
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
     t match {
       case CachedResult(tC) =>
         // already done
         tC
       case t =>
         // exhaustively process all subterms
         val tT = Traverser(this, t)
         // apply rules at this level
         tryRules(tT) match {
            case Some(tTC) =>
              // there was a change at this level, repeat
              traverse(tTC)
            case None =>
              // no change, so remember that we are done
              CachedResult(tT)
         }
     }
   }
}
