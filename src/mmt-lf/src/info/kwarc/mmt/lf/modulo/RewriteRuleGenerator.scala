package info.kwarc.mmt.lf.modulo
import info.kwarc.mmt.api._
import frontend._
import objects._

class RewriteRuleGenerator extends RoleHandler {
   def isApplicable(role: String): Boolean = role == "RewriteRule"
   def apply(c: symbols.Constant) {
      c.tp match {
         case Some(tp) => tp match {
            case LFModulo.Rewrite(context, left, right) =>
               val leftH = left.head match {
                  case Some(p: GlobalName) => p
                  case _ => null
               }
               val simpRule = new ComputationRule(leftH) {
                  def apply(solver: Solver)(tm: Term)(implicit stack: Stack) = {
                     val matcher = new Matcher(context)
                     val matched = matcher(stack.context, tm, left)
                     if (matched) {
                        val rewritten = right ^ matcher.getSolution
                        Some(rewritten)
                     } else
                        // rule is not applicable
                        None
                  }
               }
               log("found rewrite rule for " + leftH.toString)
               controller.extman.ruleStore.add(simpRule)
            case _ => 
               logError("type given but not a rewrite rule: " + tp.toString)
         }
         case _ =>
            logError("no type given")
      }
   }
}