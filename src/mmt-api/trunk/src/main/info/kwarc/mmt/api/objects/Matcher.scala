package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import Conversions._

/** matches a goal term against a template term and computes the unifying substitution if possible
 *  i.e., we try to find solution such that template ^ solution == goal

 *  this is matching not unification, i.e., only the template may contain substitution variables,
 *  goal may contain free variables though, which will then occur in the solution
 *  
 *  no equality relation is taken into account except alpha-conversion of bound variables
 *    
 *  @param templateVars the substitution variables in the template term 
 */
class Matcher(templateVars: Context) {
   
   private def log(s: => String) {
      println(s)
   }

   private var solution : Context = templateVars
   def hasUnsolvedVariables : Boolean = solution.toSubstitution.isEmpty
   /**
    * @return the solution if a match was found
    * this does not provide maps for template variables that did not occur in the template
    */
   def getSolution : Substitution = solution.toPartialSubstitution

   /**
    * solves a template variable with the corresponding subterm of goal
    * 
    * @return false if the variable has been solved previously with a different term (no equality theory), otherwise true 
    */
   private def solve(name: LocalName, value: Term) : Boolean = {
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.df.isDefined && solved.df.get != value) return false
      solution = left ::: solved.copy(df = Some(value)) :: right
      true
   }

   /**
    *  the matching function
    *  @param goal the term to be matched
    *  @param template the term to match against, this term may contain the templateVars freely
    *  @param goalVars free Variables of goal, these must be treated in the same way as constants
    *  @return true if the terms match
    */ 
   def apply(goalVars: Context, goal: Term, template: Term) = {
      log(s"matching: $goalVars |- $goal = $template")
      val res = aux(goal, template)(goalVars, Nil)
      if (res) log("matched: " + getSolution)
      else log("no match")
      res
   }
   
   /**
    *  the main recursion function
    *  @param goal the term to be matched
    *    goal may contain variables from goalVars or bound.map(_._1)
    *  @param template the term to match against, this term may contain the templateVars freely
    *  @param goalVars free Variables of the original goal, these must be treated in the same way as constants
    *  @param bound the list of bound variables that are encountered during recursion (in reverse order)
    *   these match only against the corresponding bound variable they are paired with  
    *  @return true if the terms match
    */ 
   private def aux(goal: Term, template: Term)
                  (implicit goalVars: Context, bound: List[(LocalName,LocalName)]): Boolean = (goal, template) match {
      case (_, OMV(x2)) =>
         // first check if x2 is a bound variable (b2); if so, it only matches the respective bound variable (b1)
         bound foreach {case (b1,b2) =>
            if (b2 == x2) return goal == OMV(b1)
         }
         //otherwise, x2 must be in templateVars; if goal does not contain bound variables, solve for x2
         val forbiddenVars = bound.map(_._1)
         if (goal.freeVars.intersect(forbiddenVars) == Nil)
            solve(x2, goal)
         else
            false
      case (OMA(f1, args1), OMA(f2,args2)) =>
         ((f1 :: args1) zip (f2 :: args2)) forall {
            case (x,y) => aux(x,y)
         }
      // recurse into components of binder
      case (OMBINDC(b1, con1, sc1), OMBINDC(b2, con2, sc2)) =>
         // keep track of bound variables
         val newBound = auxCon(con1, con2).getOrElse(return false)
         val bM  = aux(b1, b2)
         val scM = (sc1 zip sc2) forall {
            case (s1, s2) => aux(s1, s2)(goalVars, bound ::: newBound)
         }
         bM && scM
      // this case works for constants and literals (true if equal), and all asymmetric combinations (always false) 
      case (t1,t2) => t1 == t2 // TODO: other cases
   }
   
   /**
    * like aux but for contexts
    * @return None if no match, Some(List((x1,y1),...)) if the terms match and x1 corresponds to y1
    */
   private def auxCon(goal: Context, template: Context)
               (implicit goalVars: Context, bound: List[(LocalName,LocalName)]): Option[List[(LocalName,LocalName)]] = {
      if (goal.length != template.length) return None
      var newBound : List[(LocalName,LocalName)] = Nil
      (goal zip template) foreach {
         case (VarDecl(x1,tp1,df1,_*), VarDecl(x2,tp2, df2,_*)) =>
            val compM = List((tp1,tp2), (df1,df2)) forall {
               case (None,None) => true
               case (Some(t1), Some(t2)) =>
                  aux(t1,t2)(goalVars, newBound)
               case _ => false
            }
            if (x1 == x2 && compM)
               newBound ::= (x1,x2)
            else
               return None
      }
      Some(newBound)
   }
}