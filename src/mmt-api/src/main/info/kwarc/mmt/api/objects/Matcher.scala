package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import frontend._
import checking._
import Conversions._

/** matches a goal term against a template term and computes the unifying substitution if possible
 *  i.e., we try to find solution such that template ^ solution == goal

 *  this is matching not unification, i.e., only the template may contain substitution variables,
 *  goal may contain free variables though, which will then occur in the solution
 *  
 *  no equality relation is taken into account except alpha-conversion of bound variables and solution rules
 *  
 *  @param controller needed for lookups when type checking the matches
 *  @param queryVars the substitution variables in the template term 
 *  @param context free Variables of goal and template, these must be treated in the same way as constants
 */
class Matcher(controller: Controller, rules: RuleSet, context: Context, queryVars: Context) extends Logger {
   def logPrefix = "matcher"
   def report = controller.report
   
   private val solutionRules = rules.get(classOf[SolutionRule])
   private val equalityRules = rules.get(classOf[TermBasedEqualityRule])
   
   private var querySolution : Context = queryVars
   def getUnsolvedVariables : List[LocalName] = querySolution collect {
      case VarDecl(x,_,None,_) => x
   }
   /**
    * @return the solution if a match was found
    * this does not provide maps for template variables that did not occur in the template
    */
   def getSolution : Substitution = querySolution.toPartialSubstitution

   /**
    * solves a template variable with the corresponding subterm of goal
    * 
    * @return false if the variable has been solved previously with a different term (no equality theory), otherwise true 
    */
   private def solve(name: LocalName, value: Term) : Boolean = {
      val (left, solved :: right) = querySolution.span(_.name != name)
      val valueS = controller.simplifier(value, context ++ left, rules)
      solved.tp foreach {tp =>
         val valueTp = Solver.infer(controller, context, value, Some(rules)).getOrElse(return false)
         val tpmatch = aux(Nil, valueTp, tp)
         if (!tpmatch) return false
      }
      solved.df foreach {df =>
         if (df hashneq valueS) return false
      }
      querySolution = left ::: solved.copy(df = Some(value)) :: right
      true
   }
   
   /** 
    *  callbacks for checking rules
    *  
    *  equality judgements are channeled back into matching; other judgements are false
    */
   private val callback = new CheckingCallback {
      def check(j: Judgement)(implicit history: History) = j match {
         case j: Equality => aux(j.context, j.tm1, j.tm2)
         case j: EqualityContext => auxCon(j.context, j.context1, j.context2).isDefined
         case _ => false
      }
      def simplify(t: Obj)(implicit stack: Stack, history: History) =
         controller.simplifier(t, stack.context, rules)
      def outerContext = context ++ querySolution
   }

   /**
    *  the matching function
    *  @param goal the term to be matched
    *  @param query the term to match against, this term may contain the queryVars and the context variables freely
    *  @return true if the terms match
    */ 
   def apply(goal: Term, query: Term) = {
      log(s"matching $queryVars such that context |- $goal = $query")
      val res = aux(Nil, goal, query)
      if (res) log("matched: " + getSolution)
      else log("no match")
      res
   }
   
   /**
    *  the main recursion function that tries to match context, queryVars, bound |- goal = query
    *  @param goalOrg the term to be matched
    *    goal may not contain variables from queryVar
    *  @param queryOrg the term to match against
    *  @param boundOrg the bound variables that have been encountered during recursion
    *  @return true if the terms match
    */ 
   private def aux(boundOrg: Context, goalOrg: Term, queryOrg: Term): Boolean = {
      // 1) reflexivity
      if (goalOrg hasheq queryOrg) return true
      // 2) try term-based equality rules
      equalityRules.filter(r => r.applicable(goalOrg, queryOrg)) foreach {r =>
         r(callback)(goalOrg, queryOrg, None)(Stack(boundOrg), NoHistory) foreach {cont => return cont()}
      }
      // 3) try to isolate a query variable 
      // j is the equality judgment that we try to apply solution rules to
      var j = Equality(Stack(boundOrg), queryOrg, goalOrg, None)
      // applies all the rules found by findSolvableVariable
      def applyRules(rs: List[SolutionRule]) {
         rs.foreach {r =>
            j = r(j).getOrElse(return)._1
         }
      }
      // check if a query variable can be isolated and do so
      Solver.findSolvableVariable(solutionRules, querySolution, queryOrg) foreach {case (rs, x) => applyRules(rs)}
      val (goal, query, bound) = (j.tm2, j.tm1, j.context)
      // 4) default: congruence
      (goal, query) match {
         case (_,OMV(x)) if context.isDeclared(x) || bound.isDeclared(x) =>
            // these variables are treated like constants
            // this case must come first because they bound variables might shadow query variables 
            goal == query
         case (_,OMV(x)) if querySolution.isDeclared(x) =>
            // x2 must be a query variable (check anyway, to avoid subtle bugs)
            if (!queryVars.isDeclared(x))
               throw ImplementationError(s"undeclared variable $x")
            // if goal does not contain bound variables, solve for x
            if (goal.freeVars.forall(v => !bound.isDeclared(v)))
               solve(x, goal)
            else
               false
         // recurse into components
         case (OMA(f1, args1), OMA(f2,args2)) =>
            ((f1 :: args1) zip (f2 :: args2)) forall {
               case (x,y) => aux(bound, x,y)
            }
         // recurse into components, keeping track of variables
         case (OMBINDC(b1, bound1, sc1), OMBINDC(b2, bound2, sc2)) =>
            val bM  = aux(bound, b1, b2)
            if (!bM) return false
            val rename = auxCon(bound, bound1, bound2).getOrElse(return false)
            (sc1 zip sc2) forall {
               case (s1, s2) => aux(bound ++ bound2, s1 ^? rename, s2)
            }
         case (l1: OMLIT, l2: OMLIT) => l1.value == l2.value
         // this case works for constants (true if equal), and all asymmetric combinations (always false) 
         case (t1,t2) => t1 == t2 // TODO: other cases
      }
   }
   
   /**
    * like aux but for contexts: We match for context, queryVars, bound |- goal = query : Ctx
    * @param goal a context
    * @param query a context with query variables to match against goal
    * @param context joint free variables of goal and query
    * @return if goal and query match up to alpha-renaming, the substitution goal -> query that performs the alpha-renaming
    * 
    * terms occurring inside goal are alpha-renamed accordingly
    */
   private def auxCon(bound: Context, goal: Context, query: Context): Option[Substitution] = {
      if (goal.length != query.length) return None
      var rename = Substitution()
      (goal zip query.declsInContext.toList) foreach {
         case (VarDecl(x1,tp1,df1, _), (queryBound, VarDecl(x2,tp2, df2, _))) =>
            List((tp1,tp2), (df1,df2)) foreach {
               case (None,None) => true
               case (Some(t1), Some(t2)) =>
                  aux(bound ++ queryBound, t1 ^? rename, t2)
               case _ => return None
            }
            rename ++= (x1/OMV(x2))
      }
      Some(rename)
   }
}