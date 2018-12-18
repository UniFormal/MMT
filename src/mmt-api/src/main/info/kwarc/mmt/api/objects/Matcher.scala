package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import frontend._
import checking._
import uom._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.symbols._
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
 *
 *  the class can be reused for multiple matches using the same RuleSet but is not thread-safe
 */
class Matcher(controller: Controller, rules: RuleSet) extends Logger {
   def logPrefix = "matcher"
   def report = controller.report
   private def presentObj(o: Obj) = controller.presenter.asString(o)

   private val solutionRules = rules.get(classOf[ValueSolutionRule])
   private val equalityRules = rules.get(classOf[TermBasedEqualityRule])
   
   /** the context of the goal */
   private var constantContext: Context = Context.empty
   /** the context of the query (containing the variables to be solved) */
   private var querySolution : Context = Context.empty
   def getUnsolvedVariables : List[LocalName] = querySolution collect {
      case vd if vd.df.isEmpty => vd.name
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
      val valueS = controller.simplifier(value, constantContext ++ left, rules, false)
      solved.tp foreach {tp =>
         val valueTp = Solver.infer(controller, constantContext, value, Some(rules)).getOrElse(return false)
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

      def lookup = controller.globalLookup
      def simplify(t: Obj)(implicit stack: Stack, history: History) =
         controller.simplifier(t, SimplificationUnit(stack.context,false,false), rules)
      def outerContext = constantContext ++ querySolution

      def getTheory(tm : Term)(implicit stack : Stack, history : History) : Option[AnonymousTheory] = simplify(tm) match {
         case AnonymousTheory(mt, ds) =>
            Some(new AnonymousTheory(mt, Nil))
         // add include of codomain of mor
         case OMMOD(mp) =>
            val th = controller.globalLookup.getO(mp) match {
               case Some(th2: Theory) => th2
               case _ => return None
            }
            val ds = th.getDeclarationsElaborated.map({
               case c: Constant =>
                  OML(c.name, c.tp, c.df, c.not)
               case PlainInclude(from, to) =>
                  IncludeOML(from, Nil)
               case _ => ???
            })
            Some(new AnonymousTheory(th.meta, ds))
         case _ =>
            return None
      }

      override def safeSimplifyUntil[A](tm: Term)(simple: Term => Option[A])(implicit stack: Stack, history: History): (Term, Option[A]) = {
         val s = simplify(tm)
         (s, simple(s))
      }
   }

   /**
    *  the matching function
    *  @param goalContext the global context
    *  @param goal the term to be matched, relative to goalContext
    *  @param queryVars the variables to solve
    *  @param query the term to match against, relative to goalContext ++ queryVars
    *  @return MatchSuccess(subs) if goal == query ^ subs for  goalContext |- subs:queryVars -> .
    */
   def apply(goalContext: Context, goal: Term, queryVars: Context, query: Term): MatchResult = {
     apply(goalContext, queryVars) {eq => eq(goal, query)}
   }

   /**
    *  most general matching function that allows arbitrary checks
    *
    *  @param doit a function that takes an equality predicate (for matching) and returns true if the match is possible
    *  e.g., basic matching is obtained as apply(queryVars){eq => eq(goal, query)}
    */
   def apply(goalContext: Context, queryVars: Context)(doit: ((Term,Term) => Boolean) => Boolean): MatchResult = {
      constantContext = goalContext
      querySolution = queryVars
      val res = doit(matchTerms)
      if (res) {
        log("matched: " + getSolution)
        val total = getUnsolvedVariables.isEmpty
        MatchSuccess(getSolution, total)
      } else {
        log("no match")
        MatchFail
      }
   }

   /** tp level match function */
   private def matchTerms(goal: Term, query: Term) = {
     log(s"matching ${presentObj(querySolution)} such that |- ${presentObj(goal)} = ${presentObj(query)}")
     aux(Nil, goal, query)
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
         r(callback)(goalOrg, queryOrg, None)(Stack(boundOrg), NoHistory) foreach {cont =>
           return cont()
         }
      }
      // 3) try to isolate a query variable
      // j is the equality judgment that we try to apply solution rules to
      var j = Equality(Stack(boundOrg), goalOrg, queryOrg, None)
      // applies all the rules found by findSolvableVariable
      def applyRules(rs: List[ValueSolutionRule]) {
         rs.foreach {r =>
            j = r(j).getOrElse(return)._1
         }
      }
      // check if a query variable can be isolated and do so
      Solver.findSolvableVariable(solutionRules, querySolution, queryOrg) foreach {case (rs, x) => applyRules(rs)}
      // 4) default: congruence
      CongruenceClosure(j) forall {eq =>
        val bound = eq.context
        val goal = eq.tm1
        val query = eq.tm2
        (goal, query) match {
           case (_,OMV(x)) if constantContext.isDeclared(x) || bound.isDeclared(x) =>
              // these variables are treated like constants
              // this case must come first because the bound variables might shadow query variables
              goal == query
           case (_,OMV(x)) if querySolution.isDeclared(x) =>
              // if goal does not contain bound variables, solve for x
              if (goal.freeVars.forall(v => !bound.isDeclared(v)))
                 solve(x, goal)
              else
                 false
           case (t1,t2) => t1 hasheq t2
        }
      }
   }

   /**
    * like aux but for contexts: We match for context, queryVars, bound |- goal = query : Ctx
    * @param goal a context
    * @param query a context with query variables to match against goal
    * @param context joint free variables of goal and query
    * @return if goal and query match up to alpha-renaming, the substitution query -> goal that performs the alpha-renaming
    *
    * terms occurring inside query are alpha-renamed accordingly
    */
   private def auxCon(bound: Context, goal: Context, query: Context): Option[Substitution] = {
      if (goal.length != query.length) return None
      var rename = Substitution()
      (goal.declsInContext.toList zip query) foreach {
         case ((goalBound, VarDecl(x1,f1,tp1,df1, _)), VarDecl(x2,f2,tp2, df2, _)) =>
            if (f1 != f2) return None
            List((tp1,tp2), (df1,df2)) foreach {
               case (None,None) => true
               case (Some(t1), Some(t2)) =>
                  aux(bound ++ goalBound, t1, t2 ^? rename)
               case _ => return None
            }
            rename ++= (x2/OMV(x1))
      }
      Some(rename)
   }
}

/** returned by the [[Matcher]] */
abstract class MatchResult
/**
 * @param solution the substitution for the query variables
 * @param total true if all query variables were matched
 */
case class MatchSuccess(solution: Substitution, total: Boolean) extends MatchResult
/** definitely not equal */
case object MatchFail extends MatchResult
