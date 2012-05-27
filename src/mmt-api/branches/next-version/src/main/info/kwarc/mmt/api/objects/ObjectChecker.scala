package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
//import info.kwarc.mmt.api.objects._
import libraries._
import modules._
import symbols._
//import utils._
import frontend._
import objects.Conversions._
import scala.collection.mutable.HashSet


abstract class Feature {
	val typeConstructor: Path 
	val introSymbol: Path 
	val introIsInjective: Boolean 

	def getConstructor() : Path = {
		return typeConstructor
	}
	
	// don't repeat vars in the context when doing extensionality; can I enforce this at this stage?
	def extensionalityRule(t: TypingJudgement): Term
	
	def computationRules(tm: Term): Term
	
	def introRule(t: TypingJudgement): List[TypingJudgement] 
}
	
class UnificationProblem(report: Report) {
	
	def log(msg : => String) = report("generic unification", msg)
	
	var equation: EqualityJudgement = _
	var metaContext: Context = Context()
	var solution: Substitution = Substitution()
	
	// if true, it doesn't mean necessarily that equation has sols; look at metavars
	def unifyMMT(equation: EqualityJudgement) : Boolean = {
		
		println()
		println("Unification step in MMT")
		equation.toTerms() match {
		  	case (OMV(name),tm) => 
		  	  	if ( metaContext.isDeclared(name) && UnificationOperations.occurCheck(OMV(name), tm) ) {
		  	  		val sub = Sub(name, tm)
		  	  		var newEquation = UnificationOperations.replace(equation, sub)
		  	  		solution = solution  ++ sub
		  	  		unifyMMT(newEquation)
		  	  	}
		  	  	else {
		  	  		// if name is not in the metaContext, we are not interested in it. Also, it is a bound var, so we
		  	  		// cannot substitute it
		  	  		// if name is in the term, again the problem doesn't have solutions
		  	  		return false
		  	  	}
		  	
		  	// treated exactly the same as the previous case
		  	case (tm, OMV(name)) => 
		  	  	if ( metaContext.isDeclared(name) && UnificationOperations.occurCheck(OMV(name), tm) ) {
		  	  		val sub = Sub(name, tm)
		  	  		var newEquation = UnificationOperations.replace(equation, sub)
		  	  		solution = solution  ++ sub
		  	  		unifyMMT(newEquation)
		  	  	}
		  	  	else {
		  	  		// if name is not in the metaContext, then it is a bound variable so it cannot be substituted
		  	  		// if name is in the term, again the problem doesn't have solutions
		  	  		return false
		  	  	}
		  	  	
		  	case (OMID(name1), OMID(name2)) =>
		  	  	println("equality of constants")
		  	  	println()
		  	  	// smart definition expansion
		  	  	if (name1 == name2)
		  	  		return true
		  	  	else
		  	  		return false
		  	  	
		  	case (OMID(name), tm) => return true
		  		/*	
		  	  	UnificationOperations.lookupDef(name) match {
		  	  	  	case Some(definiens) => unifyMMT(new EqualityJudgement(context, definiens, term, T))
		  	  	  	case None => return false
		  	  	}
		  	  	*/
		  	
		  	case (tm, OMID(name)) => return true
		  	    // the same as above
		  	
		  	case (OMA(x, listx), OMA(y, listy)) =>
		  	  	println("OMA")
		  	  	var equations = UnificationOperations.decApp(equation)
		  	  	equations match {
		  	  	  	case Some(listEquations) =>	return (listEquations.map(unifyMMT)).foldLeft(true)((b,a) => b && a)
		  	  	  	case None => return false // think more about this case
		  	  	}
		  	     
		  	case (OMBIND(b1, context1, tm1), OMBIND(b2, context2, tm2)) =>  
		  	  	if ( b1 != b2)
		  	  		return false
		  	  	else {
		  	  		UnificationOperations.decBinder(context1, context2) match {
		  	  			case Some((subst,listEquations)) => return (listEquations.map(unifyMMT)).foldLeft(true)((b,a) => b && a) &&
		  	  												unifyMMT(new EqualityJudgement(context1,tm1,tm2^subst,None))
		  	  			// for the unification problem related to tm1, tm2, the context is relevant only for the variable names
		  		  		// context1 = context2 after substitution, so it doesn't matter which one we put
		  	  			case None => return false
		  		  				
		  	  		}
		  	  	}
		  		
		  	case _ => return true 	
		}
	}
	
	def getSolution() : Option[Substitution]= {
		// check if we have found two values for the same variable - is this possible?
		// check if we have found values for all metavariables
	
		var listVarsSubst = List[Content]()
		for( s <- solution.components)
			 listVarsSubst = (s.components).head :: listVarsSubst
		/*
		for (metavar <- metaContext)
			listVarSubst.contains(metavar)
		*/
		return Some(solution)
	}
}


class UnificationProblemTT(report: Report) extends UnificationProblem(report) {
  
	override def log(msg : => String) = report("tt unification", msg)
	
	var typeTheory: List[Feature] = Nil
	
	def unifyTT(equation: EqualityJudgement)(implicit lib: Lookup) : Boolean = {
	
		println()
		println("Unification step in TT")
		/*
		var T  = equation.typeAtEquality
		var tm1 = equation.term1
		var tm2 = equation.term2
		var context = equation.context
		println(tm1)
		println(tm2)
		println(T)
		*/
	  
		equation.headOfType() match {
		  	case Some(h) => 
		  	  	println("head of type: " + h)
		  	  	
		  	  	// check to see if type is simple or composed
		  	  	var sw = false
		  	  	
		  	  	for(f <- typeTheory) {
		  	  		if( h == f.getConstructor()) {
		  	  			// we are at a composed type
		  	  			sw = true
		  	  		  
						var t1 = equation.toTypingFirst()
						var t2 = equation.toTypingSecond()
						var tm1Eta = f.extensionalityRule(t1)
						var tm2Eta = f.extensionalityRule(t2)
						
						println("extensionality application; context and type don't change")
						println(tm1Eta.toString())
						println(tm2Eta.toString())
						
						// tm1Eta should always be different from t1 and the same for tm2Eta
						//(tm1Eta == tm1 && tm2Eta == tm2 && f.introIsInjective)	
						// how can I enforce that in extensionality rule, in the implementation
						
						if (f.introIsInjective) {
						  
							var listOfComp1 = f.introRule(t1.replaceTerm(tm1Eta))
							var listOfComp2 = f.introRule(t2.replaceTerm(tm2Eta))
								
							if (listOfComp1.length != listOfComp2.length)
								// think better, you are in a type theory
								return false	
							else {
								println("introduction rule")
								for ((ta,tb) <- listOfComp1.zip(listOfComp2)) {
									println("first term")
									println("-context " + ta.getContext().toString())
									println("-term " + ta.getTerm().toString())
									println("-type " + ta.getType().toString())
									println("second term")
									println("-context " + tb.getContext().toString())
									println("-term" + tb.getTerm().toString())
									println("-type" + tb.getType().toString())
									println()
									//unifyTT(new EqualityJudgement(a.getContext(),a.getTerm(),b.getTerm(),a.getType()))
									
									
									UnificationOperations.decBinder(ta.getContext(),tb.getContext()) match {
									  	case Some((substContext,listEquations)) =>
									  	  /*
									  	  	var h = listEquations.head
											println((h.getContext()).toString())
											println((h.getFirstTerm()).toString())
											println((h.getSecondTerm()).toString())
											println((h.getType()).toString())
											println(substContext.toString())
									  	  	*/
									  	  	(ta.getType(),tb.getType()) match {
									  	  	  	case (None, Some(tbType)) => return false
									  	  	  	case (Some(taType), None) => return false
									  	  	  	case (Some(taType), Some(tbType)) =>
									  	  	  	  	// formulate a new unification problem for types
									  	  	  	  	/*
									  	  	  	  	println()
									  	  	  	  	println(taType.toString())
									  	  	  	  	println(tbType.toString())
									  	  	  	  	println()
									  	  	  	  	println()
									  	  	  	  	*/
									  	  	  	  	println("unify contexts")
									  	  	  	  	println()
									  	  	  	  	var v = (listEquations.map(unifyMMT)).foldLeft(true)((b,a) => b && a)
									  	  	  	  	if (v) {
										  	  	  	  	var problem = new UnificationProblem(report)
										  	  	  	  	println("unify types")
										  	  	  	  	problem.unifyMMT(new EqualityJudgement(ta.getContext(), taType,
										  	  										tbType^substContext, None))
										  	  			problem.getSolution() match {
									  	  					case None => return false
									  	  					case Some(substType) =>
									  	  					  	println("unify terms")
									  	  				  		v = v && unifyTT( new EqualityJudgement( ta.getContext(),
														  	  	  			  						 ta.getTerm(),
														  	  	  			  						 tb.getTerm()^substContext, 
														  	  	  			  						 Some(tbType^substType)))
														  	  	return v
										  	  	  	  	}
									  	  	  	  	}
									  	  	  	  	else
									  	  	  	  		return false
									  	  	  	case (None, None) =>
									  	  	  	  	println("unify contexts")
									  	  	  	  	var v = (listEquations.map(unifyMMT)).foldLeft(true)((b,a) => b && a)
									  	  	  	  	println("no type for equation")
									  	  	  	  	println("unify terms")
									  	  	  	  	v = v && unifyMMT(new EqualityJudgement(ta.getContext(),
									  	  	  	  											ta.getTerm(),
									  	  	  	  											tb.getTerm()^substContext, 
									  	  	  	  											None))
									  	  	}
									  	  	
									  	  	
									  	case None => return false
									}
									
								}
							}
						}
						else {
							//var newEquation = equation.replaceTerms(tm1Eta,tm2Eta)
							//return unifyTT(newEquation)
							return unifyMMT(equation) // since symbol is not injective, we cannot really do but MMT unification
						}
		  	  		}
		  	  	}
		  	  	
		  	  	if (!sw) {
		  	  		// we are at a base type
		  	  		println("beta")
		  	  		for (f <- typeTheory)  {
		  	  			var tm1 = equation.getFirstTerm()
		  	  			var tm2 = equation.getSecondTerm()
		  	  			var tm1Beta = f.computationRules(tm1)
		  	  			var tm2Beta = f.computationRules(tm2)
		  	  			println(tm1Beta.toString())
		  	  			println(tm2Beta.toString())
		  	  			if (tm1 != tm1Beta || tm2 != tm2Beta)
		  	  				return unifyTT(equation.replaceTerms(tm1Beta,tm2Beta))	
		  	  			else 
		  	  				return unifyMMT(equation)	
		  	  		}
		  	  	}

				return true	
			
			// should it throw an error b/c we are in the type theory case?
			case None => return unifyMMT(equation) // if the head doesn't exist, then we don't have a type => MMT unif
		
		}	
	}
}




//=========================DECLARATIONS=============================
/*
/** the type of object level judgments as used for typing and equality of terms */
-what is the purpose of free vars?
abstract class Constraint {
  /** @return the set of names of the meta-variables occurring in this judgment
   *    Constraints must come with a Context binding all object variables occurring freely in any expressions;
   *    therefore, the remaining free variables are meta-variables
   */ 
  def freeVars : HashSet[LocalName]
}
/** represents an equality judgment, optionally at a type
 * context |- t1 = t2 : tp  if t = Some(tp)
 * or
 * context |- t1 = t2       if t = None
 */
case class EqualityConstraint(context: Context, t1: Term, t2: Term, t: Option[Term]) extends Constraint {
   lazy val freeVars = {
     val ret = new HashSet[LocalName]
     val fvs = context.freeVars_ ::: t1.freeVars_ ::: t2.freeVars_ ::: (t.map(_.freeVars_).getOrElse(Nil))
     fvs foreach {n => if (! context.isDeclared(n)) ret += n}
     ret
   }
}
/** represents a typing judgment
 * context |- tm : tp
 */
case class TypingConstraint(context: Context, tm: Term, tp: Term) extends Constraint {
  lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = context.freeVars_ ::: tm.freeVars_ ::: tp.freeVars_
    fvs foreach {n => if (! context.isDeclared(n)) ret += n}
    ret
  }
}

//TODO case class InhabitationConstraint(name: LocalName, tp: Term) extends Constraint

/** A wrapper around a Constraint to maintain meta-information while that Constraint is delayed */
class DelayedConstraint(val constraint: Constraint) {
  private val freeVars = constraint.freeVars
  private var activatable = false
  /** This must be called whenever a variable that may occur free in this constraint has been solved */
  def solved(name: LocalName) {
     if (! activatable && (freeVars contains name)) activatable = true
  }
  /** @return true iff a variable has been solved that occurs free in this Constraint */
  def isActivatable: Boolean = activatable
}


abstract class TypeConstructor {
   def extensionalityRule(t: Term) : Term
   def extensionalityRule(eq: EqualConstraint) : Option[List[EqualConstraint]]
   def introSymbol: Path
   def computationRule(t: Term): Option[Term]
}

class Unifier {
   val typeCons : List[TypeConstructor]
   def unify(con: Context, t1: Term, t2: Term, t: Term) {
      normal MMT equality check
      if t1 and t2 have the same shape and the same head, and the head is known to be an introSymbol (and thus injective) recurse into components
      otherwise, try extend
   }
   def extend(con: Context, t1: Term, t2: Term, t: Term) {
      (typeCons find {tc => (tc.typeSymbol == t.head)}) match {
        case Some(tc) => unify (... tc.extensionalityRule(...) ...)
        case None =>
           compute(con, t1, t2)
      }
   }
   def compute(con: Context, t1: Term, t2: Term) {
      val t1C = typeCons findMap {tc => tc.computationRule(t1)}
      val t2C = typeCons findMap {tc => tc.computationRule(t1)}
      unify(..)
   }
}
*/





//=================================SOLVER==================================
/*
/**
 * A Solver is used to solve a system of judgments about Term's, given by Constraint's,
 * by applying typing rules to validate the judgment.
 * The judgments may contain unknown variables (also called meta-variables or logic variables);
 * the solution is a Substitution that provides a closed Term for every unknown variable.
 * A new instance must be created for every system of judgments. 
 * @param controller used for looking up Foundation's and Constant's. No changes are made to controller.
 * @param unknown the list of all unknown variables in dependency order;
 *   unknown variables may occur in or as the types of each other
 */
class Solver(controller: Controller, unknowns: Context) {
   /** tracks the solution, like unknowns but a definiens is added for every solved variable */ 
   private var solution : Context = unknowns
   /** tracks the delayed constraints, in any order */ 
   private var delayed : List[DelayedConstraint] = Nil
   /** true if unresolved constraints are left */
   def hasUnresolvedConstraints : Boolean = ! delayed.isEmpty
   /** true if unsolved variables are left */
   def hasUnsolvedVariables : Boolean = solution.toSubstitution.isEmpty
   /** the solution to the constraint problem
    * @return None if there are unresolved constraints or unsolved variables; Some(solution) otherwise 
    */
   def getSolution : Option[Substitution] = if (delayed.isEmpty) solution.toSubstitution else None

   /** delays a constraint for future processing */
   private def delay(c: Constraint): Boolean = {
      val dc = new DelayedConstraint(c)
      delayed = dc :: delayed
      true
   }
   /** activates a previously delayed constraint if one of its free variables has been solved since */
   private def activate: Boolean = {
      delayed find {_.isActivatable} match {
         case None => true
         case Some(dc) => apply(dc.constraint)
      }
   }
   /** registers the solution for a variable; notifies all delayed constraints */
   //TODO solutions should also be propagated to currently active constraints
   private def solve(name: LocalName, value: Term): Boolean = {
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.df.isDefined)
         checkEquality(value, solved.df.get, solved.tp)(Context())
      else {
         solution = left ::: solved.copy(df = Some(value)) :: right
         delayed foreach {_.solved(name)}
         true
      }
   }
   /** applies this Solver to one constraint
    *  this method can be called multiple times to solve a system of constraints
    *  @param c the constraint
    *  @return false only if the constraints are unsatisfiable; true if constraints have been resolved or delayed  
    */
   def apply(c: Constraint): Boolean = {
     val subs = solution.toPartialSubstitution
     val result = c match {
        case TypingConstraint(con, tm, tp) =>
           checkTyping(tm ^ subs, tp ^ subs)(con ^ subs)
        case EqualityConstraint(con, tm1, tm2, tp) =>
           checkEquality(tm1 ^ subs, tm2 ^ subs, tp map {_ ^ subs})(con ^ subs)
     }
     activate
   }
   /** proves a TypingConstraint by recursively applying rules and solving variables where applicable,
    *  delays a constraint if unsolved variables preclude further processing
    *  checkTyping(tm, tp)(con) solves the judgment con |- tm : tp
    *  @return false only if the judgment does not hold; true if it holds or constraint have been delayed
    */
   private def checkTyping(tm: Term, tp: Term)(implicit con: Context): Boolean = {
      true
   }
   /** handles an EqualityConstraint by recursively applying rules and solving variables where applicable,
    *  delays a constraint if unsolved variables preclude further processing
    *  @param tpOpt if empty, tm1 and tm2 may be ill-typed; if non-empty, they must also type-check at that type
    *  @return false only if the judgment does not hold; true if it holds or constraint have been delayed
    */
   private def checkEquality(tm1: Term, tm2: Term, tpOpt: Option[Term])(implicit con: Context): Boolean = {
      // the common case of identical terms
      if (tm1 == tm2) tpOpt match {
         case None => true
         case Some(tp) => checkTyping(tm1, tp)
      } else (tm1, tm2) match {
         // if no case applied, try the remaining cases
         case _ => checkEquality2(tm1, tm2, tpOpt, true)
      }
   }
   /** like checkEquality, but contains all those cases that are tried twice:
    *  if no case applies, we flip tm1 and tm2 and try again; if still no case applies, we delay
    * @param firstTime true if called for the first time, false if called for the second time
    */
   private def checkEquality2(tm1: Term, tm2: Term, tpOpt: Option[Term], firstTime: Boolean)(implicit con: Context): Boolean = {
      (tm1, tm2) match {
         // |- x = t: solve x as t
         case (OMV(x), t) =>
            if (unknowns.isDeclared(x)) {
               if (! t.freeVars.isEmpty) {
                  solve(x, t)
                  //check x.tp=tp? or t:x.tp?
               } else {
                 delay(EqualityConstraint(con, tm1, tm2, tpOpt))
               }
            } else {
               delay(EqualityConstraint(con, tm1, tm2, tpOpt))
            }
         case _ =>
            // if no case applied, ...
            if (firstTime)
               // flip the arguments and try again, or ...
               checkEquality2(tm2, tm1, tpOpt, false)
            else
               // delay if we've tried that already
               delay(EqualityConstraint(con, tm2, tm1, tpOpt))
      }
   }
}
*/

