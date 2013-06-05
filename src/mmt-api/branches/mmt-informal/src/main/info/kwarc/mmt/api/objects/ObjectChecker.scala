package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._

//import info.kwarc.mmt.api.objects._
import libraries._
import modules._
import symbols._
import frontend._
import objects.Conversions._
import scala.collection.mutable.{HashSet,HashMap}

/* ideas
 * inferType guarantees well-formedness (not done yet by LambdaTerm)
 *   but what if there unknowns whose type cannot be inferred? Is that even possible?
 * checkEquality with type assumes well-typing if
 * checkEquality without type infers both types and checks their equality
 * checkTyping is simply solveType, inferType and checkEquality; no typingRules (works at least if there are unknows)?
 *   does that work for all type-formers?
 * equality calls simplify only if no equality rule is found; then it includes computation, definition expansion
 * limitedSimplify must include computation, definition expansion, but can stop on GlobalChange; but safety is usually needed
 */

/** A wrapper around a Judgement to maintain meta-information while a constraint is delayed */
class DelayedConstraint(val constraint: Judgement) {
  private val freeVars = constraint.freeVars
  private var activatable = false
  /** This must be called whenever a variable that may occur free in this constraint has been solved */
  def solved(names: List[LocalName]) {
     if (! activatable && (names exists {name => freeVars contains name})) activatable = true
  }
  /** @return true iff, since delaying, a variable has been solved that occurs free in this Constraint */
  def isActivatable: Boolean = activatable
  override def toString = constraint.toString
}

/**
 * A Solver is used to solve a system of constraints about Term's given as judgments.
 * 
 * The judgments may contain unknown variables (also called meta-variables or logic variables);
 * variables may represent any MMT term, i.e., object language terms, types, etc.;
 * the solution is a Substitution that provides a closed Term for every unknown variable.
 * (Higher-order abstract syntax should be used to represent unknown terms with free variables as closed function terms.) 
 * 
 * The Solver decomposes the judgments individually by applying typing rules, collecting (partial) solutions along the way and possibly delaying unsolvable judgments.
 * If the unknown variables are untyped and rules require a certain type, the Solver adds the type.
 * 
 * Unsolvable constraints are delayed and reactivated if later solving of unknowns provides further information.
 * 
 * @param controller an MMT controller that is used to look up Rule's and Constant's. No changes are made to the controller.
 * @param unknowns the list of all unknown variables including their types and listed in dependency order;
 *   unknown variables may occur in the types of later unknowns.
 * 
 * Use: Create a new instance for every problem, call apply on all constraints, then call getSolution.  
 */
class Solver(val controller: Controller, theory: Term, unknowns: Context) extends Logger {
   /** tracks the solution, initially equal to unknowns, then a definiens is added for every solved variable */ 
   private var solution : Context = unknowns
   /** the unknowns that were solved since the last call of activate (used to determine which constraints are activatable) */
   private var newsolutions : List[LocalName] = Nil
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
   /**
    * @return the current partial solution to the constraint problem
    * This solution may contain unsolved variables, and there may be unresolved constraints. 
    */
   def getPartialSolution : Context = solution
   /**
    * @return the context containing only the unsolved variables
    * This solution may contain unsolved variables, and there may be unresolved constraints. 
    */   
   def getUnsolvedVariables : Context = solution.filter(_.df.isEmpty)
   /**
    * @return the current list of unresolved constraints
    */
   def getConstraints : List[Judgement] = delayed.map(_.constraint)

   /** for Logger */ 
   val report = controller.report
   /** prefix used when logging */ 
   val logPrefix = "object-checker"
   /** shortcut for the global Lookup of the controller; used to lookup Constant's */
   private val content = controller.globalLookup
   /** shortcut for the RuleStore of the controller; used to retrieve Rule's */
   private val ruleStore = controller.extman.ruleStore
   
   /** @return a string representation of the current state */ 
   override def toString = {
      "  unknowns: " + unknowns.toString + "\n" +
      "  solution: " + solution.toString + "\n" +
      "  constraints:\n" + delayed.mkString("  ", "\n  ", "") 
   }
   
   /** delays a constraint for future processing
    * @param c the Judgement to be delayed
    * @return true (i.e., delayed Judgment's always return success)
    */
   private def delay(c: Judgement): Boolean = {
     log("delaying " + c)
      val dc = new DelayedConstraint(c)
      delayed = dc :: delayed
      true
   }
   /** activates a previously delayed constraint if one of its free variables has been solved since
    * @return whatever application to the delayed judgment returns
    */
   private def activate: Boolean = {
      delayed foreach {_.solved(newsolutions)}
      newsolutions = Nil
      delayed find {_.isActivatable} match {
         case None => true
         case Some(dc) =>
           delayed = delayed filterNot (_ == dc)
           apply(dc.constraint)
      }
   }
   /** registers the solution for a variable
    * 
    * If a solution exists already, their equality is checked.
    * @param name the solved variable
    * @param value the solution; must not contain object variables, but may contain meta-variables that are declared before the solved variable
    * @return true unless the solution differs from an existing one
    */
   private def solve(name: LocalName, value: Term)(implicit stack: Stack) : Boolean = {
      log("solving " + name + " as " + value)
      // use controller.uom.simplify? yes, if well-formedness is guaranteed at this point
      val valueS = simplify(value)
      parser.SourceRef.delete(valueS) // source-references from looked-up types may sneak in here
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.df.isDefined)
         checkEquality(valueS, solved.df.get, solved.tp) //TODO
      else {
         solution = left ::: solved.copy(df = Some(valueS)) :: right
         newsolutions = name :: newsolutions
         true
      }
   }
   /** registers the solved type for a variable
    * 
    * If a type exists already, their equality is checked.
    * @param name the variable
    * @param value the type; must not contain object variables, but may contain meta-variables that are declared before the solved variable
    * @return true unless the type differs from an existing one
    */
   private def solveType(name: LocalName, value: Term)(implicit stack: Stack) : Boolean = {
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.tp.isDefined)
         checkEquality(value, solved.tp.get, None) //TODO
      else {
         solution = left ::: solved.copy(tp = Some(value)) :: right
         //no need to register this in newsolutions
         true
      }
   }
   
   /** applies this Solver to one Judgement
    *  This method can be called multiple times to solve a system of constraints.
    *  @param j the Judgement
    *  @return false if the Judgment is definitely not provable; true if it has been proved or delayed
    */
   def apply(j: Judgement) : Boolean = {
     log("judgment: " + j)
     log("state: \n" + this.toString)
     val subs = solution.toPartialSubstitution
     val mayhold = j match {
        case Typing(stack, tm, tp) =>
           checkTyping(tm ^ subs, tp ^ subs)(stack ^ subs)
        case Equality(stack, tm1, tm2, tp) =>
           def prepare(t: Term) = simplify(t ^ subs)(stack)
           checkEquality(prepare(tm1), prepare(tm2), tp map prepare)(stack ^ subs)
        case Universe(stack, tm) =>
           val stackS = stack ^ subs
           inferType(tm ^ subs)(stackS) match {
              case None => delay(j)
              case Some(univ) => checkUniverse(univ)(stackS)
           }
        case IsMorphism(stack, mor, from) =>
           checkMorphism(mor ^ subs, from ^ subs)(stack^subs)
     }
     if (mayhold) activate else false
   }

   /** proves a Typing Judgement by recursively applying TypingRule's and InferenceRule's. Takes reflection into account, so it needs frames */
   def checkTyping(tm: Term, tp: Term)(implicit stack: Stack) : Boolean = {
     log("typing: " + stack.context + " |- " + tm + " : " + tp)
     report.indent
     val res = tm match {
       // the foundation-independent cases
       case OMV(x) => (unknowns ++ stack.context)(x).tp match {
         case None =>
            if (unknowns.isDeclared(x))
              solveType(x, tp) //TODO: check for occurrences of bound variables?
            else
              false //untyped variable type-checks against nothing
         case Some(t) => checkEquality(t, tp, None)
       }
       case OMS(p) =>
         val c = content.getConstant(p)
         c.tp match {
           case None => c.df match {
             case None => false //untyped, undefined constant type-checks against nothing
             case Some(d) => checkTyping(d, tp) // expand defined constant
           }
           case Some(t) => checkEquality(t, tp, None)
         }
       // the foundation-dependent cases
       case tm =>
         limitedSimplify(tp,ruleStore.typingRules) match {
           case (tpS, Some(rule)) =>
             rule(this)(tm, tpS)
           case (tpS, None) =>
             // either this is an atomic type, or no typing rule is known
             inferType(tm) match {
               case Some(itp) => checkEquality(itp, tpS, None)
               case None => delay(Typing(stack, tm, tpS))
             }
         }
     }
     report.unindent
     res
   }

   def checkMorphism(mor: Term, from : Term)(implicit stack: Stack) : Boolean = {
     log("typing: " + stack.theory + " |- " + mor + " : " + from)
     report.indent
     val res : Boolean = from match {
       case OMMOD(p) => controller.globalLookup.getTheory(p) match {
         case thdf : DefinedTheory =>  checkMorphism(mor, thdf.df)
         case thd : DeclaredTheory =>
           val clist : List[Declaration] = thd.getDeclarations filter (p => !p.isInstanceOf[Structure])  // list of constants in the domain theory
           //val oclist : List[Declaration] = clist.sortWith((x,y) => x.name.toString() <= y.name.toString()) //ordered list of constants in the domain theory
           mor match {
             case ExplicitMorph(rec, dom) =>
               if (from == dom)
               {
                 val ocassig = rec.fields.sortWith((x,y) => x._1.toString() <=  y._1.toString()) //ordered list of constants in the morphism
                 if (clist.map(_.name) == ocassig.map(_._1)) { //testing that the local names of the constants in the domain theory are the same as those in the morphism
                   (clist zip ocassig) forall (pair =>
                    {
                     inferType(OMID(pair._1.path)) match {    //matching type of constant from domain theory
                       case Some(tp) => checkTyping(pair._2._2,OMM(tp,mor))
                       case None => true
                     }
                   })
                 }
                 else false
               }
              else false
             case _ => false
           }
         case _ => false
       }
       case _ => false
     }
     report.unindent
     res
   }

   /** infers the type of a term by applying InferenceRule's
    * @param tm the term
    * @param context its Context
    * @return the inferred type, if inference succeeded
    *
    * This method should not be called by users (instead, call apply to a typing judgement with an unknown type). It is only public because it serves as a callback for Rule's.
    */

   def inferType(tm: Term)(implicit stack: Stack): Option[Term] = {
     log("inference: " + stack.context + " |- " + tm + " : ?")
     report.indent
     val resFoundInd = tm match {
       //foundation-independent cases
       case OMV(x) => (unknowns ++ stack.context)(x).tp
       case OMS(p) =>
         val c = content.getConstant(p)
         c.tp orElse {
           c.df match {
             case None => None
             case Some(d) => inferType(d) // expand defined constant
           }
         }
       case _ => None
     }
     //foundation-dependent cases if necessary
     val res = resFoundInd orElse {
         val (tmS, ruleOpt) = limitedSimplify(tm,ruleStore.inferenceRules)
         ruleOpt match {
           case Some(rule) => rule(this)(tmS)
           case None => None
         }
     }
     report.unindent
     log("inferred: " + res.getOrElse("failed"))
     res
   }

   def checkUniverse(univ: Term)(implicit stack: Stack): Boolean = {
     log("universe: " + stack.context + " |- " + univ + " : universe")
     report.indent
     val res = limitedSimplify(univ, ruleStore.universeRules) match {
        case (uS, Some(rule)) => rule(this)(uS)
        case (uS, None) => delay(Universe(stack, uS))  
     }
     report.unindent
     res
   }
   
   case object Delay extends java.lang.Throwable
   
   /** proves an Equality Judgment by recursively applying EqualityRule's and other Rule's.
    * @param tm1 the first term
    * @param tm2 the second term
    * @param tpOpt their type; if given it is used as guidance for the selection of Rule's
    *   The well-typedness of tm1 and tm2 is neither assumed nor guaranteed;
    *   however, if tpOpt is given and the terms are well-typed, they must type-check against tpOpt.
    * @param context their context
    * @return false if the Judgment is definitely not provable; true if it has been proved or delayed
    * 
    * This method should not be called by users (instead, call apply). It is only public because it serves as a callback for Rule's.
    */
   def checkEquality(tm1: Term, tm2: Term, tpOpt: Option[Term])(implicit stack : Stack): Boolean = {
      log("equality: " + stack.context + " |- " + tm1 + " = " + tm2 + " : " + tpOpt)
      // simplify to improve chances of equating the terms  
      val tm1S = simplify(tm1)
      val tm2S = simplify(tm2)
      // first, we check for some common cases where it's redundant to do induction on the type
      // identical terms
      if (tm1S == tm2S) return true
      // solve an unknown
      val solved = tryToSolve(tm1S, tm2S, tpOpt) || tryToSolve(tm2S, tm1S, tpOpt)
      if (solved) return true

      // use the type for foundation-specific equality reasoning
      
      // first infer the type if it has not been given in tpOpt
      val tp = tpOpt match {
        case Some(tp) => tp
        case None =>
           val itp = inferType(tm1S) orElse inferType(tm2S)
           itp.getOrElse(return false)
      }
      // try to simplify the type until an equality rule is applicable 
      limitedSimplify(tp, ruleStore.equalityRules) match {
         case (tpS, Some(rule)) => rule(this)(tm1S, tm2S, tpS)
         case (tpS, None) =>
            // this is either a base type or an equality rule is missing
            // TorsoNormalForm is useful to inspect terms of base type
            val tm1T = TorsoForm.fromHeadForm(tm1S)
            val tm2T = TorsoForm.fromHeadForm(tm2S)
            val heads = tm1T.heads
            // TODO: if the torsos are constants but not equal, try to make them equal by expanding definitions
            if (tm1T.torso == tm2T.torso && heads == tm2T.heads) {
               //the two terms have the same shape, i.e., same torso and same heads
               //we can assume heads != Nil; otherwise, tm1S == tm2S would hold
               ruleStore.atomicEqualityRules.get(heads.head) match {
                  case Some(rule) => rule(this)(tm1S, tm2S, tpS)   //apply the rule for the outermost head 
                  case None => 
                    //default: apply congruence rules backwards, amounting to initial model semantics, i.e., check for same number and equality of arguments everywhere
                    (tm1T.apps zip tm2T.apps) forall {case (Appendages(_,args1),Appendages(_,args2)) =>
                        if (args1.length == args2.length)
                           (args1 zip args2) forall {case (a1, a2) => checkEquality(a1, a2, None)}
                        else false
                    }
               }
            } else
              //TODO: in some cases, we may conclude false right away
              delay(Equality(stack, tm1S, tm2S, Some(tpS)))
      }
   }

   /** tries to solve an unknown occurring as the torso of tm1 in terms of tm2.
    * It is an auxiliary function of checkEquality because it is called twice to handle symmetry of equality.
    */
   private def tryToSolve(tm1: Term, tm2: Term, tpOpt: Option[Term])(implicit stack: Stack): Boolean = {
      tm1 match {
         //foundation-independent case: direct solution of an unknown variable
         case OMV(m) if unknowns.isDeclared(m) =>
            val fvs = tm2.freeVars
            if (fvs.isEmpty) {
               val res = solve(m, tm2)
               tpOpt foreach {case tp => solveType(m, tp)}
               res
            }
            else if (fvs.forall(unknowns.isDeclared(_))) //forall {v => context.isDeclaredBefore(v,m)}
               // delay until other meta-variables are solved
               // TODO: registering solution in terms of preceding meta-variables might save time
               delay(Equality(stack,tm1,tm2,tpOpt))
            else
               delay(Equality(stack,tm1,tm2,tpOpt))
               // meta-variable solution has free variable --> type error unless free variable disappears later
               // TODO: need to simplify tm2 in case free variable disappears
               
         //apply a foundation-dependent solving rule selected by the head of tm1
         case TorsoNormalForm(OMV(m), Appendages(h,_) :: _) if unknowns.isDeclared(m) && ! tm2.freeVars.contains(m) => //TODO what about occurrences of m in tm1?
            ruleStore.solutionRules.get(h) match {
               case None => false
               case Some(rule) => rule(this)(tm1, tm2)
            }
         case _ => false
      }
   }
  
   /** applies all ForwardSolutionRules of the given priority
    * @param priority exactly the rules with this Priority are applied */
   //TODO call this method at appropriate times
   private def forwardRules(priority: ForwardSolutionRule.Priority)(implicit stack: Stack): Boolean = {
      val results = unknowns.zipWithIndex map {
         case (vd @ VarDecl(x, Some(tp), None, _*), i) =>
            implicit val con : Context = unknowns.take(i)
            limitedSimplify(tp, ruleStore.forwardSolutionRules) match {
               case (tpS, Some(rule)) if rule.priority == priority => rule(this)(vd)
               case _ => false 
            }
         case _ => false
      }
      results.exists(_ == true)
   }
   
   /** special case of the version below where we simplify until an applicable rule is found
    *  @param tm the term to simplify
    *  @param rm the RuleMap from which an applicable rule is needed
    *  @param stack the context of tm
    *  @return (tmS, Some(r)) where tmS = tm and r from rm is applicable to tmS; (tmS, None) if tm = tmS and no further simplification rules are applicable
    */  
   private def limitedSimplify[R <: Rule](tm: Term, rm: RuleMap[R])(implicit stack: Stack): (Term,Option[R]) =
      limitedSimplify[R](tm)(t => t.head flatMap {h => rm.get(h)})
   
   /** applies ComputationRule's to simplify a term until some condition is satisfied;
    *  A typical case is transformation into weak head normal form.
    *  @param tm the term to simplify (It may be simple already.)
    *  @param simple a term is considered simple if this function returns a non-None result
    *  @param stack the context of tm
    *  @return (tmS, Some(a)) if tmS is simple and simple(tm)=tmS; (tmS, None) if tmS is not simple but no further simplification rules are applicable
    */  
   private def limitedSimplify[A](tm: Term)(simple: Term => Option[A])(implicit stack: Stack): (Term,Option[A]) = {
      simple(tm) match {
         case Some(a) => (tm,Some(a))
         case None => tm.head match {
            case None => (tm, None)
            case Some(h) =>
               val rOpt = ruleStore.computationRules.get(h)
               rOpt match {
                  case None => (tm, None) //TODO test for definition expansion
                  case Some(rule) =>
                     rule(this)(tm) match {
                        case Some(tmS) =>
                           log("simplified: " + tm + " ~~> " + tmS)
                           limitedSimplify(tmS.from(tm))(simple)
                        case None => (tm, None) 
                     }
               }
         }
      }
   }
   /** applies ComputationRule's at toplevel until no further rules are applicable.
    * @param tm the term
    * @param context its context
    * @return the simplified Term
    * 
    * This method should not be called by users. It is only public because it serves as a callback for Rule's.
    */
   def simplify(tm: Term)(implicit stack: Stack): Term = {
     tm.head match {
       case None => tm
       case Some(h) => ruleStore.computationRules.get(h) match {
         case None => tm
         case Some(rule) =>
            rule(this)(tm) match {
               case Some(tmS) => simplify(tmS).from(tm)
               case None => tm
            }
       }
     }
   }

   /** applies simplify to all Term's in a Context
    * @param context the context
    * @return the simplified Context
    */
   private def simplifyStack(stack: Stack) = {
      val Stack(Frame(theory,context) :: tail) = stack
      val contextS = stack.context mapTerms {case (con, t) => simplify(t)(Stack(Frame(theory,con) :: tail))}
      Stack(Frame(theory, contextS) :: tail)
   }
}

//TODO
//equality judgment should be solved by simplification if no other rule is available
//equality freeness rule should provide type if known to avoid re-inference

object Solver {
  /** reconstructs a single term and returns the reconstructed term and its type */
  def check(controller: Controller, stack: Stack, tm: Term): Option[(Term,Term)] = {
      val (unknowns,tmU) = parser.AbstractObjectParser.splitOffUnknowns(tm)
      val etp = LocalName("expected_type")
      val oc = new Solver(controller, stack.theory, unknowns ++ VarDecl(etp, None, None))
      val j = Typing(stack, tmU, OMV(etp))
      oc(j)
      oc.getSolution map {sub =>
          val tmR = tmU ^ sub
          (tmR, sub("expected_type").get) // must be defined if there is a solution
      }
  }
}