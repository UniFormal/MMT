package info.kwarc.mmt.api.proving

import info.kwarc.mmt.api._
import objects._
import objects.Conversions._
import utils._

/**
 * a fact is a sequent derived during forward search
 * @param goal antecedent of the fact (conclusion of the goal is irrelevant)
 * @param tm the proof term
 * @param tp the proved type
 */
case class Fact(goal: Goal, tm: Term, tp: Term) {
   override def toString = tp.toString + "\n     " + tm.toString
   def present(presentObj: Obj => String) = {
      presentObj(tp) + " by " + presentObj(tm)
   }
}

/**
 * an atomic fact: a constant or a variable
 * @param tm the proof term
 * @param tp the proved type
 * @param role the of the constant/variable
 */
case class Atom(tm: Term, tp: Term, rl: Option[String]) {
   def isConstant = tm.isInstanceOf[OMID]
   def isVariable = tm.isInstanceOf[OMV]
}

/**
 * A database of facts obtained through forward proof search
 *
 * @param prover the prover
 * @param shapeDepth complexity of indexing
 * @param outerLogPrefix for logging
 */
class Facts(prover: Searcher, shapeDepth: Int, outerLogPrefix: String) extends frontend.Logger {
   val report = prover.report
   def logPrefix = outerLogPrefix + "/facts"

   private var constantAtoms : List[Atom] = Nil
   private[proving] def addConstantAtom(a: Atom): Unit = {
      constantAtoms ::= a
   }
   // these become facts during forward search (as a trivial case of ForwardPiElimination) 
   def getConstantAtoms = constantAtoms

   /**
    * the database of (non-atomic) facts, indexed by the shape of the type
    */
   private val facts = new HashMapToSet[Shape,Fact]
   /** the facts added in the current iteration */
   private var futureFacts : List[Fact] = Nil

   /**
    *  adds a fact to the database
    *  @param tm a term that is valid over the full context of this goal
    *  @param tp its type
    *
    *  the facts are not actually added immediately but queued for addition
    *  see integrateFutureFacts
    *
    *  facts are ignored if their proof does not use a free variable
    */
   def add(f: Fact): Unit = {
      if (!f.tm.freeVars.isEmpty)
         futureFacts ::= f
   }
   /**
    * adds all queued facts to the database
    */
   private[proving] def integrateFutureFacts: Unit = {
      futureFacts foreach {f =>
         val fS = prover.simplifyFact(f)
         log("new fact: " + fS.present(prover.presentObj))
         val sh = Shape(Nil, Nil, fS.tp, shapeDepth)
         facts(sh) += fS
      }
      futureFacts = Nil
   }

   /**
    * applies a function to (at least) all facts that match a query
    * @param queryVars those free variables of query to instantiate when matching
    * @param query the term to match against all facts
    * @param fun the function to apply
    *
    * only approximate matching based on shapes is performed; fun must still perform a precise match
    */
   private def foreachFact(queryVars: Context, query: Term)(fun: Fact => Unit): Unit = {
      val querySh = Shape(queryVars, Nil, query, shapeDepth)
      facts.keys foreach {sh =>
         if (Shape.matches(querySh, sh)) {
            facts(sh) foreach {f =>
               if (f.goal.isFinished)
                  facts(sh) -= f // forget facts about finished goals
               else
                  fun(f)
            }
         }
      }
   }

   /**
    * matches a facts against a query
    * @param queryVars those free variables of query to instantiate when matching
    * @param query the term to match against the fact
    * @param f the fact to match against
    * @return the pair (s: queryVars -> f.goal.fullContext, t) such that query ^ s = fact.tp, if possible
    *
    * s is partial if queryVars contains variables that do not occur in query
    */
   private def matchFact(queryVars: Context, query: Term, f: Fact): Option[(Substitution,Term)] = {
      val (queryFresh, freshSub) = Context.makeFresh(queryVars, f.goal.fullContext.map(_.name))
      val matcher = prover.makeMatcher
      val matches = matcher(f.goal.fullContext, f.tp, queryFresh, query)
      matches match {
        case MatchSuccess(solution, _) =>
           // we need freshSub ^ solution but restricted to those variables that were solved
           val freshSubRestrict = freshSub.filter {
             case Sub(_, OMV(qF)) => solution.maps(qF)
             case _ => false // impossible
           }
           Some((freshSubRestrict ^ solution, f.tm))
        case MatchFail =>
         None
      }
   }

   /**
    * the set of facts whose type matches a given type and which are valid at a certain goal
    *
    *  @param goal goal where facts must be valid
    *  @param queryVars free variables in the needed type that are to be matched
    *  @param query the needed type
    *  @return the pairs (sub, t) such that Fact(goal, tm, query ^ sub)
    *
    *  If not all queryVars occur in query, then sub will be partial.
    */
   def termsOfTypeAtGoal(goal: Goal, queryVars: Context, query: Term): List[(Substitution, Term)] = {
      var res: List[(Substitution, Term)] = Nil
      foreachFact(queryVars, query) {f =>
         if (goal below f.goal) {
            matchFact(queryVars, query, f) foreach {x => res ::= x}
         }
      }
      res
   }

   /**
    * the set of terms of the type required by the goal
    */
   def solutionsOfGoal(goal: Goal): List[Term] = {
      var res: List[Term] = Nil
      foreachFact(Nil, goal.conc) {case f =>
         matchFact(Nil, goal.conc, f) foreach {case (_,x) => res ::= x}
      }
      res
   }

   /**
    * the set of facts whose type matches a given type and which are valid at a certain goal or some of its subgoals
    *
    * retrieving facts at subgoals is useful for forward search: facts are only needed at leaf nodes,
    * but computing them as high as possible avoids duplication of facts
    *
    *  @param goal goal where facts must be valid
    *  @param queryVars free variables in the needed type that are to be matched
    *  @param query the needed type
    *  @return the triples (sub, t, g) such that Fact(g, tm, query ^ sub) and (g below goal); g is None if (goal below g)
    *
    *  If not all queryVars occur in query, then sub will be partial.
    */
   def termsOfTypeBelowGoal(goal: Goal, queryVars: Context, query: Term): List[(Substitution, Term, Option[Goal])] = {
      var res: List[(Substitution, Term, Option[Goal])] = Nil
      foreachFact(queryVars, query) {case f =>
         if (goal below f.goal) {
            matchFact(queryVars, query, f) foreach {case (sub,t) => res ::= (sub,t,None)}
         } else if (f.goal below goal) {
            matchFact(queryVars, query, f) foreach {case (sub,t) => res ::= (sub,t,Some(f.goal))}
         }
      }
      res
   }

   /**
    * the proof term (if any) of a fact for the type tp valid at goal g
    */
   def has(g: Goal, tp: Term): Option[Term] = {
      foreachFact(Nil, tp) {f =>
         if ((g below f.goal) && (f.tp hasheq tp)) return Some(f.tm)
      }
      None
   }

   override def toString = {
      facts.toString
   }
}
