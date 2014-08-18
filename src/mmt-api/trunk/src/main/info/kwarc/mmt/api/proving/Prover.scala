package info.kwarc.mmt.api.proving

import info.kwarc.mmt.api._
import checking._
import objects._
import frontend._
import symbols._
import objects.Conversions._
import utils._

//import scala.collection.mutable.{HashMap}

/**
 * A database of facts obtained through forward proof search
 * 
 * For efficiency, each instance only searches for terms that are added when the context is enriched.
 * Therefore, each [[Goal]] g maintains one instance of Facts, which links to the instance of the g.parent.
 * Each instance knows the local context of its goal, and maintains only terms that use a local variable.
 * 
 * @param parent g.parent.facts
 * @param newContext g.context
 */
class Facts(parent: Option[Facts], val newContext: Context) extends Iterable[Term] {
   /** the database: maps every type to a set of terms of that type */
   private val facts = new HashMapToSet[Term,Term]
   
   /**
    * initializes the database by adding the context to the facts, called in class initializer
    */
   private def initFacts {
      newContext foreach {case VarDecl(n, Some(tp), _,_) =>
         add(OMV(n), tp)
      }
   }
   initFacts

   /** the facts added in the previous iteration */
   private var newFacts : List[(Term,Term)] = Nil
   /** the facts added in the current iteration */
   private var futureFacts : List[(Term,Term)] = Nil
   /** 
    *  adds a fact to the database
    *  @param tm a term that is valid over the full context of this goal
    *  @param tp its type
    *  
    *  the facts are not actually added immediately but queued for addition
    *  see integrateNewFacts 
    */
   def add(tm: Term, tp: Term) {
      futureFacts ::= (tm,tp)
   }
   /**
    * adds all queued facts to the database 
    */
   def integrateFutureFacts {
      futureFacts foreach {case (tm,tp) => facts(tp) += tm}
      newFacts = futureFacts
      futureFacts = Nil
   }
   /** all facts added in the previous iteration */
   def getNewFacts = newFacts
   /** an iterator over all types inhabited at this context (including those of the parent goal) */
   def iterator: Iterator[Term] = facts.keys.iterator ++ parent.map(_.iterator).getOrElse(Nil)
   /** the set of terms of a given type */
   def termsOfType(tp: Term): List[Term] = facts(tp).toList
   
   def hasLocal(tp: Term) = facts.isDefinedAt(tp)
   def has(tp: Term): Boolean = hasLocal(tp) || parent.map(_.has(tp)).getOrElse(true)
}

/**
 * Each [[Goal]] stores a list of alternatives each of which stores a list of subgoals
 * 
 * A goal can be closed if all subgoals of one alternative can be closed
 * 
 * @param subgoals the conjuncts of this alternative 
 */
case class Alternative(subgoals: List[Goal]) {
   /** true if all subgoals are solved */
   def isSolved: Boolean = subgoals.forall(_.isSolved)
}

/**
 * a single-conclusion sequent - the basic node in a proof tree
 * 
 * The root goal is stored and the whole proof tree is acted on by the [[Prover]].  
 * 
 * A goal nodes knows its parent (except for the root goal) and children (the subgoals).
 * In fact, a goal is also a node in the backwards proof search: A goal stores not simply a list of subgoals,
 * but a list of [[Alternative]] ways to prove it each of which stores a list of subgoals.
 * 
 * Moreover, a goal stores the explored part of a the forward proof search space:
 * a set of facts implied by the goals premises, updated externally.
 * 
 * The prover expands new goals greedily by applying invertible rules,
 * and each goal stores those invertible rules that have not been applied yet.
 * 
 * @param context the premises added to the sequent by this goal;
 *                the full antecedent arises by prepending the one of the parent goal
 * @param tp the conclusion of the sequent     
 */

class Goal(val context: Context, val conc: Term) {
   /** the parent node, None only for the root */
   private var parent: Option[Goal] = None

   /** the database of facts found by forward search space from this goal's premises */
   lazy val facts: Facts = new Facts(parent.map(_.facts), context)
   def integrateFutureFacts {
      facts.integrateFutureFacts
      solved = None
   }
   
   /** stores the list of alternatives */
   private var alternatives: List[Alternative] = Nil
   /** adds a new alternative in the backward search space */
   def addAlternative(alt: Alternative) {
      alt.subgoals.foreach {g => g.parent = Some(this)}
      alternatives ::= alt
      solved = None
   }
   /**
    * the list of explored directions in the backward search space that can prove the goal
    */
   def getAlternatives = alternatives
   
   /** caches the result of isSolved */
   private var solved: Option[Boolean] = None
   /** checks if the goal can be closed
    *  - by applying the axiom rule or
    *  - by closing all subgoals of some alternative
    *  
    *  the result is cached so that the method can be called arbitrarily often without performance penalty
    */
   def isSolved: Boolean = {
      if (solved.isDefined) return solved.get
      val s = alternatives.find(_.isSolved) match {
         case Some(a) =>
            alternatives = List(a)
            true
         case None =>
            if (facts.has(conc)) {
               alternatives = Nil
               true
            } else {
               false
            }
      }
      solved = Some(s)
      s
   }
   
   /** stores the invertible backward rules that have not been applied yet */
   private var backward : List[ApplicableTactic] = Nil
   /** stores the invertible forward rules that have not been applied yet */
   private var forward  : List[ApplicableTactic]  = Nil
   /** true if no more invertible rules are left */
   def isExpanded = backward.isEmpty && forward.isEmpty
   /** 
    *  the invertible backward/forward tactics that have not been applied yet are stored here,
    *  but set and read by the [[Prover]]
    *  this method retrieves the next tactic to apply
    */
   def getNextExpansion: Option[ApplicableTactic] = {
      (backward, forward) match {
         case (ai::rest, _) =>
            backward = rest
            Some(ai)
         case (_, ae::rest) =>
            forward = rest
            Some(ae)
         case _ => None
      }
   }
   /** initializes the invertible backward/forward tactics that can be applied */
   def setExpansionTactics(prover: P, backw: List[BackwardInvertible], forw: List[ForwardInvertible]) {
      backward = parent match {
         case Some(g) if g.conc == conc => g.backward
         // TODO it can be redundant to check the applicability of all tactics
         case _ => backw.flatMap {t => t.apply(prover, conc)} 
      }
      val applForw = forw.flatMap {t => t.apply(prover, context)}
      forward = applForw ::: parent.map(_.forward).getOrElse(Nil)
   }
}

/**
 * A prover conducts the proof search. A new instance is created for each proof obligation.
 * 
 * @param goal the goal to prove
 * @param intros the backward tactics to use
 * @param elims  the forward  tactics to use
 * 
 * A prover greedily applies invertible tactics to each new goal (called the expansion phase).
 * Then forward and backward breadth-first searches are performed in parallel.
 */
class P(goal: Goal, rules: RuleSet) {
   private val invertibleBackward = rules.get(classOf[BackwardInvertible]).toList
   private val invertibleForward  = rules.get(classOf[ForwardInvertible]).toList
   private val searchBackward     = rules.get(classOf[BackwardSearch]).toList.sortBy(_.priority).reverse
   private val searchForward      = rules.get(classOf[ForwardSearch]).toList
   /**
    * tries to solve the goal
    * @param levels the depth of the breadth-first searches
    * @return true if the goal was solved
    */
   def apply(levels: Int): Boolean = {
      if (goal.isSolved) return true
      expand(goal)
      if (goal.isSolved) return true
      search(goal, levels)
      goal.isSolved
   }
   /**
    * applies one tactic to a goal and expands the resulting subgoals
    * @return true if the tactic made any progress
    */
   private def applyAndExpand(at: ApplicableTactic, g: Goal): Boolean = {
      val altOpt = at.apply()
      altOpt foreach {alt =>
         g.addAlternative(alt)
         if (!g.isSolved) {
            // recursively process subgoals
            alt.subgoals.foreach {sg => expand(sg)}
         }
      }
      altOpt.isDefined
   }
   /** exhaustively applies invertible tactics to a goal */
   private def expand(g: Goal) {
      g.setExpansionTactics(this, invertibleBackward, invertibleForward)
      g.getNextExpansion foreach {at =>
         // apply the next invertible tactic, if any
         val applicable = applyAndExpand(at, g)
         if (! applicable)
           // at not applicable, try next tactic
           expand(g)
      }
   }
   /**
    * applies forward search to all goals in the tree and backward search to all fully expanded goals
    * @param g the goal to apply tactics to
    * @param levels the depth of the breadth-first searches 
    *  
    */
   private def search(g: Goal, levels: Int) {
      if (g.isSolved || levels == 0) return
      g.integrateFutureFacts
      // forward search at g
      searchForward.foreach {e =>
         e.generate(g.facts)
         if (g.isSolved) return
      }
      // recurse into subgoals
      g.getAlternatives.foreach {case Alternative(sgs) =>
            sgs.foreach {ng => search(ng,levels)}
            if (g.isSolved) return
      }
      if (g.isSolved) return
      // backward search at g if g is the end product of an expansion phase
      // new goals are expanded immediately and subject to forward/backward search in the next iteration
      if (g.isExpanded) {
         searchBackward.foreach {t =>
            t.apply(this, g) foreach {at =>
               applyAndExpand(at, g)
               if (g.isSolved) return
            }
         }
      }
      // repeat
      search(g, levels-1)
   }   
}

class Prover(controller: Controller) {
   
   def getIntroRules(goal: Term)(implicit stack: Stack,rules: RuleSet) : List[ApplicableProvingRule] = {
      rules.get(classOf[IntroProvingRule]).filter(_.applicable(goal)).flatMap {r => r(goal).toList}.toList
   }
   
   def applicable(goal: Term)(implicit stack: Stack,rules: RuleSet) : List[ApplicableProvingRule] = {
      // first look for all intro rules, if any return them
      val possibleIntro = getIntroRules(goal)
      if (possibleIntro != Nil) return possibleIntro

      // if none, look for applicable elim rules by inspecting the current theory and context
      // axioms holds the list of applicable axiom rules, which are found along the way
      var axioms : List[ApplicableProvingRule] = Nil
      val elimProvingRules = rules.get(classOf[ElimProvingRule]).toList
      def doType(src: Term, tpOpt: Option[Term]) : List[ApplicableProvingRule] = {
            val tp = tpOpt.getOrElse(return Nil)
            if (tp == goal) axioms ::= axiomRule(src) 
            val tpH = tp.head.getOrElse(return Nil)
            elimProvingRules.filter(_.head == tpH).toList flatMap {r => r(src, tp, goal).toList}
      }
      val possibleElim = stack.context flatMap {
         case IncludeVarDecl(p,_) =>
            val decls = controller.localLookup.getDeclaredTheory(p).getConstants
            decls flatMap {c => doType(c.toTerm, c.tp)}
         case vd => doType(vd.toTerm, vd.tp)
      }
      axioms.reverse ::: possibleElim
   }
   
   private def axiomRule(ax: Term) = new ApplicableProvingRule {
      def label = ax.toString
      def apply() = ax
   }
}
