package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import objects._
import frontend._
import symbols._
import objects.Conversions._
import utils._

import scala.collection.mutable.{HashMap}

case class Fact(tm: Term, tp: Term)

class Facts(parent: Option[Facts], newContext: Context) {
   private val data  = new HashMapToSet[Term,Term]
   private val facts = new HashMap[Term,Term]
   
   def init {
      newContext foreach {case VarDecl(n, Some(tp), _,_) =>
         facts(tp) = OMV(n)
      }
   }
   init
   
   def hasLocal(tp: Term) = facts.isDefinedAt(tp)
   def has(tp: Term): Boolean = hasLocal(tp) || parent.map(_.has(tp)).getOrElse(true)
}

class Alternative(val subgoals: List[Goal]) {
   def isComplete: Boolean = subgoals.forall(_.isSolved)
}

class Goal(parent: Option[Goal], val context: Context, val tp: Term) {
   private var expansionIntros : List[ApplicableIntro] = Nil
   private var expansionElims  : List[ApplicableElim]  = Nil
   
   val facts: Facts = new Facts(parent.map(_.facts), context)
   
   private var alternatives: List[Alternative] = Nil
   def addAlternative(alt: Alternative) {
      alternatives ::= alt
      solved = None
   }
   def getAlternatives = alternatives
   
   /** caches result of isSolved if true */
   private var solved: Option[Boolean] = None
   /** recursively checks if the goal can be closed (result is cached if true) */
   def isSolved: Boolean = {
      if (solved.isDefined) return solved.get
      val s = alternatives.find(_.isComplete) match {
         case Some(a) =>
            alternatives = List(a)
            true
         case None =>
            if (facts.has(tp)) {
               alternatives = Nil
               true
            } else {
               false
            }
      }
      solved = Some(s)
      s
   }
   
   def getNextExpansion: Option[ApplicableTactic] = {
      (expansionIntros, expansionElims) match {
         case (ai::rest, _) =>
            expansionIntros = rest
            Some(ai)
         case (_, ae::rest) =>
            expansionElims = rest
            Some(ae)
         case _ => None
      }
   }
   
   def applySearchElims(tactics: List[ElimTactic]) {
      
   }
   
   def setApplicableTactics(intros: List[IntroTactic], elims: List[ElimTactic]) {
      expansionIntros = parent match {
         case Some(g) if g.tp == tp => g.expansionIntros
         case _ => intros.flatMap {t => t.applicable(tp)}
      }
         
      expansionElims = parent.map(_.expansionElims).getOrElse(Nil)
      elims.foreach {t =>
         t.applicable(context) foreach {at =>
            expansionElims ::= at
         }
      }
   }
}

abstract class Tactic extends Rule {
   def priority: Int
   def isInvertible: Boolean
   def apply(prover: P, g: Goal): Option[Alternative]
}

abstract class IntroTactic extends Tactic {
   def applicable(tp: Term): List[ApplicableIntro]
}

abstract class ElimTactic extends Tactic {
   def applicable(newContext: Context): List[ApplicableElim]
   def generate(facts: Facts)
}

abstract class ApplicableTactic {
   def tactic : Tactic 
}
case class ApplicableElim(tactic: ElimTactic, fact: Term) extends ApplicableTactic
case class ApplicableIntro(tactic: IntroTactic) extends ApplicableTactic

class P(goal: Goal, intros: List[IntroTactic], elims: List[ElimTactic]) {
   def apply(levels: Int): Boolean = {
      if (goal.isSolved) return true
      expand(goal)
      if (goal.isSolved) return true
      search(goal, levels)
      goal.isSolved
   }
   /**
    * applies one tactic to a goal, resulting subgoals are immediately expanded
    * @return true if the tactic made progress
    */
   private def applyAndExpand(at: ApplicableTactic, g: Goal): Boolean = {
      val altOpt = at.tactic.apply(this, g)
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
      g.setApplicableTactics(intros, elims)
      g.getNextExpansion foreach {at =>
         // apply the next invertible tactic, if any
         val applicable = applyAndExpand(at, g)
         if (! applicable)
           // at not applicable, try next tactic
           expand(g)
      }
   }
   /** apply any one tactic and expand the subgoals */
   private def search(g: Goal, levels: Int) {
      if (g.isSolved || levels == 0) return
      elims.foreach {e =>
         e.generate(g.facts)
         if (g.isSolved) return
      }
      if (g.isSolved) return
      intros.foreach {t =>
         t.applicable(g.tp) foreach {at =>
            applyAndExpand(at, g)
            if (g.isSolved) return
         }
      }
      g.getAlternatives.foreach {
         case alt: Alternative =>
            alt.subgoals.foreach {ng => search(ng,levels)}
            if (g.isSolved) return
      }
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
