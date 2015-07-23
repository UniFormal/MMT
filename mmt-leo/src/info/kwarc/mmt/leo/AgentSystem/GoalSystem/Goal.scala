package info.kwarc.mmt.leo.AgentSystem.GoalSystem

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.HTML
/*

/**
 * Each [[Goal]] stores a list of alternatives each of which stores a list of subgoals
 * 
 * A goal can be closed if all subgoals of one alternative can be closed
 * 
 * @param subgoals the conjuncts of this alternative 
 * @param proof the proof term
 * 
 * Owners may call `proof()` only if `isSolved == true`.
 * When creating instances, it is safe to call [[Goal#proof]] to compute the proof term.
 */
case class Alternative(subgoals: List[Goal], proof: () => Term) {
   /** true if all subgoals are solved */
   def isSolved: Boolean = subgoals.forall(_.isSolved)
   def present(depth: Int)(implicit presentObj: Obj => String, current: Option[Goal], newAlt: Option[Alternative]) = {
      val gS = subgoals.map {g => Indenter.indent(depth) + g.present(depth+1)}
      gS.mkString("\n")
   }

   def presentHtml(depth: Int)(implicit presentObj: Obj => String, current: Option[Goal], newAlt: Option[Alternative]) = {
      val gS = subgoals.map {g => Indenter.indent(depth) + g.presentHtml(depth+1, firstTime=false)}
      HTML.build { h => import h._
         gS.foreach { l =>
            div("prover-alternative") {
               literal(l)
            }
         }
      }
   }

   override def toString = subgoals.mkString("\n")
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

class Goal(val context: Context, private var concVar: Term) {
   /** the parent node, None only for the root */
   private[leo] var parent: Option[Goal] = None

   def path: List[Goal] = this :: parent.map(_.path).getOrElse(Nil)
   def below(that: Goal): Boolean = this == that || parent.exists(_ below that)

   /** getter for the conclusion (may have been simplified since Goal creation) */
   def conc = concVar
   /** sets a new goal, can be used by the prover to simplify goals in place */
   private[leo] def setConc(newConc: Term)(implicit facts: Facts) {
      concVar = newConc
      checkAxiomRule
   }
   /** the complete context/antecedent (i.e., including the parent's context) of this sequent */
   lazy val fullContext: Context = parent.map(_.fullContext).getOrElse(Context()) ++ context
   /** the local context of this goal seen as a list of atomic facts that rules can make use of */
   lazy val varAtoms: List[Atom] = context.flatMap {
      case IncludeVarDecl(_,_) => Nil
      case StructureVarDecl(_,_,_) => Nil
      case VarDecl(n,Some(t),_,_) => List(Atom(OMV(n), t, None))
      case VarDecl(_, None,_,_) => Nil
   }
   /** the complete context of this goal seen as a list of atomic facts that rules can make use of */
   lazy val fullVarAtoms: List[Atom] = parent.map(_.fullVarAtoms).getOrElse(Nil) ::: varAtoms
   
   /** stores the list of alternatives */
   private var alternatives: List[Alternative] = Nil
   /** adds a new alternative in the backward search space */
   private[leo] def addAlternative(alt: Alternative) {
      alt.subgoals.foreach {sg =>
         sg.parent = Some(this)
      }
      alternatives ::= alt
      solved = None
   }

   /**
    * the list of explored directions in the backward search space that can prove the goal
    */
   def getAlternatives = alternatives

   /** stores the finishedness status */
   private var finished = false
   /**
    *  true if no further proving should be performed at this goal
    *  pointers to it should be abandoned as soon as the proof term is collected
    */
   private[leo] def isFinished = finished
   /**
    * recursively abandons all alternatives
    * (all goals are marked so that existing pointers to them (e.g., in facts) can be abandoned)
    */
   //TODO Ask how existing pointers to them in facts are handled
   private def removeAlternatives() {
      finished = true
      alternatives.foreach {a => a.subgoals.foreach {sg => sg.removeAlternatives()}}
      alternatives = Nil
   }
   
   /** caches the result of isSolved */
   private var solved: Option[Boolean] = None
   /** stores the proof */
   private var proofVar: Term = Hole(conc)
   /** stores the proof, contains holes if the goal is not solved */
   def proof = proofVar
   /**
    * checks if the goal can be closed by closing all subgoals of some alternative
    * the result is cached so that the method can be called arbitrarily often without performance penalty
    */
   def isSolved: Boolean = {
      if (solved.isDefined) return solved.get
      solved = Some(false)
      alternatives.find(_.isSolved) foreach {a =>
         setSolved(a.proof())
      }
      solved.getOrElse(false)
   }
   /** sets the proof of this goal and removes alternatives */
   private def setSolved(p: Term) {
      proofVar = p
      solved = Some(true)
      removeAlternatives()
   }
   
   /** checks whether this can be closed using the axiom rule, i.e., whether the goal is in the database of facts */
   private def checkAxiomRule(implicit facts: Facts) {
      if (!solved.contains(true)) {
         solved = None
         facts.has(this, conc) foreach {p =>
            setSolved(p)
         }
      }
   }

   /**
    * recursively checks if the goal can be closed by using the axiom rule
    * 
    * should be called iff there are new facts available (result is cached by isSolved) 
    */
   def newFacts(implicit facts: Facts) {
      checkAxiomRule
      alternatives.foreach {a =>
         a.subgoals.foreach {sg => sg.newFacts}
      }
   }
   
   /** stores the invertible backward rules that have not been applied yet */
   private var backward : List[ApplicableTactic] = Nil
   /** stores the invertible forward rules that have not been applied yet */
   private var forward  : List[ApplicableTactic]  = Nil
   /** stores the backward search rules that have not been applied yet */
   private var backwardSearch : List[BackwardSearch] = Nil
   /** initializes the invertible backward/forward tactics that can be applied */
   def setExpansionTactics(blackboard: GoalBlackboard, backw: List[BackwardInvertible], forw: List[ForwardInvertible]) {
      backward = parent match {
         case Some(g) if g.conc == conc => g.backward
         // TODO it can be redundant to check the applicability of all tactics
         case _ => backw.flatMap {t => t.apply(blackboard, this)}
      }
      val applForw = forw.flatMap {t => t.apply(blackboard, context)}
      forward = applForw ::: parent.map(_.forward).getOrElse(Nil)
   }
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

   def setSearchTactics(blackboard: GoalBlackboard, backw: List[BackwardSearch]) {
      backwardSearch = backw
   }
   def getNextSearch(blackboard: GoalBlackboard): List[ApplicableTactic] = {
      backwardSearch match {
         case Nil => Nil
         case hd::tl =>
            backwardSearch = tl
            hd.apply(blackboard, this) match {
               case Nil => getNextSearch(blackboard)
               case l => l
            }
      }
   }
   
   override def toString = conc.toString
   def present(depth: Int)(implicit presentObj: Obj => String, current: Option[Goal], newAlt: Option[Alternative]): String = {
      val goalHighlight = if (current.contains(this)) "X " else "  "
      def altHighlight(a: Alternative) = if (newAlt.contains(a)) "+ new\n" else "+ \n"
      if (isSolved) {
         goalHighlight + "! " + presentObj(context) + " |- " + presentObj(proof) + " : " + presentObj(conc)
      } else {
         val aS = alternatives.map(a => Indenter.indent(depth+1) + altHighlight(a) + a.present(depth+1))
         val lines = goalHighlight + (presentObj(context) + " |- _  : " + presentObj(conc)) :: aS
         lines.mkString("\n")
      }
   }

   def presentHtml(depth: Int, firstTime:Boolean = true)(implicit presentObj: Obj => String, current: Option[Goal], newAlt: Option[Alternative]): String = {

      def addHtmlDiv(s: String, cl: String)() = {
         HTML.build { h => import h._
            div(cl) {
               literal(s)
            }
         }
      }

      val goalHighlight = if (current.contains(this)) {
         addHtmlDiv("X ", "prover-X")
      } else {
         "  "
      }

      def altHighlight(a: Alternative) = if (newAlt.contains(a)) "+ new\n" else "+ \n"
      if (isSolved) {
         addHtmlDiv(goalHighlight + "! " + presentObj(context) + " |- " + presentObj(proof) + " : " + presentObj(conc),"prover-solved")
      } else {
         val aS = alternatives.map(a => Indenter.indent(depth + 1) + altHighlight(a) + a.presentHtml(depth + 1))
         val lines = goalHighlight + (presentObj(context) + " |- _  : " + presentObj(conc)) :: aS

         if (firstTime) {
            HTML.build { h => import h._
               html {
                  body {
                     lines.foreach { l =>
                        div("prover-goal") {
                           literal(l)
                        }
                     }
                  }
               }
            }
         } else {
            lines.map({ l => addHtmlDiv(l, "prover-goal") }).mkString("")

         }
      }
   }

}
*/
