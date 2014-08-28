package info.kwarc.mmt.api.proving

import info.kwarc.mmt.api._
import checking._
import objects._
import objects.Conversions._

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
      val gS = subgoals.map {g => Prover.indent(depth) + g.present(depth+1)}
      gS.mkString("\n")
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

class Goal(val context: Context, val conc: Term) {
   /** the parent node, None only for the root */
   private var parent: Option[Goal] = None

   def path: List[Goal] = this :: parent.map(_.path).getOrElse(Nil)
   def below(that: Goal): Boolean = this == that || parent.map(_ below that).getOrElse(false)

   lazy val fullContext: Context = parent.map(_.fullContext).getOrElse(Context()) ++ context
   lazy val varAtoms: List[(Term,Term)] = parent.map(_.varAtoms).getOrElse(Nil) ::: context.flatMap {
      case IncludeVarDecl(_,_) => Nil
      case StructureVarDecl(_,_,_) => Nil
      case VarDecl(n,Some(t),_,_) => List((OMV(n), t))
      case VarDecl(_, None,_,_) => Nil
   }
   
   /** stores the list of alternatives */
   private var alternatives: List[Alternative] = Nil
   /** adds a new alternative in the backward search space */
   def addAlternative(alt: Alternative) {
      alt.subgoals.foreach {g =>
         g.parent = Some(this)
      }
      alternatives ::= alt
      solved = None
   }
   /**
    * the list of explored directions in the backward search space that can prove the goal
    */
   def getAlternatives = alternatives
   
   /** caches the result of isSolved */
   private var solved: Option[Boolean] = None
   /** stores the proof once the goal is solved */
   private var proofOption: Option[Term] = None
   /** the proof term for this goal; pre: isSolved == Some(true) */
   def proof = proofOption.get
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
   /**
    * recursively checks if the goal can be closed by using the axiom rule
    * 
    * should be called iff there are new facts available (result is cached by isSolved) 
    */
   def newFacts(facts: FactsDB) {
      if (solved != Some(true)) {
         solved = None
         facts.has(this, conc) foreach {p =>
            setSolved(p)
         }
      }
      alternatives.foreach {a =>
         a.subgoals.foreach {sg => sg.newFacts(facts)}
      }
   }
   /** sets the proof of this goal and finishes up */
   private def setSolved(p: Term) {
      proofOption = Some(p)
      solved = Some(true)
      setFinished
   }
   
   /** stores the finishedn status */
   private var finished = false
   /** 
    *  true if no further proving should be performed at this goal
    *  pointers to it should be abandoned as soon as the proof term is collected
    */
   private[proving] def isFinished = finished
   /** recursively abandons all alternatives
    *  (has to be recursive so that all goals are finished up so that facts can be removed)
    */
   private def setFinished {
   }
   
   /** stores the invertible backward rules that have not been applied yet */
   private var backward : List[ApplicableTactic] = Nil
   /** stores the invertible forward rules that have not been applied yet */
   private var forward  : List[ApplicableTactic]  = Nil
   /** stores the backward search rules that have not been applied yet */
   private var backwardSearch : List[BackwardSearch] = Nil
   /** initializes the invertible backward/forward tactics that can be applied */
   def setExpansionTactics(prover: P, backw: List[BackwardInvertible], forw: List[ForwardInvertible]) {
      backward = parent match {
         case Some(g) if g.conc == conc => g.backward
         // TODO it can be redundant to check the applicability of all tactics
         case _ => backw.flatMap {t => t.apply(prover, this)} 
      }
      val applForw = forw.flatMap {t => t.apply(prover, context)}
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

   def setSearchTactics(prover: P, backw: List[BackwardSearch]) {
      backwardSearch = backw
   }
   def getNextSearch(prover: P): List[ApplicableTactic] = {
      backwardSearch match {
         case Nil => Nil
         case hd::tl =>
            backwardSearch = tl
            hd.apply(prover, this) match {
               case Nil => getNextSearch(prover)
               case l => l
            }
      }
   }
   
   override def toString = conc.toString
   def present(depth: Int)(implicit presentObj: Obj => String, current: Option[Goal], newAlt: Option[Alternative]): String = {
      val goalHighlight = if (Some(this) == current) "X " else "  "
      def altHighlight(a: Alternative) = if (Some(a) == newAlt) "+ new\n" else "+ \n"
      proofOption match {
         case Some(p) => goalHighlight + "! " + presentObj(context) + " |- " + presentObj(p) + " : " + presentObj(conc)
         case None =>
            val aS = alternatives.map(a => Prover.indent(depth+1) + altHighlight(a) + a.present(depth+1))
            val lines = goalHighlight + (presentObj(context) + " |- _  : " + presentObj(conc)) :: aS
            lines.mkString("\n")
      }
   }
}
