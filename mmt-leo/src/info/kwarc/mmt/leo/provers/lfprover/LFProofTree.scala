package info.kwarc.mmt.leo.provers.lfprover

import info.kwarc.mmt.api.checking.Hole
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.proving.{ForwardInvertible, BackwardInvertible, BackwardSearch, ApplicableTactic}
import info.kwarc.mmt.leo.datastructures._

/**
 * Created by Mark on 7/15/2015.
 *
 * This class represents the proof tree for the LF prover
 */

class LFProofTree(val contextVar: Context, var concVar: Term, conjVar: Boolean, isSatVar: Option[Boolean] = None )
  extends AndOr[LFProofTree] {

  var conj = conjVar
  var isSat = isSatVar

  /**The context held by the node*/
  var context = contextVar
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


  /** the conclusion held by the node*/
  var conc = concVar
  /** sets a new goal, can be used by the prover to simplify goals in place */
  def setConc(newConc: Term)= {concVar = newConc}


  /** stores the proof */
  private var proofVar: Term = Hole(conc)
  /** stores the proof, contains holes if the goal is not solved */
  def proof = proofVar


  /*/**
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
    removeAlternatives
  }

  /** checks whether this can be closed using the axiom rule, i.e., whether the goal is in the database of facts */
  private def checkAxiomRule(implicit facts: Facts) {
    if (solved != Some(true)) {
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
  def setExpansionTactics(prover: Searcher, backw: List[BackwardInvertible], forw: List[ForwardInvertible]) {
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

  def setSearchTactics(prover: Searcher, backw: List[BackwardSearch]) {
    backwardSearch = backw
  }
  def getNextSearch(prover: Searcher): List[ApplicableTactic] = {
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
    if (isSolved) {
      goalHighlight + "! " + presentObj(context) + " |- " + presentObj(proof) + " : " + presentObj(conc)
    } else {
      val aS = alternatives.map(a => Searcher.indent(depth+1) + altHighlight(a) + a.present(depth+1))
      val lines = goalHighlight + (presentObj(context) + " |- _  : " + presentObj(conc)) :: aS
      lines.mkString("\n")
    }
  }

*/

}

