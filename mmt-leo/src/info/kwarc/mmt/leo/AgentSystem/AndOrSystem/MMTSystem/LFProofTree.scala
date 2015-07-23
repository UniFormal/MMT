package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.MMTSystem

import info.kwarc.mmt.api.checking.Hole
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.AndOr
/*

/**
 * Created by Mark on 7/15/2015.
 *
 * This class represents the proof tree for the LF prover
 */
//TODO Ask Florian: Should I further specialize to ensure that And nodes always follow or nodes and vice versa
class LFProofTree(val contextVar: Context, var concVar: Term, conjVar: Boolean, isSatVar: Option[Boolean] = None )
  extends AndOr[LFProofTree] {

  var conj = conjVar
  var sat = isSatVar

  var finished = false
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



  /** caches the result of isSolved */
  private var solved: Option[Boolean] = None

  /**
   * checks if the goal can be closed by closing all subgoals of some alternative
   * the result is cached so that the method can be called arbitrarily often without performance penalty
   */
  override def isSolved: Boolean = {
    if (solved.isDefined) return solved.get
    solved = Some(false)
    if (this.isOr) {
      children.find(_.isSolved) foreach {a=>setSolved(a.proof())}
      //TODO Ask Florian if i need to set solved for the current node
    }else if (children.forall(_.isSolved)){
      children foreach {a=>setSolved(a.proof())}
    }
    solved.getOrElse(false)
  }

  /**
   * recursively abandons all alternatives
   * (all goals are marked so that existing pointers to them (e.g., in facts) can be abandoned)
   */
  private def closeSubgoals() {
    finished = true
    children.foreach {_.closeSubgoals()}
  }

  /** sets the proof of this goal and removes alternatives */
  private def setSolved(p: Term) {
    proofVar = p
    solved = Some(true)
    closeSubgoals()
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
    children.foreach {_.newFacts}
  }

  /** stores the invertible backward rules that have not been applied yet */
  private var backward : List[ApplicableTactic] = Nil
  /** stores the invertible forward rules that have not been applied yet */
  private var forward  : List[ApplicableTactic]  = Nil
  /** stores the backward search rules that have not been applied yet */
  private var backwardSearch : List[BackwardSearch] = Nil
  /** initializes the invertible backward/forward tactics that can be applied */
  def setExpansionTactics(blackboard: LFBlackboard, backw: List[BackwardInvertible], forw: List[ForwardInvertible]) {
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
   *  but set and read by the [[LFBlackboard]]
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

  def setSearchTactics(blackboard: LFBlackboard, backw: List[BackwardSearch]) {
    backwardSearch = backw
  }
  def getNextSearch(blackboard: LFBlackboard): List[ApplicableTactic] = {
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

  private def indent(depth: Int) = (0 to depth).map(_ => "  ").mkString("")

  def present(depth: Int, newAlt: Option[LFProofTree])(implicit presentObj: Obj => String, current: Option[LFProofTree]): String = {
    val goalHighlight = if (current.contains(this)) "X " else "  "
    def altHighlight(a: LFProofTree) = if (newAlt.contains(a)) "+ new\n" else "+ \n"
    if (isSolved) {
      goalHighlight + "! " + presentObj(context) + " |- " + presentObj(proof) + " : " + presentObj(conc)
    } else {
      val aS = this.children.map(a => a.indent(depth+1) + altHighlight(a) + a.present(depth+1,newAlt))
      val lines = goalHighlight + (presentObj(context) + " |- _  : " + presentObj(conc)) :: aS
      lines.mkString("\n")
    }
  }


}

*/
