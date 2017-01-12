package info.kwarc.mmt.sequences

import info.kwarc.mmt.api._
import checking._
import objects._

import info.kwarc.mmt.lf._

import Sequences._
import Nat._

/** auxiliary functions for length-based operations on sequences */
object Length {
  private val ONE = Some(OMS(one))
  /** fast (no recursion) computation of the length of an expression
   *  
   *  assumes that expressions cannot be simplified into ellipsis or ntype
   *
   *  currently succeeds for all terms by assuming that unknown terms always have length 1
   *  unknowns representing sequences are desirable but their unknown length would block type inference
   */
  def infer(context: Context, tm: Term) : Option[Term] = {
    tm match {
      case Univ(_) => ONE
      case Lambda(_,_,_) => ONE
      case Pi(_,_,_) => ONE
      case Apply(_,_) => ONE
      case Sequences.rep(_,n) => Some(n)
      case Sequences.ellipsis(n,_,_) => Some(n)
      case Sequences.flatseq(as @ _*) => Some(NatRules.NatLit(as.length))
      case Sequences.index(_,_) => ONE
      case OMS(p) => ONE
      case OMV(x) =>
        val vd = context(x)
        vd.tp.flatMap {t => infer(context,t)} orElse vd.df.flatMap {t => infer(context,t)} orElse ONE
      case _: OMLITTrait => ONE
      case OMA(_,_) => ONE
      case OMBINDC(_,_,_) => ONE
    }
  }
  /** convenience */
  def infer(solver: CheckingCallback, tm: Term)(implicit stack: Stack): Option[Term] = infer(solver.outerContext++stack.context, tm)
  
  /** true if length is known to be 1 */
  def isIndividual(context: Context, t: Term) = infer(context, t) == ONE

  /** check |tm1| = |tm2| for two sequences by calling an equality check on the length
   *  @return the result, if a check was possible
   */
  //TODO can we partially solve an unknown if one term has no length?
  def checkEqual(solver: CheckingCallback, tm1: Term, tm2: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val l1 = Length.infer(solver, tm1) getOrElse {return None}
    val l2 = Length.infer(solver, tm2) getOrElse {return None}
    val r = solver.check(Equality(stack, l1, l2, Some(OMS(nat))))(history.branch)
    Some(r)
  }
  
  /** check t <= up for natural numbers */
  def checkBelow(solver: CheckingCallback)(t: Term, up: Term)(implicit stack: Stack, history: History): Boolean = {
      solver.check(Inhabited(stack, lessType(t, up)))(history.branch)
  }
}