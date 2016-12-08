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
   *  fails only for variables without type, e.g., unknowns representing types
   *  assumes that expressions cannot be simplified into ellipsis or ntype
   */
  def infer(solver: CheckingCallback, tm: Term)(implicit stack: Stack) : Option[Term] = {
    tm match {
      case Univ(_) => ONE
      case Sequences.ntype(l) => Some(l)
      case Lambda(_,_,_) => ONE
      case Pi(_,_,_) => ONE
      case Apply(_,_) => ONE
      case Sequences.ellipsis(n,_,_) => Some(n)
      case Sequences.flatseq(as @ _*) => Some(natlit(as.length))
      case Sequences.index(_,_) => ONE
      case OMS(p) => ONE
      case OMV(x) => (solver.outerContext++stack.context)(x).tp match {
        case Some(t) => infer(solver,t)
        case None => None
      }
      case _: OMLITTrait => ONE
      case OMA(_,_) => ONE
      case OMBINDC(_,_,_) => ONE
    }
  }

  /** check |tm1| = |tm2| for two sequences by calling an equality check on the length
   *  @return the result, if a check was possible
   */
  //TODO can we partially solve an unknown if one term has no length?
  def checkEqual(solver: CheckingCallback, tm1: Term, tm2: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val l1 = Length.infer(solver, tm1) getOrElse {return None}
    val l2 = Length.infer(solver, tm2) getOrElse {return None}
    val r = solver.check(Equality(stack, l1, l2, Some(OMS(nat))))
    Some(r)
  }
  
  /** check t <= up for natural numbers */
  def checkBelow(solver: CheckingCallback)(t: Term, up: Term)(implicit stack: Stack, history: History): Boolean = {
      solver.check(Inhabited(stack, leq(t, up))) 
  }
}