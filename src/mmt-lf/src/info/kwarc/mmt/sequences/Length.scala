package info.kwarc.mmt.sequences

import info.kwarc.mmt.api._
import checking._
import objects._

import info.kwarc.mmt.lf._

import TypeSequences._
import LFS._
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
      case ntype(l) => Some(l)
      case Lambda(_,_,_) => ONE
      case Pi(_,_,_) => ONE
      case Apply(_,_) => ONE
      case LFS.ellipsis(m,n,_,_) => Some(succ(minus(n,m)))
      case LFS.flatseq(as @ _*) => Some(natlit(as.length))
      case LFS.index(_,_) => ONE
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

  /** check that two expressions have the same length by calling an equality check
   *  @return false if the lengths could not be compared
   */
  //TODO can we partially solve an unknown if one term has no length?
  def checkEqual(solver: CheckingCallback, tm1: Term, tm2: Term)(implicit stack: Stack, history: History): Boolean = {
    val l1 = Length.infer(solver, tm1) getOrElse {return false}
    val l2 = Length.infer(solver, tm2) getOrElse {return false}
    solver.check(Equality(stack, l1, l2, Some(OMS(nat))))
  }
  
  def checkWithin(solver: CheckingCallback)(low: Term, t: Term, up: Term)(implicit stack: Stack, history: History): Boolean = {
      solver.check(Inhabited(stack, leq(low, t))) &&
      solver.check(Inhabited(stack, leq(t, up))) 
   }
}