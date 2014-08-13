package info.kwarc.mmt.sequences

import info.kwarc.mmt.api._
import checking._
import objects._
import objects.Conversions._

import info.kwarc.mmt.lf._

import LFS._

/** |- type ^ n UNIVERSE */
object UniverseNType extends UniverseRule(ntype.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Boolean = tm match {
      case LFS.ntype(n) =>
         solver.check(Typing(stack, n, OMS(LFS.nat))) //TODO already covered by precondition or universe rules?
   }
}

/** |- type ^ n : kind */
object NTypeTerm extends InferenceRule(ntype.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case OMA(OMS(ntype), List(n)) =>
         solver.check(Typing(stack, n, OMS(nat)))
         Some(OMS(Typed.kind))
      case _ => None
   }
}

/** |- nat INHABITABLE */
object NatInhabitable extends InhabitableRule(nat) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Boolean = tm match {
      case OMS(LFS.nat) => true
   }
}

/** 
 *  i, i/low, i/up |- t_i : a_i : type --->  |- [t_i] i=m ^ n : [a_i] i=m ^ n
 *  i, i/low, i/up |- t_i : type       --->  |- [t_i] i=m ^ n : type ^ (n-m)
 */
object EllipsisInfer extends InferenceRule(ellipsis.path, OfType.path) {
   private val low = "low"
   private val up  = "up"
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case LFS.ellipsis(m, n, i, t) =>
         val stackI = stack ++ i%OMS(nat)
         val stackILU = stackI ++ i/low%leq(m, OMV(i)) ++ i/up%leq(OMV(i), m)
         val aOpt = solver.inferType(t)(stackILU , history)
         aOpt flatMap {a =>
            if (a == OMS(Typed.ktype))
               Some(ntype(n))
            else {
               val ok = if (covered) true else {
                  val vars = !a.freeVars.exists(x => x == i/low || x == i/up)
                  vars && solver.check(Typing(stackI, a, OMS(Typed.ktype)))
               }
               if (ok)
                  Some(ellipsis(m,n,i,a))
               else
                  // TODO error message?
                  None
            }
         }
      case _ => None
   }
}


class NatSolver(stack: Stack) {
   private var lows: List[(Term,LocalName)] = Nil
   private var ups : List[(LocalName,Term)] = Nil
   private def add(l: Term, u: Term) {(l,u) match {
      case (succ(x), succ(y)) => add(x,y)
      case (OMV(x),OMV(y)) =>
         lows ::= (l, y)
         ups  ::= (x, u)
      case (l, OMV(y)) =>
         lows ::= (l, y)
      case (OMV(x),u) =>
         ups  ::= (x, u)
      case _ =>
   }}
   private def init {
      stack.context.foreach {
         case VarDecl(_, Some(LFS.leq(l,u)), _ ,_) => add(l,u)
         case _ =>
      }
   }
   init

   //TODO check for cycles
   def apply(x: Term, y: Term): Option[Boolean] = (x,y) match {
      case (x,y) if x == y => Some(true)
      case (succ(m), succ(n)) => apply(m,n)
      case (zero,_) => Some(true)
      case (succ(_), zero) => Some(false)
      case (OMV(vx),_) =>  if (useUpper(vx,y)) Some(true) else None
      case (_, OMV(vy)) => if (useLower(x, vy)) Some(true) else None
      case (OMV(vx),OMV(vy)) => if (useUpper(vx,y) || useLower(x, vy)) Some(true) else None
      case _ => None
   }
   private def useUpper(vx: LocalName, y: Term) = ups .exists {case (v,up)  => v == vx && apply(up,y) == Some(true)}
   private def useLower(x: Term, vy: LocalName) = lows.exists {case (low,v) => v == vy && apply(x,low) == Some(true)}
}

object IndexInfer extends InferenceRule(index.path, OfType.path) {
   private def checkWithin(solver: Solver)(low: Term, t: Term, up: Term)(implicit stack: Stack, history: History): Boolean = {
      val ns = new NatSolver(stack)
      ns.apply(low, t) == Some(true) && ns.apply(t, up) == Some(true)
   }
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case LFS.index(s, at) =>
         val sTOpt = solver.inferType(s)
         sTOpt flatMap {
            case LFS.ntype(n) =>
               if (covered || checkWithin(solver)(OMS(one), at, n))
                  Some(OMS(Typed.ktype))
               else
                  None
            case LFS.ellipsis(m,n,i,a) =>
               if (covered || checkWithin(solver)(OMS(one), at, n))
                  Some(a ^? (i -> at))
               else
                  None
            case _ => None // TODO error message?
         }
      case _ => None
   }
}

object ApplyTerm extends InferenceRule(Apply.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case _ => None
   }
}