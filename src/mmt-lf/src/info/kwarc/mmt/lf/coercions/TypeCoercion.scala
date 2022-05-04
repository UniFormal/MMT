package info.kwarc.mmt.lf.coercions

import info.kwarc.mmt.api._
import objects._
import checking._
import info.kwarc.mmt.lf._

/** the typical case where lift(A) = Apply(operator, A) */  
class LFTypeCoercionRule(h: GlobalName, u: List[GlobalName], operator: GlobalName) extends TypeCoercionRule(h,u) {
  def apply(tp: Term, tm: Term) = Some(Apply(OMS(operator), tm))
}

/** the typical case
  * set: type
  * in: set --> set --> prop
  * Elem: set --> type
  * elem: {A: set}{a:set} ded a in A --> Elem A
  */
/** UNFINISHED
class LiftedSoftType(set: GlobalName, ded: GlobalName, in: GlobalName, Elem: GlobalName, elem: GlobalName, val under: List[GlobalName]) extends TypingRule(Elem) with ApplicableUnder {
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) = {
     solver.inferTypeAndThen(tm, true)(stack, history) {tmI =>
       if (tmI == OMS(set)) {
         val
         solver.addUnknowns(???, None)
         solver.check(Typing())

       } else {
         throw TypingRule.SwitchToInference
       }
     }
   }
}
*/

/* old attempt, never used

  /** auxiliary object for checking applicability */
  private object Applicable extends ApplicableUnder {
    val head = self.head
    def under = self.under
    def apply(tm: Term) = applicable(tm)
  }

  /** convenience for rules with multiple terms where only some have to be lifted */
  protected def liftIfApplicable(tm: Term) = if (Applicable(tm)) (lift(tm),true) else (tm,false)

  /* |- t:A     <--  |- t: lift(A) */
  object AsTermInTyping extends TypingRule(Typed.ktype) {
     def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) = {
       if (Applicable(tm)) {
         val r = solver.check(Typing(stack, lift(tm), tp))(history + "lifting left side")
         Some(r)
       } else
         None
     }
  }

  /** |- A:type  <--  |- lift(A):type */
  object AsTypeInTyping extends TypingRule(head) {
     def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) = {
       val r = solver.check(Typing(stack, tm, lift(tp)))(history + "lifting right side")
       Some(r)
     }
  }
  
  /** |- A=A':type  <--  |- lift(A)=lift(A'):type   (if A or A' liftable) */
  object AsTermInEquality extends TypeBasedEqualityRule(Nil, Typed.ktype) {
    def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History) = {
      val (tm1L, lifted1) = liftIfApplicable(tm1)
      val (tm2L, lifted2) = liftIfApplicable(tm2)
      if (lifted1 || lifted2) {
        val r = solver.check(Equality(stack, tm1L, tm2L, Some(tp)))(history + "lifting term(s)")
        Some(r)
      } else {
        // impossible due to applicableToTerm
        None
      }
    }
    def applicableToTerm(tm: Term) = Applicable(tm)
  }

  /** |- t=t':A  <--  |- t=t':lift(A):type */
  object AsTypeInEquality extends TypeBasedEqualityRule(under, head) {
    def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History) = { 
      val r = solver.check(Equality(stack, tm1, tm2, Some(lift(tp))))(history + "lifting type")
      Some(r)
    }
    def applicableToTerm(tm: Term) = true
  }
  
  /** |- A <: A'  <--  |- lift(A) <: lift(A')   (if A or A' liftable) */
  object InSubtyping extends SubtypingRule {
   val head = self.head
   def applicable(tp1: Term, tp2: Term) = Applicable(tp1) || Applicable(tp2)
   def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) = {
      val (tp1L, lifted1) = liftIfApplicable(tp1)
      val (tp2L, lifted2) = liftIfApplicable(tp2)
      if (lifted1 || lifted2) {
        val r = solver.check(Subtyping(stack, tp1L, tp2L))(history + "lifting term(s)")
        Some(r)
      } else {
        // impossible due to method applicable
        None
      }
    }
  }

  /** |- A Inh  <--  |- lift(A) Inh */
  object InInhabitable extends InhabitableRule(head) {
    def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History) = {
      val r = solver.check(Inhabitable(stack, lift(term)))(history + "lifting")
      Some(r)
    }
  }
  
  override def providedRules = super.providedRules ::: List(AsTermInTyping, AsTypeInTyping, AsTermInEquality, AsTypeInEquality, InSubtyping, InInhabitable)
 
*/
