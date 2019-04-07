package info.kwarc.mmt.lf.coercions

import info.kwarc.mmt.api._
import objects._
import checking._
import info.kwarc.mmt.api.utils.MMT_TODO
import objects.Conversions._
import info.kwarc.mmt.lf._

/**
 * a type coercion rule lifts a non-type A to the type lift(A)
 * if applicable(A) and A occurs where a type is expected
 * 
 * This rule does not do anything by itself.
 * Instead, it provides multiple rules that insert the lifting function during type-checking, see the respective members
 */
@MMT_TODO("this is a nice idea, but it doesn't quite work; it's not used yet")
abstract class TypeCoercionRule(val head: GlobalName, under: List[GlobalName]) extends CheckingRule {self =>
  /** auxiliary object for checking applicability */
  private object Applicable extends ApplicableUnder {
    val head = self.head
    def under = self.under
    def apply(tm: Term) = applicable(tm)
  }
  /** the lifting function
   *  pre: Applicable(tm)
   *  post: |- lift(tm): type
   */
  def lift(tm: Term): Term
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
}

/** the typical case where lift(A) = Apply(operator, A) */  
class LFTypeCoercionRule(h: GlobalName, u: List[GlobalName], operator: GlobalName) extends TypeCoercionRule(h,u) {
  def lift(tm: Term) = Apply(OMS(operator), tm)
}