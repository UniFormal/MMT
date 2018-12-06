package info.kwarc.mmt.lf.coercions

import info.kwarc.mmt.api._
import objects._
import checking._
import objects.Conversions._

import info.kwarc.mmt.lf._

/**
 * a type coercion rule lifts a non-type A to the type lift(A)
 * if applicable(A) and A occurs where a type is expected
 * 
 * This rule does not do anything by itself.
 * Instead, it provides multiple rules that insert the lifting function during type-checking, see the respective members
 */
abstract class TypeCoercionRule(val head: GlobalName, under: List[GlobalName]) extends CheckingRule {self =>
  private object Applicable extends ApplicableUnder {
    val head = self.head
    def under = self.under
  }
  def lift(tm: Term): Term
  /* |- t:A     <--  |- t: lift(A) */
  object OnLeftSide extends TypingRule(Typed.ktype) {
     def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) = {
       if (Applicable.applicable(tm)) {
         val r = solver.check(Typing(stack, lift(tm), tp))(history + "lifting left side")
         Some(r)
       } else
         None
     }
  }
  /** |- A:type  <--  |- lift(A):type */
  object OnRightSide extends TypingRule(head) {
     def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) = {
       val r = solver.check(Typing(stack, tm, lift(tp)))(history + "lifting right side")
       Some(r)
     }
  }
  
  /** |- t=t':A  <--  |- t=t':lift(A):type */
  object WithEquality extends TypeBasedEqualityRule(under, head) {
    def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History) = { 
      val r = solver.check(Equality(stack, tm1, tm2, Some(lift(tp))))(history + "lifting type")
      Some(r)
    }
    def applicableToTerm(tm: Term) = true
  }
  
  /** |- A Inh  <--  |- lift(A) Inh */
  object WithInhabitable extends InhabitableRule(head) {
    def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History) = {
      val r = solver.check(Inhabitable(stack, lift(term)))(history + "lifting")
      Some(r)
    }
  }
  
  override def providedRules = super.providedRules ::: List(OnLeftSide, OnRightSide, WithEquality, WithInhabitable)
}

/** the typical case where lift(A) = Apply(operator, A) */  
class LFTypeCoercionRule(h: GlobalName, u: List[GlobalName], operator: GlobalName) extends TypeCoercionRule(h,u) {
  def lift(tm: Term) = Apply(OMS(operator), tm)
}