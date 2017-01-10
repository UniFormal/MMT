package org.omdoc.latin.foundations.mizar

import info.kwarc.mmt.api._
import objects._
import uom._

import info.kwarc.mmt.lf._

import info.kwarc.mmt.mizar.mmtwrappers._
import Mizar._

object IntroductionRule {
  val metaVarBase = LocalName("")
  def context(n: Int) = List("x","y","z").take(n).map {s => VarDecl(LocalName(metaVarBase / s),None,None,None)}
  val X = OMV(metaVarBase / "x")
  val Y = OMV(metaVarBase / "y")
  
  val boundVar = "BOUND"
  def hasBoundVar(f: Term) = apply(f, OMV(boundVar))
  
  val allRules = List(IntroduceDisjunction,IntroduceImplication,IntroduceEquivalence,IntroduceExistential)
}

import IntroductionRule._

/**
 * not (not X and not Y) ---> X or Y
 */
object IntroduceDisjunction extends RewriteRule(constantName("or"), context(2), not(and(List(not(X),not(Y)))), or(List(X,Y))) with ComplificationRule

/**
 * not (X and not Y) ---> X implies Y
 */
object IntroduceImplication extends RewriteRule(constantName("implies"), context(2), not(and(List(X,not(Y)))), implies(X,Y)) with ComplificationRule {
  /** lower than [[IntroduceDisjunction]] */
  override def priority = -5
}

/**
 * (X implies Y) and (Y implies X) ---> X equiv Y
 */
object IntroduceEquivalence extends RewriteRule(constantName("iff"), context(2), and(List(implies(X,Y),implies(Y,X))), iff(X,Y)) with ComplificationRule {
  /** lower than [[IntroduceImplication]] because it can only fire afterwards anyway */
  override def priority = -10
}

/**
 * not forall x. not P(x) ---> exists x. P(x)
 * 
 * this cannot be a RewriteRule because HOAS matching cannot match for the name of the bound variable
 * so we use basic Scala matching instead
 */
object IntroduceExistential extends TermTransformationRule with ComplificationRule {
   val head = constantName("exists")
   
   // extract the names of the Mizar constants
   private val dummy = OMV("dummy")
   private val Apply(neg, ApplySpine(univ, List(_, Lambda(_, any, _)))) = not(forall(dummy.name.toString, dummy, dummy))
   
   def apply(matcher: Matcher, goalContext: Context, goal: Term): Option[Term] = {
      val (n, tp, scope) = goal match {
        case Apply(this.neg, ApplySpine(this.univ, List(x, Lambda(n, this.any, Apply(this.neg, y))))) => (n, x, y)
        case _ => return None
      }
      Some(exists(n.toString, tp, scope))
   }
}