package info.kwarc.mmt.mizar.foundations

import info.kwarc.mmt._
import api._
import objects._
import uom._
import sequences._

import info.kwarc.mmt.mizar.mmtwrapper._
import MizarPrimitiveConcepts._

object IntroductionRule {

  val boundVar = "BOUND"
  def hasBoundVar(f: Term) = apply(f, OMV(boundVar))

  val allRules = List(IntroduceImplication,IntroduceEquivalence,IntroduceExistential,ContractRep)
}

/**
 * (X implies Y) and (Y implies X) ---> X equiv Y
 */
object IntroduceEquivalence extends SimplificationRule(constantName("iff")) {
  def apply(c: Context, t: Term) = {
    t match {
      case And(List(implies(x,y),implies(y2,x2))) if y == y2 && x == x2 => Simplify(iff(x,y))
      case _ => Recurse
    }
  }
  /** lower than [[IntroduceImplication]] because it can only fire afterwards anyway */
  override def priority = -10

  override def complificationOnly = true
}

/**
 * not and(X1,...,Xm, not Y1,...,not Yn) ---> and X implies or Y
 * not and (not Y1,..., not Yn) ---> or Y
 *
 * this cannot easily be a rewrite rule because m and n can be arbitrary
 */
object IntroduceImplication extends SimplificationRule(constantName("implies")) {
  import lf._
  def apply(goalContext: Context, goal: Term): Simplifiability = {
    goal match {
      case Apply(OMS(negCon), ApplySpine(OMS(andCon), NoSeqs(args))) =>
        var succ: List[Term] = Nil
        args.reverse.find {
          case not(a) =>
            succ ::= a
            false
          case forall(n,t,not(a)) =>
            // forall with negated body often stems from negated existential
            succ ::= exists(n.name.toString,t,a)
            false
          case _ => true
        }
        if (succ.isEmpty)
        // n = 0, nothing to do
          Recurse
        else {
          val disj = if (succ.length == 1) succ.head else Or(succ)
          val anteL = args.length-succ.length
          val res = if (anteL == 0)
          // m = 0, return disjunction
            disj
          else {
            // default case: return implication
            val conj = if (anteL == 1) args.head else And(args.take(anteL))
            implies(conj, disj)
          }
          Simplify(res)
        }
      case _ => Recurse
    }
  }
  override def complificationOnly = true
}

/**
 * not forall x. not P(x) ---> exists x. P(x)
 *
 * this cannot be a RewriteRule because HOAS matching cannot match for the name of the bound variable
 * so we use basic Scala matching instead
 */
object IntroduceExistential extends SimplificationRule(constantName("exists"))  {
  import lf._
  // extract the names of the Mizar constants
  private val dummy = OMV("dummy")
  private val Apply(neg, ApplySpine(univ, List(_, Lambda(_, any, _)))) = not(forall(dummy.name.toString, dummy, dummy))

  def apply(goalContext: Context, goal: Term): Simplifiability = {
    goal match {
      case not(forall(n, x, not(y))) => Simplify(exists(n, x, y))
      case _ => Recurse
    }
  }
  override def complificationOnly = true
}

