package info.kwarc.mmt.mizar.newxml.foundations

import info.kwarc.mmt._
import api._
import objects._
import uom._
import sequences._

import info.kwarc.mmt.mizar.newxml.mmtwrapper._
import MizarPrimitiveConcepts._

object IntroductionRule {
  val metaVarBase = LocalName("")
  def context(n: Int) = List("x","y","z").take(n).map {s => VarDecl(LocalName(metaVarBase / s))}
  val X = OMV(metaVarBase / "x")
  val Y = OMV(metaVarBase / "y")

  val boundVar = "BOUND"
  def hasBoundVar(f: Term) = apply(f, OMV(boundVar))

  val allRules = List(IntroduceImplication,IntroduceEquivalence,IntroduceExistential,ContractRep)
}

import IntroductionRule._

/**
 * (X implies Y) and (Y implies X) ---> X equiv Y
 */
object IntroduceEquivalence extends RewriteRule(constantName("iff"), context(2), And(List(implies(X,Y),implies(Y,X))), iff(X,Y)) with ComplificationRule {
  /** lower than [[IntroduceImplication]] because it can only fire afterwards anyway */
  override def priority = -10
}

/**
 * not and(X1,...,Xm, not Y1,...,not Yn) ---> and X implies or Y
 * not and (not Y1,..., not Yn) ---> or Y
 *
 * this cannot easily be a rewrite rule because m and n can be arbitrary
 */
object IntroduceImplication extends TermTransformationRule with ComplificationRule {
  import lf._
  val head = constantName("implies")
  def apply(matcher: Matcher, goalContext: Context, goal: Term) = {
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
          None
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
          Some(res)
        }
      case _ => None
    }
  }
}

/**
 * not forall x. not P(x) ---> exists x. P(x)
 *
 * this cannot be a RewriteRule because HOAS matching cannot match for the name of the bound variable
 * so we use basic Scala matching instead
 */
object IntroduceExistential extends TermTransformationRule with ComplificationRule {
  val head = constantName("exists")
  import lf._
  // extract the names of the Mizar constants
  private val dummy = OMV("dummy")
  private val Apply(neg, ApplySpine(univ, List(_, Lambda(_, any, _)))) = not(forall(dummy.name.toString, dummy, dummy))

  def apply(matcher: Matcher, goalContext: Context, goal: Term): Option[Term] = {
    goal match {
      case not(forall(n, x, not(y))) => Some(exists(n, x, y))
      case _ => None
    }
  }
}

