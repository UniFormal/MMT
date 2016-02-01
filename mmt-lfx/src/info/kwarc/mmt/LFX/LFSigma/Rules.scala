package info.kwarc.mmt.LFX.LFSigma

import info.kwarc.mmt.api.{objects, LocalName}
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects.OMV
import info.kwarc.mmt.api.objects._
import objects.Conversions._
import info.kwarc.mmt.lf._

object Common {
  /** convenience function for recursively checking the judgement |- a: type */
  def isType(solver: Solver, a: Term)(implicit stack: Stack, history: History) =
    solver.check(Typing(stack, a, OMS(Typed.ktype), Some(OfType.path)))(history + "type of bound variable must be a type")

  /** performs safe simplifications and variable transformation to force the argument to become a Sigma
    * @param solver the Solver
    * @param tp the function type
    * @return a type equal to tp that may have Pi shape
    */
  def makeSigma(solver: Solver, tp: Term)(implicit stack: Stack, history: History): Term = {
    val tpS = solver.safeSimplifyUntil(tp)(Sigma.unapply)._1
    tpS match {
      case Sigma(x,a,b) => tpS
      case ApplyGeneral(OMV(m), args) =>
        // check that tp is unknown applied to variables
        if (! solver.getUnsolvedVariables.isDeclared(m)) {
          return tpS
        }
        args foreach {
          case OMV(u) =>
          case _ => return tpS
        }
        val mD = OMV(m/"d")
        val mC = OMV(m/"c")
        val mV = OMV(m/"v")
        val mSol = Sigma(mV.name, ApplyGeneral(mD, args), ApplyGeneral(mC, args ::: List(mV)))
        // if we have not done the variable transformation before, add the new unknowns
        if (! solver.getPartialSolution.isDeclared(mD.name)) {
          val newVars = Context(VarDecl(mD.name, None, None, None), VarDecl(mC.name, None, None, None))
          solver.addUnknowns(newVars, m)
        }
        history += ("trying to solve "+m+" as "+solver.presentObj(mSol))
        // solve m in terms of newVars
        val success = solver.check(Equality(stack, tpS, mSol, Some(OMS(Typed.ktype)))) //TODO does this work for polymorphism?
        if (success) mSol else tpS
      case _ => tpS
    }
  }
  def pickFresh(solver: Solver, x: LocalName)(implicit stack: Stack) =
    Context.pickFresh(solver.constantContext ++ solver.getPartialSolution ++ stack.context, x)
}

/** Formation: the type inference rule x:A:type|-B:U  --->  Sigma x:A.B(x) : U  * */
object SigmaTerm extends FormationRule(Sigma.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    Common.makeSigma(solver,tm) match {
      case Sigma(x,a,b) =>
        if (!covered) Common.isType(solver,a)
        val (xn,sub) = Common.pickFresh(solver, x)
        solver.inferType(b ^? sub)(stack ++ xn % a, history) flatMap {bT =>
          if (bT.freeVars contains xn) {
            // usually an error, but xn may disappear later, especially when unknown in b are not solved yet
            None
          } else
            Some(bT)
        }
      case _ => None // should be impossible
    }
  }
}

/** Introduction: the type inference rule |-t1:A, x:A|-t2:B(t1)  --->  <t1,t2>:Sigma x:A.B(t1)  * */
object TupleTerm extends IntroductionRule(Tuple.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case Tuple(t1,t2) =>
          val tpA = solver.inferType(t1)(stack, history).get
          val (xn,_) = Common.pickFresh(solver,LocalName("x"))
          val tpB = solver.inferType(t2)(stack, history).get
          Some(Sigma(xn,tpA,tpB))
      case _ => None // should be impossible
    }
  }
}

/** Elimination: the type inference rule t : Sigma x:A.B(x)  --->  Proj1(t):A, Proj2(t):B(Proj1(t)) */
object ProjectionTerm {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case Proj(i, Tuple(a,b)) => solver.inferType(if (i == 1) a else b)(stack, history)
    case Proj(i, t) =>
      history += "inferring type of tuple " + solver.presentObj(t)
      solver.inferType(t)(stack, history) match {
        case None =>
          history += "failed"
          solver.inferType(tm)(stack, history.branch) // inference of the argument may solve some varialbes
          None
        case Some(tp) =>
          history += "tuple type is: " + solver.presentObj(tp)
          val tpSigma = Common.makeSigma(solver, tp)
          if (tpSigma != tp)
            history += "tuple type is: " + solver.presentObj(tpSigma)
          tpSigma match {
            case Sigma(x, a, b) =>
              if (!covered) {
                if (i == 1) Some(a)
                else if (i == 2) Some(b ^? (x / Proj1(t)))
                else None
              } else None
            case _ =>
              history += "does not look like a tuple type at this point"
              None
          }
        case _ => None // should be impossible
      }
  }
}

object Projection1Term extends EliminationRule(Proj1.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] =
    ProjectionTerm.apply(solver)(tm,covered)
}
object Projection2Term extends EliminationRule(Proj2.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] =
    ProjectionTerm.apply(solver)(tm,covered)
}

/** type-checking: the type checking rule pi1(t):A|-pi2(t):B(pi1(t))  --->  t : Sigma x:A.B */

object SigmaType extends TypingRule(Sigma.path) {
  def apply(solver:Solver)(tm:Term, tp:Term)(implicit stack: Stack, history: History) : Boolean = {
    Common.makeSigma(solver,tp) match {
      case Sigma(x,tpA,tpB) =>
        /*
        val tpA2 = solver.inferType(Proj1(tm))
        val tpB2 = solver.inferType(Proj2(tm))(stack,history)
        if (tpA2.isDefined && tpB2.isDefined) solver.check(Equality(stack,tpA,tpA2.get,None)) &&
          solver.check(Equality(stack,tpB ^? (x / Proj1(tm)),tpB2.get,None))
        else false
        */
        solver.check(Typing(stack,Proj1(tm),tpA))
        solver.check(Typing(stack ++ x % tpA,Proj2(tm),tpB ^? (x / Proj1(tm))))
      case _ => false
    }
  }
}

/** Equality rule for sigma and product types */

object SigmaEquality extends TermBasedEqualityRule {
  val head = Sigma.path
  private val heads = List(Some(Sigma.path), Some(Product.path))
  def applicable(tm1: Term, tm2: Term) = heads.contains(tm1.head) && heads.contains(tm2.head)
  def apply(checker: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
    (tm1,tm2) match {
      case (Sigma(x1,a1,t1), Sigma(x2,a2,t2)) =>
        val cont = Continue {
          history += "congruence for Sigma types"
          val res1 = checker.check(Equality(stack,a1,a2,None))(history + "equality of domain types")
          val (xn,_) = Context.pickFresh(stack.context, x1)
          val t1sub = t1 ^? (x1 / OMV(xn))
          val t2sub = t2 ^? (x2 / OMV(xn))
          val res2 = checker.check(Equality(stack ++ xn % a1, t1sub, t2sub, None))(history + "equality of scopes")
          res1 && res2
        }
        Some(cont)
      case _ => None
    }
  }
}

/** equality-checking: the eta rule |- pi1(t1) = pi1(t2) : A , |- pi2(t1) = pi2(t2) : B(pi1(t)) --->  t1 = t2 : Sigma x:A. B */
object TupleEquality extends TypeBasedEqualityRule(Nil, Sigma.path) {
  def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val Sigma(x, tpA, tpB) = Common.makeSigma(solver,tp)
    val match1 = solver.check(Equality(stack, Proj1(tm1),Proj1(tm2),Some(tpA)))
    val (xn,sub) = Common.pickFresh(solver,x)
    Some(match1 && solver.check(Equality(stack ++ xn % tpA,Proj2(tm1),Proj2(tm2),Some(tpB ^? sub))))
  }
}

/** computation: the beta-reduction rule Proj1(<a,b>) = a, Proj2(<a,b>) = b
  */
object ProjectionBeta {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case Proj(i,Tuple(a,b)) => if (i==1) Some(a) else Some(b)
      case _ => None
    }
  }
}

object Projection1Beta extends ComputationRule(Proj1.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term]
    = ProjectionBeta.apply(solver)(tm, covered)
}
object Projection2Beta extends ComputationRule(Proj2.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term]
  = ProjectionBeta.apply(solver)(tm, covered)
}

/** computation: expand product to become sigma type */
object ExpandProduct extends ComputationRule(Product.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
    case Product(a,b) => Some(Sigma(OMV.anonymous, a, b))
    case _ => None
  }
}