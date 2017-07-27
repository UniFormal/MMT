package info.kwarc.mmt.sequences

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom.FouraryConstantScala
import info.kwarc.mmt.lf._
import info.kwarc.mmt.api.objects.Conversions._


object FoldLeft extends FouraryConstantScala(Sequences._path, "foldLeft") {
  case class Deconstruct(solver : Solver, covered : Boolean)(implicit stack: Stack, history: History) {
    def unapply(tm: Term) : Option[(Term,Term,Term,Term,Term,Term)] = history.indented {
      history += "Trying to deconstruct foldLeft"
      tm match {
        case FoldLeft(f, init,n,seq) =>
          solver.inferType(f, covered).map(t => Common.makePi(solver, t)) match {
            case Some(Pi(x1, tp1, bd1)) if !bd1.freeVars.contains(x1) =>
              Common.makePi(solver, bd1) match {
                case Pi(x2, tp2, tp3) if !tp3.freeVars.contains(x2) =>
                  history += "Deconstructed function type: " + solver.presentObj(Arrow(tp1,Arrow(tp2,tp3)))
                  val itp = solver.inferType(init,covered).getOrElse(return None)
                  solver.check(Typing(stack,init,tp1))
                  solver.check(Typing(stack,init,tp3))
                  solver.check(Subtyping(stack,tp3,tp1))
                  solver.check(Typing(stack,n,OMS(Nat.nat)))
                  solver.check(Typing(stack,seq,Sequences.rep(tp2,n)))
                  Some((init,tp1,tp2,tp3,n,seq))
                case _ =>
                  history += "Apparently not an arrow type: " + solver.presentObj(bd1)
                  None
              }
            case _ =>
              history += "Apparently not a simple function: " + solver.presentObj(f)
              None
          }
        case _ => None
      }
    }
  }
  case class CheckingDeconstruct(solver : CheckingCallback, covered : Boolean)(implicit stack: Stack) {
    def unapply(tm: Term): Option[(Term, Term, Term, Term, Term, Term)] = tm match {
      case FoldLeft(f, init,n,seq) =>
        solver.inferType(f, covered)(stack,NoHistory).map(t => solver.simplify(t)(stack,NoHistory)) match {
          case Some(Pi(x1, tp1, bd1)) if !bd1.freeVars.contains(x1) =>
            solver.simplify(bd1)(stack,NoHistory) match {
              case Pi(x2, tp2, tp3) if !tp3.freeVars.contains(x2) =>
                val itp = solver.inferType(init, covered)(stack,NoHistory).getOrElse(return None)
                Some((init, tp1, tp2, tp3,n,seq))
              case _ =>
                None
            }
          case _ =>
            None
        }
      case _ => None
    }
  }
}



object FoldLeftType extends InferenceRule(FoldLeft.path,OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
    val fold = FoldLeft.Deconstruct(solver,covered)
    tm match {
      case fold(init,tp1,tp2,tp3,n,seq) =>
        Some(tp3)
      case _ => None
    }
  }
}
/*
object FoldLeftCompute extends ComputationRule(FoldLeft.path) {
  def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = {
    tm match {
      case FoldLeft(f, init, n, seq) =>
        ???
      case _ => None
    }
  }
} */