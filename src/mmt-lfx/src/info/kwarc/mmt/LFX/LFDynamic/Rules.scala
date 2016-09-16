package info.kwarc.mmt.LFX.LFDynamic

import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.OfType

/**
  * Created by raupi on 01.12.15.
  */

object VarKindRule extends FormationRule(VarType.path,OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case VarType(tp) => solver.inferType(tp,covered)
    case _ => None
  }
}

object VarSubtypeRule extends SubtypingRule {
  val head = VarType.path

  def applicable(tp1: Term, tp2: Term): Boolean = tp1 match {
    case VarType(t) => true
    case _ => false
  }

  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean] =
    tp1 match {
      case VarType(t) => Some(solver.check(Subtyping(stack,t,tp2)))
      case _ => throw Backtrack()
    }
}

object VarTypeRule extends TypingRule(VarType.path) {
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) : Boolean = (tm,tp) match {
    case (OMV(x),VarType(alpha)) =>
      history += "Trying Typing rule for Var"
      val v = stack.context.find(w => w.name == x).getOrElse(return false)
      solver.check(Typing(stack,v.toTerm,alpha))
    case (_,VarType(alpha)) =>
      val v = stack.context.find(w => w.df.isDefined &&
        solver.safecheck(Equality(stack,w.df.get,tm,None)).getOrElse(false)).getOrElse(return false)
      solver.check(Typing(stack,v.toTerm,alpha))
    case _ => false
  }
}

object AssTermRule extends ComputationRule(Assign.path) {
  def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case Assign(v,t,e) =>
      val x = stack.context.find(w => w.name == v.name).getOrElse(return None)
      val tpx = x.tp.getOrElse(return None)
      val t2 = if (x.df.isDefined) {
        val ts = check.simplify(t)
        val (nvar,sub) = Context.pickFresh(stack.context,v.name)
        if (ts == (ts ^? sub)) ts else t
      } else t
      check.check(Typing(stack,t2,VarType(tpx)))
      Some(e ^? (v/t2))
    case _ => None
  }
}