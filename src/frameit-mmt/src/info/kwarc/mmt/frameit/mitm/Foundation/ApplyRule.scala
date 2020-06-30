package com.example.mitm.Foundation

import info.kwarc.mmt.api.checking.{History, Solver, TypingRule}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.{Apply, ApplySpine, Pi}
import info.kwarc.mmt.api.objects.Conversions._

@deprecated("this looks weird; it should be explained or removed", "")
object ApplyRule extends TypingRule(Apply.path) {
  override def applicable(t: Term): Boolean = true

  override def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = tm match {
    case ApplySpine(f,args) =>
      var debug = (solver.checkingUnit.component.toString contains "realVec2") && (tp.toString contains "realVec")
      val tpF = solver.inferType(f).getOrElse(return None) // solver.checkingUnit.component.toString contains "realVec2"
      val ret = check(tpF,args,solver,debug)
      ret.flatMap(t => solver.tryToCheckWithoutDelay(Equality(stack,t,tp,None)) match {
        case r@Some(true) =>
          r
        case _ =>
          None
      })
    case _ => None
  }

  def check(pi : Term, args : List[Term],solver : Solver, debug : Boolean)(implicit stack: Stack, history: History): Option[Term] = solver.safeSimplifyUntil(pi)(Pi.unapply)._1 match {
    case Pi(x,a,b) =>
      solver.check(Typing(stack,args.head,a))
      if (args.length == 1) Some(b ^? x/args.head)
      else check(b ^? x/args.head,args.tail,solver,debug)
    case _ =>
      None
  }
}
