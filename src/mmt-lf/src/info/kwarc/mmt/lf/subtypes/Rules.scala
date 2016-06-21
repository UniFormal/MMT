package info.kwarc.mmt.lf.subtypes

import info.kwarc.mmt._
import api._
import checking._
import proving._
import objects._

import lf._
import inhabitation.Inhabitation._
import Subtyping.Sub

class ProveSubtypingRule extends SubtypingRule {
   val head = Sub.path
   def applicable(tp1: Term, tp2: Term): Boolean = true
   /**
    * pre all arguments covered
    */
   def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean] = {
      val p = solver.prove(Inh(Sub(tp1,tp2)))
      if (p.isDefined) Some(true)
      else None
   }
}
