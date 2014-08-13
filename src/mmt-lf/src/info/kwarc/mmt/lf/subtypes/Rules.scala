package info.kwarc.mmt.lf.subtypes

import info.kwarc.mmt._
import api._
import checking._
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
      val g = new Goal(None, stack.context, Inh(Sub(tp1,tp2)))
      val r = solver.prove(g, 3)
      if (r) Some(true)
      else None
   }
}
