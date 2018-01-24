package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import checking._
import objects._
import objects.Conversions._

object ShallowPolymorphism extends InhabitableRule(Pi.path) with PiOrArrowRule {
   def apply(solver: Solver)(tp: Term)(implicit stack: Stack, history: History) : Boolean = {
      tp match {
         case Pi(x,a,b) =>
            val historyArg = history + (x.toString + " must be typed by universe")
            solver.inferTypeAndThen(a)(stack, historyArg + "infer type") {u =>
               solver.check(Universe(stack, u))(historyArg + "check universe")
            } &&
            solver.check(Inhabitable(stack++x%a, b))
      }
   }
}
