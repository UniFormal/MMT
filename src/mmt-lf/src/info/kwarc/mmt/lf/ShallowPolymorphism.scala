package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import objects._
import objects.Conversions._

object ShallowPolymorphism extends InhabitableRule(Pi.path) {
   def apply(solver: Solver)(tp: Term)(implicit stack: Stack, history: History) : Boolean = {
      tp match {
         case Pi(x,a,b) =>
            solver.inferTypeAndThen(a)(stack, history + "toplevel argument must be typed by universe") {u =>
               solver.check(Universe(stack, u))
            } &&
            solver.check(Inhabitable(stack++x%a, b))
      }
   }
}