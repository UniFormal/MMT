package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import objects._

class ShallowPolymorphism extends InhabitableRule(Pi.path) {
   def apply(solver: Solver)(tp: Term)(implicit stack: Stack, history: History) : Boolean = {
      tp match {
         case FunType(args,u) =>
            val cont = FunType.argsAsContext(args)
            cont.mapVarDecls {case (con, vd) =>
               vd.tp match {
                  case None => false
                  case Some(t) => solver.check(Typing(stack++con, t, OMS(Typed.kind)))
               }
            }.forall(_ == true)
            solver.check(Inhabitable(stack++cont, u))
      }
   }
}