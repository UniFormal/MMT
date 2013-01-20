package info.kwarc.mmt.lfs

import info.kwarc.mmt.api._
import objects._

object InferConcat extends InferenceRule(LFS.seq) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = {
      //tm is of the form OMA(OMID(LFS.seq), args)
      tm match {
         case OMA(OMID(LFS.seq), args) =>
            val tps = args.map(a => solver.inferType(a))
            if (tps.forall(_.isDefined)) {
               Some(OMA(OMID(LFS.seq), tps.map(_.get)))
            } else
               None
         case  _ => None // impossible
      }
   }
}

/*
// trivial, naive
object Oracle extends EqualityRule(Nat.nat) {
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack) : Boolean = {
      val n1: Int = normalize(tm1)
      val n2: Int = normalize(tm2)
      n1 == n2
   }
      
}
*/