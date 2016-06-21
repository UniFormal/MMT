package info.kwarc.mmt.lf.inhabitation

import info.kwarc.mmt.lf._

import info.kwarc.mmt.api._
import objects._
import checking._

import Inhabitation._

/**
 * P:!A, Q:!A |- P = Q
 */
object ProofIrrelevance extends TypeBasedEqualityRule(List(Apply.path), Inh.path) {
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
      // applicability and well-formedness are guaranteed by Solver, so we can simply succeed
      Some(true)
   }
}
