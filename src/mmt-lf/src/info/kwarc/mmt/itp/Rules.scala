package info.kwarc.mmt.itp

import info.kwarc.mmt.api.{checking, _}
import checking._
import uom._
import objects._
import objects.Conversions._
import info.kwarc.mmt.lf._

import ITP._

/** */
object ProofTerm extends InferenceRule(ITP.proof.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
     val proof(args) = tm
     None
   }
}
