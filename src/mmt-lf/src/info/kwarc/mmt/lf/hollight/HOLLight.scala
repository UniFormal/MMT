package info.kwarc.mmt.lf.hollight

import info.kwarc.mmt.api._
import utils._

object HOLLight {
   val _base = DPath(URI("http", "latin.omdoc.org") / "foundations" / "hollight")
   val logic = _base ? "Kernel"
   val foundation = _base ? "HOL"
   
   val apply = logic ? "Comb"
   val lambda = logic ? "Abs"
   val oftype = logic ? "term"
   val hoas = notations.HOAS(apply, lambda, oftype)
}