package info.kwarc.mmt.lf.hollight

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.refactoring.ParameterPreprocessor
import info.kwarc.mmt.lf.LFClassicHOLPreprocessor
import utils._

object HOLLight {
   val _base = DPath(URI("http", "latin.omdoc.org") / "foundations" / "hollight")
   val logic = _base ? "Kernel"
   val foundation = _base ? "HOL"

   val apply = logic ? "Comb"
   val lambda = logic ? "Abs"
   val oftype = logic ? "term"
   val hoas = notations.HOAS(apply, lambda, oftype)
/* TODO
   val mitmpreproc = ParameterPreprocessor + new LFClassicHOLPreprocessor(
      ded = MitM.ded,
      and = MitM.and,
      not = MitM.not,
      or = Some(MitM.or),
      implies = Some(MitM.implies),
      equiv = Some(MitM.equiv),
      forall = Some(MitM.forall),
      exists = Some(MitM.exists)
   ) */
}
