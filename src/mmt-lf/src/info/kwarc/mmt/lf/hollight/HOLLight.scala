package info.kwarc.mmt.lf.hollight

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.refactoring.{DefinitionExpander, ParameterPreprocessor}
import info.kwarc.mmt.lf.{LFClassicHOLPreprocessor, LFHOASElim, ViewFinderHOAS}
import utils._

object HOLLight {
   val _base = DPath(URI("http", "latin.omdoc.org") / "foundations" / "hollight")
   val logic = _base ? "Kernel"
   val foundation = _base ? "HOL"

   val apply = logic ? "Comb"
   val lambda = logic ? "Abs"
   val oftype = logic ? "term"
   val hoas = notations.HOAS(apply, lambda, oftype)

   val tp = logic ? "holtype"
   val tm = logic ? "term"
   val arrow = logic ? "fun"

   val vfhoas = ViewFinderHOAS(tp,tm,lambda,apply,arrow)

   val boolth = (DPath(URI("http://github.com")) / "jrh13" / "hol-light") ? "bool"

   val ded = logic ? "thm"
   val and = boolth ? "/\\"
   val not = boolth ? "~"
   val implies = boolth ? "==>"
   val forall = boolth ? "!"
   val exists = boolth ? "?"
   val or = boolth ? "\\/"

   val preproc = (DefinitionExpander + LFHOASElim(vfhoas) /* + LFClassicHOLPreprocessor(
      ded, and, not, Some(forall), Some(or),Some(implies),None,Some(exists)
   )*/ ).withKey("HOLLight")
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
