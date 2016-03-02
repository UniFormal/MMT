package info.kwarc.mmt.LFX

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.checking.{History, Solver, UniverseRule}
import info.kwarc.mmt.api.objects.{Stack, Term, OMS}
import info.kwarc.mmt.api.uom.{StandardNat, RealizedType}
import info.kwarc.mmt.api.utils.URI

/**
  * Created by raupi on 08.02.16.
  */
object NatLit {
  val baseURI = DPath(URI.http colon "cds.omdoc.org") / "LFX"
  val thname = "NatLit"
  val path = baseURI ? thname
  def symbol(name : String) = path ? name
}

object NatLiterals extends RealizedType(OMS(NatLit.symbol("NatLit")),StandardNat)
/** the rule that makes type a valid universe */
/*
object NatLiteralsType extends UniverseRule(NatLit.symbol("NatLit")) {
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Boolean = tm == OMS(NatLit.symbol("NatLit"))
}
*/