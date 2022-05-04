package info.kwarc.mmt.frameit.archives

import info.kwarc.mmt.api.{DPath, GlobalName}
import info.kwarc.mmt.api.objects.OMS
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardBool, StandardDouble, StandardInt, StandardNat, StandardPositive, StandardString}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.BinaryLFConstantScala

private[archives] object MitM {
  /**
    * Symbols and literals of the MitM/Foundation archive.
    *
    * Partially copied from https://gl.mathhub.info/MitM/Foundation/-/tree/devel/scala/info/kwarc/mmt/mitm/rules.
    */
  object Foundation {
    val path: DPath = DPath(URI("http", "mathhub.info") / "MitM" / "Foundation")

    object Math {
      val pos: GlobalName = path ? "NatLiterals" ? "pos_lit"
      val nat: GlobalName = path ? "NatLiterals" ? "nat_lit"
      val int: GlobalName = path ? "IntLiterals" ? "int_lit"

      val real: GlobalName = path ? "RealLiterals" ? "real_lit"
      val prop: GlobalName = path ? "Logic" ? "prop"
    }

    object NatLiterals extends RepresentedRealizedType(OMS(Math.nat), StandardNat)
    object PosLiterals extends RepresentedRealizedType(OMS(Math.pos), StandardPositive)
    object IntegerLiterals extends RepresentedRealizedType(OMS(Math.int), StandardInt)

    object RealLiterals extends RepresentedRealizedType(OMS(Math.real), StandardDouble)

    val eq: GlobalName = path ? "Logic" ? "eq"
    val ded: GlobalName = path ? "Logic" ? "ded"
    val sketchOperator: GlobalName = path ? "InformalProofs" ? "proofsketch"
  }
}