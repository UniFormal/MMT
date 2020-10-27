package info.kwarc.mmt.frameit.archives

import info.kwarc.mmt.api.{DPath, GlobalName}
import info.kwarc.mmt.api.objects.OMS
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardBool, StandardDouble, StandardInt, StandardPositive, StandardString}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.BinaryLFConstantScala

object MitM {
  /**
    * Symbols and literals of the MitM/Foundation archive.
    *
    * Partially copied from https://gl.mathhub.info/MitM/Foundation/-/tree/devel/scala/info/kwarc/mmt/mitm/rules.
    */
  object Foundation {
    val path: DPath = DPath(URI("http", "mathhub.info") / "MitM" / "Foundation")

    object Math {
      val bool: GlobalName = path ? "Logic" ? "prop"

      val nat: GlobalName = path ? "NatLiterals" ? "nat_lit"
      val int: GlobalName = path ? "IntLiterals" ? "int_lit"
      val pos: GlobalName = path ? "IntLiterals" ? "pos_lit"
      val real: GlobalName = path ? "RealLiterals" ? "real_lit"

      // old from MitM/Foundation@devel branch:
      // val string: GlobalName = path ? "Strings" ? "string"

      // now with MitM/Foundation@urtheories-strings branch (not yet tested and approved by Dennis)
      private[Foundation] val string: GlobalName = MMT.urtheories.path ? "Strings" ? "string"
    }

    object StringConcat extends BinaryLFConstantScala(MMT.urtheories.path ? "Strings", "concat")

    object BooleanLiterals extends RepresentedRealizedType(OMS(Math.bool), StandardBool)

    object StringLiterals extends RepresentedRealizedType(OMS(Math.string), StandardString)

    object RealLiterals extends RepresentedRealizedType(OMS(Math.real), StandardDouble) {
      override def priority: Int = 2
    }

    object IntegerLiterals extends RepresentedRealizedType(OMS(Math.int), StandardInt) {
      override def priority: Int = -1
    }

    object PosLiterals extends RepresentedRealizedType(OMS(Math.pos), StandardPositive) {
      override def priority: Int = 1
    }

    val ded: GlobalName = path ? "Logic" ? "ded"
    val eq: GlobalName = path ? "Logic" ? "eq"
    val sketchOperator: GlobalName = path ? "InformalProofs" ? "proofsketch"
  }
}