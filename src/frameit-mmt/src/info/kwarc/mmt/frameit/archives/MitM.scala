package info.kwarc.mmt.frameit.archives

import info.kwarc.mmt.api.{DPath, GlobalName}
import info.kwarc.mmt.api.objects.OMS
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardBool, StandardDouble, StandardInt, StandardPositive, StandardString}
import info.kwarc.mmt.api.utils.URI

object MitM {
  /**
    * Symbols and literals of the MitM/Foundation archive.
    *
    * Partially copied from https://gl.mathhub.info/MitM/Foundation/-/tree/devel/scala/info/kwarc/mmt/mitm/rules.
    */
  object Foundation {
    val path: DPath = DPath(URI("http", "mathhub.info") / "MitM" / "Foundation")

    private object Math {
      val bool: GlobalName = path ? "Logic" ? "prop"

      val nat: GlobalName = path ? "NatLiterals" ? "nat_lit"
      val int: GlobalName = path ? "IntLiterals" ? "int_lit"
      val pos: GlobalName = path ? "IntLiterals" ? "pos_lit"
      val real: GlobalName = path ? "RealLiterals" ? "real_lit"

      val string: GlobalName = path ? "Strings" ? "string"
    }

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
  }
}