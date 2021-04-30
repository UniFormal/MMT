package info.kwarc.mmt.frameit.archives

import info.kwarc.mmt.api.objects.OMS
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardString}
import info.kwarc.mmt.api.{DPath, GlobalName}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.frameit.archives.MitM.Foundation.Math
import info.kwarc.mmt.lf.BinaryLFConstantScala

// private to force all symbols to be exposed from FrameIT.FrameWorld
private[archives] object MMT {
  object LFX {
    val path: DPath = DPath(URI("http://gl.mathhub.info/MMT/LFX"))
  }

  object urtheories {
    val path: DPath = DPath(URI("http", "cds.omdoc.org") / "urtheories")

    val string: GlobalName = path ? "Strings" ? "string"
    val ded: GlobalName = path ? "Ded" ? "DED"

    object StringLiterals extends RepresentedRealizedType(OMS(string), StandardString)
    object StringConcat extends BinaryLFConstantScala(MMT.urtheories.path ? "Strings", "concat")
  }
}
