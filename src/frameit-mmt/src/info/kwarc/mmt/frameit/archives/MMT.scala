package info.kwarc.mmt.frameit.archives

import info.kwarc.mmt.api.{DPath, GlobalName}
import info.kwarc.mmt.api.utils.URI

object MMT {
  object LFX {
    private val _path = DPath(URI("http://gl.mathhub.info/MMT/LFX"))
    val tupleSymbol: GlobalName = (_path / "Sigma") ? "Symbols" ? "Tuple"
  }
}
