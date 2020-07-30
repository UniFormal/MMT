package info.kwarc.mmt.frameit.archives

import info.kwarc.mmt.api.{DPath, GlobalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.api.utils.URI

object FrameIT {

  /**
    * Symbols and paths of the FrameIT/FrameWorld archive:
    * https://gl.mathhub.info/FrameIT/frameworld
    */
  object FrameWorld {
    object MetaKeys {
      private val _scrollMeta: MPath = FrameWorld.rootDocument ? "ScrollMeta"

      val scrollName: GlobalName = _scrollMeta ? "name"
      val problemTheory: GlobalName = _scrollMeta ? "problemTheory"
      val solutionTheory: GlobalName = _scrollMeta ? "solutionTheory"
      val scrollDescription: GlobalName = _scrollMeta ? "description"

      // TODO not yet realized in formalization
      val factLabel: GlobalName = _scrollMeta ? "factLabel"
    }

    val archiveID: String = "FrameIT/frameworld"
    val rootDocument: DPath = DPath(URI("http://mathhub.info/FrameIT/frameworld"))
    val FactCollection: MPath = Path.parseM("http://mathhub.info/FrameIT/frameworld?FactCollection", NamespaceMap.empty)
  }
}
