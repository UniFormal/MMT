package info.kwarc.mmt.frameit.archives

import info.kwarc.mmt.api.{DPath, GlobalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.api.utils.URI

object FrameIT {

  /**
    * Symbols and paths of the FrameIT/frameworld archive:
    * https://gl.mathhub.info/FrameIT/frameworld
    */
  object FrameWorld {
    object MetaKeys {
      private val _metaAnnotations: MPath = FrameWorld.rootDocument ? "ScrollMetaAnnotations"

      val scrollName: GlobalName = _metaAnnotations ? "name"
      val problemTheory: GlobalName = _metaAnnotations ? "problemTheory"
      val solutionTheory: GlobalName = _metaAnnotations ? "solutionTheory"
      val scrollDescription: GlobalName = _metaAnnotations ? "description"

      // TODO not yet realized in formalization
      val factLabel: GlobalName = _metaAnnotations ? "factLabel"
    }

    val archiveID: String = "FrameIT/frameworld"
    val rootDocument: DPath = DPath(URI("http://mathhub.info/FrameIT/frameworld"))
    val metaTheoryForSituationTheory: MPath = rootDocument ? "FrameworldMeta"

    val situationTheoryForDebugging: MPath = (rootDocument / "integrationtests") ? "SampleSituationTheory"
  }
}
