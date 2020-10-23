package info.kwarc.mmt.frameit.archives

import info.kwarc.mmt.api.{DPath, GlobalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.api.utils.URI

object FrameIT {

  /**
    * Symbols and paths of the FrameIT/frameworld archive:
    * https://gl.mathhub.info/FrameIT/frameworld
    */
  object FrameWorld {
    val archiveID: String = "FrameIT/frameworld"
    val rootDocument: DPath = DPath(URI("http://mathhub.info/FrameIT/frameworld"))
    val metaTheoryForSituationTheory: MPath = rootDocument ? "FrameworldMeta"

    val situationTheoryForDebugging: MPath = (rootDocument / "integrationtests") ? "SampleSituationTheory"

    private val _metaAnnotations: MPath = rootDocument ? "MetaAnnotations"

    object MetaKeys {
      val label: GlobalName = _metaAnnotations ? "label"
      val description: GlobalName = _metaAnnotations ? "description"
      val problemTheory: GlobalName = _metaAnnotations ? "problemTheory"
      val solutionTheory: GlobalName = _metaAnnotations ? "solutionTheory"
      val scrollDescription: GlobalName = _metaAnnotations ? "description"
    }

    object MetaFunctions {
      val labelVerbOf: GlobalName = _metaAnnotations ? "label_verbalization_of"
    }
  }
}
