package info.kwarc.mmt.frameit.archives

import info.kwarc.mmt.api.objects.{OMID, OMS, Term}
import info.kwarc.mmt.api.uom.ConstantScala
import info.kwarc.mmt.api.{DPath, GlobalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.lf.{ApplySpine, BinaryLFConstantScala, UnaryLFConstantScala}

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

    object MetaAnnotations {
      object MetaKeys {
        val label: GlobalName = _metaAnnotations ? "label"
        val description: GlobalName = _metaAnnotations ? "description"
        val problemTheory: GlobalName = _metaAnnotations ? "problemTheory"
        val solutionTheory: GlobalName = _metaAnnotations ? "solutionTheory"
        val scrollDescription: GlobalName = _metaAnnotations ? "description"
      }

      // a flexary LF constant
      object LabelVerbalization {
        private val path = _metaAnnotations ? "label_verbalization_of"

        def apply(args: Term*): Term = ApplySpine(OMS(path), args : _*)

        def unapply(t: Term): Option[List[Term]] = t match {
          case ApplySpine(OMS(`path`), args) if args.nonEmpty => Some(args)
          case _ => None
        }
      }

      // todo: not formalized in FrameIT/frameworld yet!
      object DescriptionVerbalization extends UnaryLFConstantScala(_metaAnnotations, "description_verbalization_of")
    }
  }
}
