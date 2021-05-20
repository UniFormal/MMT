package info.kwarc.mmt.frameit.archives

import info.kwarc.mmt.api.objects.{OMID, OMS, Term}
import info.kwarc.mmt.api.uom.{ConstantScala, RepresentedRealizedType, StandardString}
import info.kwarc.mmt.api.{DPath, GlobalName, LocalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.frameit.archives.MMT.urtheories
import info.kwarc.mmt.frameit.archives.MMT.urtheories.string
import info.kwarc.mmt.frameit.archives.MitM.Foundation
import info.kwarc.mmt.frameit.business.{SituationSpace, SituationTheoryPath}
import info.kwarc.mmt.lf.{ApplySpine, BinaryLFConstantScala, UnaryLFConstantScala}

/**
  * Symbols declared and used (imported) in the [[https://gl.mathhub.info/FrameIT/frameworld FrameIT archive]]
  */
object FrameIT {

  /**
    * Symbols declared in the [[https://gl.mathhub.info/FrameIT/frameworld FrameIT/frameworld archive]] and
    * symbols used therein that are imported from other archives (e.g. MitM/Foundation, MMT/urtheories).
    *
    * The latter symbols are given here for encapsulation reasons: applications should depend
    * on, say, FrameWorld.string being a [[GlobalName]] to a string type, but not on whether it actually
    * comes from MitM/Foundation or MMT/urtheories.
    */
  object FrameWorld {
    val archiveID: String = "FrameIT/frameworld"
    val rootDocument: DPath = DPath(URI("http://mathhub.info/FrameIT/frameworld"))
    val metaTheoryForSituationTheory: MPath = rootDocument ? "FrameworldMeta"

    val debugSituationTheory: SituationTheoryPath = SituationTheoryPath(
      SituationSpace((rootDocument / "integrationtests") ? "SampleSituationSpace"),
      LocalName("Root"),
    )

    val defaultRootSituationTheory: SituationTheoryPath = SituationTheoryPath(
      SituationSpace(rootDocument ? "DefaultSituationSpace"),
      LocalName("Root")
    )

    object MetaAnnotations {
      private val _metaAnnotations: MPath = rootDocument ? "MetaAnnotations"
      object MetaKeys {
        val label: GlobalName = _metaAnnotations ? "label"
        val description: GlobalName = _metaAnnotations ? "description"
        val problemTheory: GlobalName = _metaAnnotations ? "problemTheory"
        val solutionTheory: GlobalName = _metaAnnotations ? "solutionTheory"
        val scrollDescription: GlobalName = _metaAnnotations ? "description"
      }

      // a flexary LF constant
      object LabelVerbalization {
        val path: GlobalName = _metaAnnotations ? "label_verbalization_of"

        def apply(args: Term*): Term = ApplySpine(OMS(path), args : _*)

        def unapply(t: Term): Option[List[Term]] = t match {
          case ApplySpine(OMS(`path`), args) if args.nonEmpty => Some(args)
          case _ => None
        }
      }

      // todo: not formalized in FrameIT/frameworld yet!
      object DescriptionVerbalization extends UnaryLFConstantScala(_metaAnnotations, "description_verbalization_of")
    }

    // Reexported symbols from other archives
    // ===============================================================
    val LFXPath: DPath = MMT.LFX.path

    val sketchOperator: GlobalName = MitM.Foundation.sketchOperator
    val eq: GlobalName = MitM.Foundation.eq
    val ded: GlobalName = MitM.Foundation.ded
    val string: GlobalName = MMT.urtheories.string
    val real: GlobalName = MitM.Foundation.Math.real
    val prop: GlobalName = MitM.Foundation.Math.prop
    val StringLiterals: urtheories.StringLiterals.type = MMT.urtheories.StringLiterals
    val RealLiterals: Foundation.RealLiterals.type = MitM.Foundation.RealLiterals

    object PosOrIntLiterals {
      def apply(i: BigInt): Term = {
        if (i >= 0) {
          Foundation.NatLiterals(i)
        } else {
          Foundation.IntegerLiterals(i)
        }
      }

      def unapply(tm: Term): Option[BigInt] = tm match {
        case Foundation.NatLiterals(i) => Some(i)
        case Foundation.PosLiterals(i) => Some(i)
        case Foundation.IntegerLiterals(i) => Some(i)
        case _ => None
      }
    }
  }
}
