package info.kwarc.mmt.frameit.communication.datastructures

import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.{GlobalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.frameit.business.datastructures.FactReference
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.{SDynamicScrollApplicationInfo, SFact, SGeneralFact, SScroll, SScrollApplication, SScrollAssignments, SValueEqFact}
import info.kwarc.mmt.frameit.communication.datastructures.SOMDoc.{OMDocBridge, SFloatingPoint, SInteger, SOMA, SOMS, SRawOMDoc, SString, STerm}
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.{deriveConfiguredDecoder, deriveConfiguredEncoder}
import io.circe.{CursorOp, Decoder, DecodingFailure, Encoder, HCursor, Json, JsonObject}

import scala.util.Try

/**
  * All the JSON codecs for [[DataStructures]] and [[SOMDoc]]
  *
  *
  * WARNINGS
  *
  * be cautious in using companion objects for abstract class/case classes for which you would like to derive
  * io.circe codecs!! See [[https://gitter.im/circe/circe?at=5f8ea822270d004bcfdb28e9]]
  *
  * Also, invariant of frameit scala code: io.circe.generic.{auto,semiauto} and io.circe.generic.extras.{auto,semiauto} includes should only occur in this file (Codecs.scala).
  * Reasoning: performance when compiling increases much more if io.circe implicits are not touched by the (iterative, caching) compiler! Also, abstract over io.circe which is brittle to use as a human being.
  *
  * Note: you cannot use deriveConfigured{Encoder,Decoder} for things that don't have a @ConfiguredJsonCodec annotation. Use derive{Encoder,Decoder} from plain io.circe.generic.semiauto (not from extras!).
  */
private[communication] object Codecs {
  object PathCodecs {
    implicit val mpathEncoder: Encoder[MPath] = Encoder.encodeString.contramap[MPath](_.toString)
    implicit val mpathDecoder: Decoder[MPath] = Decoder.decodeString.emapTry { str => {
      Try(Path.parseM(str, NamespaceMap.empty))
    }
    }

    implicit val globalNameEncoder: Encoder[GlobalName] = Encoder.encodeString.contramap[GlobalName](_.toString)
    implicit val globalNameDecoder: Decoder[GlobalName] = Decoder.decodeString.emapTry { str => {
      Try(Path.parseS(str, NamespaceMap.empty))
    }
    }
  }

  object SOMDocCodecs {
    import io.circe.generic.extras.semiauto._

    private[datastructures] object config {
      implicit val jsonConfig: Configuration = Configuration.default
        .withDiscriminator("kind")
        .copy(transformConstructorNames = oldCtorName => {
          // Cannot declare this in the outer object due to some weird
          // errors with circe-generic-extras macro magic
          val rewriteMap = Map(
            classOf[SOMA] -> "OMA",
            classOf[SOMS] -> "OMS",
            classOf[SFloatingPoint] -> "OMF",
            classOf[SString] -> "OMSTR",
            classOf[SInteger] -> "OMI",
            classOf[SRawOMDoc] -> "RAW"
          ).map { case (key, value) => (key.getSimpleName, value) }

          rewriteMap.getOrElse(oldCtorName, oldCtorName)
        })
    }

    import config._
    implicit val stermEncoder: Encoder[STerm] = deriveConfiguredEncoder
    implicit val stermDecoder: Decoder[STerm] = deriveConfiguredDecoder

    implicit val termDecoder: Decoder[Term] = (c: HCursor) => {
      stermDecoder(c).map(OMDocBridge.decode)
    }

    implicit val termEncoder: Encoder[Term] = (tm: Term) => {
      stermEncoder(OMDocBridge.encode(tm))
    }
  }

  object DataStructureCodecs {
    object FactCodecs {
      // vvvvvvv CAREFUL WHEN REMOVING IMPORTS (IntelliJ might wrongly mark them as unused)
      import PathCodecs._
      import SOMDocCodecs._
      // ^^^^^^^ END

      private[datastructures] object config {
        implicit val factJsonConfig: Configuration = Configuration.default
          .withDiscriminator("kind")
          .copy(transformConstructorNames = oldCtorName => {
            val rewriteMap = Map(
              classOf[SGeneralFact] -> "general",
              classOf[SValueEqFact] -> "veq"
            ).map { case (key, value) => (key.getSimpleName, value) }

            rewriteMap.getOrElse(oldCtorName, oldCtorName)
          })
      }

      // vvvvvvv CAREFUL WHEN REMOVING IMPORTS (IntelliJ might wrongly mark them as unused)
      import config._
      // ^^^^^^^ END

      implicit val sfactEncoder: Encoder[SFact] = deriveConfiguredEncoder
      implicit val sfactDecoder: Decoder[SFact] = deriveConfiguredDecoder

      // deliberately use generic.semiauto.derive{Encoder,Decoder}, not the ones from extras.generic.semiauto!
      implicit val factReferenceEncoder: Encoder[FactReference] = io.circe.generic.semiauto.deriveEncoder
      implicit val factReferenceDecoder: Decoder[FactReference] = io.circe.generic.semiauto.deriveDecoder

      /*val knownFactEncoder: Encoder[SFact with SKnownFact] = (knownFact: SFact with SKnownFact) => {
        // just add `uri: ...` field to encoded fact
        Json.fromJsonObject(
          // assumption: facts are encoded as objects
          sfactEncoder(knownFact).asObject.getOrElse(???).add("uri", globalNameEncoder(knownFact.ref.uri))
        )
      }*/

      // No knownFactDecoder (not needed yet)
    }

    // re-export implicits (implicitness isn't transitive in Scala)
    implicit val sfactEncoder: Encoder[SFact] = FactCodecs.sfactEncoder
    implicit val sfactDecoder: Decoder[SFact] = FactCodecs.sfactDecoder
    // implicit val knownFactEncoder: Encoder[SFact with SKnownFact] = FactCodecs.knownFactEncoder

    // vvvvvvv CAREFUL WHEN REMOVING IMPORTS (IntelliJ might wrongly mark them as unused)
    import PathCodecs._
    import SOMDocCodecs._

    import io.circe.generic.auto._
    import io.circe.generic.semiauto._
    // ^^^^^^^ END

    implicit val factReferenceEncoder: Encoder[FactReference] = deriveEncoder
    implicit val factReferenceDecoder: Decoder[FactReference] = deriveDecoder
    implicit val scrollApplicationDecoder: Decoder[SScrollApplication] = deriveDecoder
    implicit val scrollEncoder: Encoder[SScroll] = deriveEncoder

    // [[SScrollAssignments]] codecs
    //
    private val originalScrollAssignmentsEncoder = deriveEncoder[SScrollAssignments]
    private val originalScrollAssignmentsDecoder = deriveDecoder[SScrollAssignments]

    implicit val scrollAssignmentsEncoder: Encoder[SScrollAssignments] = assignments => {
      originalScrollAssignmentsEncoder(assignments).hcursor
        .downField("assignments")
        .values
        .map(values => Json.arr(values.toSeq : _*))
        .getOrElse(throw new Exception("This should not occur, did you change the signature/field names of SScrollAssignments?"))
    }
    implicit val scrollAssignmentsDecoder: Decoder[SScrollAssignments] = (c: HCursor) => {
      originalScrollAssignmentsDecoder(
        Json.fromJsonObject(JsonObject.singleton("assignments", c.value)).hcursor
      )
    }

    implicit val dynamicScrollInfoEncoder: Encoder[SDynamicScrollApplicationInfo] = deriveEncoder
    implicit val dynamicScrollInfoDecoder: Decoder[SDynamicScrollApplicationInfo] = deriveDecoder
  }
}
