package info.kwarc.mmt.frameit.communication.datastructures

import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.{GlobalName, LocalName, MPath, NamespaceMap, Path, SimpleStep}
import info.kwarc.mmt.frameit.business.Scroll
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.{FactReference, KnownFact, SFact, SGeneralFact, SScrollApplication, SValueEqFact}
import info.kwarc.mmt.frameit.communication.datastructures.SOMDoc.{OMDocBridge, SFloatingPoint, SInteger, SOMA, SOMS, SRawOMDoc, SString, STerm}
import io.circe.generic.extras.Configuration
import io.circe.{Decoder, Encoder, HCursor, Json}

import scala.util.Try

/**
  * All the JSON codecs for [[DataStructures]] and [[SOMDoc]]
  */
object Codecs {
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

    object config {
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

  object FactCodecs {
    // vvv DO NOT REMOVE even if IntelliJ marks it as unused
    import PathCodecs._
    import SOMDocCodecs._

    import io.circe.generic.extras.semiauto._
    // ^^^^^^^ END: DO NOT REMOVE

    object config {
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

    // vvv DO NOT REMOVE even if IntelliJ marks it as unused
    import config._
    // ^^^^^^^ END: DO NOT REMOVE

    implicit val sfactEncoder: Encoder[SFact] = deriveConfiguredEncoder
    implicit val sfactDecoder: Decoder[SFact] = deriveConfiguredDecoder

    implicit val d: Decoder[SScrollApplication] = ???

    implicit val ee: Encoder[FactReference] = ???
    implicit val e: Encoder[Scroll] = ???

    implicit val knownFactEncoder: Encoder[SFact with KnownFact] = (knownFact: SFact with KnownFact) => {
      // just add `uri: ...` field to encoded fact
      Json.fromJsonObject(
        // assumption: facts are encoded as objects
        sfactEncoder(knownFact).asObject.getOrElse(???).add("uri", globalNameEncoder(knownFact.ref.uri))
      )
    }

    // No knownFactDecoder (not needed yet)
  }

  object DataStructureCodecs {

  }
}
