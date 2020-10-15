package info.kwarc.mmt.frameit.communication.datastructures

import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.{GlobalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.{KnownFact, SFact, SGeneralFact, SValueEqFact}
import info.kwarc.mmt.frameit.communication.datastructures.SOMDoc.{OMDocBridge, SFloatingPoint, SInteger, SOMA, SOMS, SString, STerm}
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

  object TermCodecs {
    implicit def termDecoder(implicit stermDecoder: Decoder[STerm]): Decoder[Term] = (c: HCursor) => {
      stermDecoder(c).map(OMDocBridge.decode)
    }

    implicit def termEncoder(implicit stermEncoder: Encoder[STerm]): Encoder[Term] = (tm: Term) => {
      stermEncoder(OMDocBridge.encode(tm))
    }
  }

  object SOMDocCodecs {
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
          classOf[SInteger] -> "OMI"
        ).map { case (key, value) => (key.getSimpleName, value) }

        rewriteMap.getOrElse(oldCtorName, oldCtorName)
      })
  }

  object FactCodecs {
    // vvv DO NOT REMOVE even if IntelliJ marks it as unused

    import PathCodecs._
    import io.circe.generic.extras.semiauto.deriveConfiguredEncoder

    implicit val factJsonConfig: Configuration = Configuration.default
      .withDiscriminator("kind")
      .copy(transformConstructorNames = oldCtorName => {
        val rewriteMap = Map(
          classOf[SGeneralFact] -> "general",
          classOf[SValueEqFact] -> "veq"
        ).map { case (key, value) => (key.getSimpleName, value) }

        rewriteMap.getOrElse(oldCtorName, oldCtorName)
      })

    // use the encoder as given by the _above_ configuration
    // (e.g. by contrast using deriveEncoder[SFact] would be wrong [but would compile!] as it would neglect the above configuration)
    implicit val factEncoder: Encoder[SFact] = deriveConfiguredEncoder[SFact]

    implicit val knownFactEncoder: Encoder[SFact with KnownFact] = (knownFact: SFact with KnownFact) => {
      // just add `uri: ...` field to encoded fact
      Json.fromJsonObject(
        // assumption: facts are encoded as objects
        factEncoder(knownFact).asObject.getOrElse(???).add("uri", globalNameEncoder(knownFact.ref.uri))
      )
    }

    // No knownFactDecoder (not needed yet)
  }
}
