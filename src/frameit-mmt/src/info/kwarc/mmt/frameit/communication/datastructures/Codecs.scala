package info.kwarc.mmt.frameit.communication.datastructures

import info.kwarc.mmt.api
import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.utils.mmt
import info.kwarc.mmt.api.{GlobalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.frameit.business.datastructures.{FactReference, ScrollReference}
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.{SCheckingError, SDynamicScrollInfo, SEquationSystemFact, SFact, SGeneralFact, SInvalidScrollAssignment, SMiscellaneousError, SNonTotalScrollApplication, SScroll, SScrollApplication, SScrollApplicationResult, SScrollAssignments, SValueEqFact}
import info.kwarc.mmt.frameit.communication.datastructures.SOMDoc.{OMDocBridge, SFloatingPoint, SFunction, SFunctionType, SInteger, SOMA, SOMS, SRawOMDoc, SRecArg, SString, STerm}
import io.circe.Decoder.Result
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.{deriveConfiguredDecoder, deriveConfiguredEncoder}
import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder, HCursor, Json, JsonObject}

import scala.util.Try

/**
  * All the JSON codecs for [[DataStructures]] and [[SOMDoc]]
  *
  *
  * ## WARNINGS
  *
  * - Be cautious in using companion objects for abstract class/case classes for which you would like to derive
  * io.circe codecs!! See [[https://gitter.im/circe/circe?at=5f8ea822270d004bcfdb28e9]]
  *
  * - Invariant of whole frameit-mmt code: io.circe.generic.{auto,semiauto} and io.circe.generic.extras.{auto,semiauto}
  *   imports should only occur in this file (Codecs.scala).
  *
  *   Reasons:
  *     - compilation is much faster when editing server endpoints (say in [[ConcreteServerEndpoints]]) if the
  *       implicit-heavy codec derivation is encapsulated (and thus cached by the compiler from previous runs)
  *       in another file (Codecs.scala)
  *     - Abstract over io.circe which is brittle to use as a human being.
  *
  * ## Things I wish I knew about io.circe myself
  *
  * - io.circe.generic.semiauto.derive{Encoder, Decoder} is something completely unrelated to
  *   io.circe.generic.extras.semiauto.derive{Encoder, Decoder}.
  *   Precisely because of this confusion arising from the name clash, the latter is deprecated.
  *
  * - You can derive "configure" codecs by having an implicit configuration in scope and using
  *   io.circe.generic.extras.semiauto.deriveConfigured{Encoder,Decoder}.
  *   You do *not* need a @ConfiguredJsonCodec annotation for this on the case classes for which you want to derive
  *   the codecs. See [[https://gitter.im/circe/circe?at=5f9934f1f2fd4f60fc3b4539]].
  *
  * - [[io.circe.generic.extras.semiauto.deriveConfiguredEncoder deriveConfiguredEncoder]] and
  *   [[io.circe.generic.extras.semiauto.deriveConfiguredDecoder deriveConfiguredDecoder]] will fail
  *   if there is no *or* more than one implicit [[Configuration]] in scope. In both cases, you will get the
  *   *same* error message (due to Scala 2 being limited in that regard, no fault of io.circe).
  *   Hence, if we want to use different configurations for different case classes, then we need to encapsulate
  *   the [[Configuration]] objects. This is what this file does via the use of ''object config'', see the code
  *   to learn more about that pattern.
  */
private[communication] object Codecs {
  /**
    * Create a Circe [[Configuration]] encoding case classes as objects with their name (subject to rewriteMap)
    * in a "kind" discriminator field.
    * @param rewriteMap With this map, you can control how specific case classes should be referred to in the
    *                   "kind" field.
    *
    * @see [[SOMDocCodecs.config]] for an example
    */
  private def kindedJsonConfig(rewriteMap: Map[Class[_], String]): Configuration = {
    val map = rewriteMap.map { case (key, value) => (key.getSimpleName, value) }
    Configuration.default.withDiscriminator("kind").copy(transformConstructorNames = oldCtorName => {
      map.getOrElse(oldCtorName, oldCtorName)
    })
  }

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
      implicit val somdocConfig: Configuration = kindedJsonConfig(Map(
        classOf[SOMA] -> "OMA",
        classOf[SOMS] -> "OMS",
        classOf[SFloatingPoint] -> "OMF",
        classOf[SString] -> "OMSTR",
        classOf[SInteger] -> "OMI",
        classOf[SRecArg] -> "RECARG",
        classOf[SFunction] -> "FUN",
        classOf[SFunctionType] -> "FUNTYPE",
        classOf[SRawOMDoc] -> "RAW"
      ))
    }

    // vvvvvvv CAREFUL WHEN REMOVING IMPORTS (IntelliJ might wrongly mark them as unused)
    import config._
    // ^^^^^^^ END
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
        implicit val factConfig: Configuration = kindedJsonConfig(Map(
          classOf[SGeneralFact] -> "general",
          classOf[SValueEqFact] -> "veq",
          classOf[SEquationSystemFact] -> "eqsys"
        ))
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
    implicit val factReferenceEncoder: Encoder[FactReference] = FactCodecs.factReferenceEncoder
    implicit val factReferenceDecoder: Decoder[FactReference] = FactCodecs.factReferenceDecoder
    implicit val sfactEncoder: Encoder[SFact] = FactCodecs.sfactEncoder
    implicit val sfactDecoder: Decoder[SFact] = FactCodecs.sfactDecoder
    // implicit val knownFactEncoder: Encoder[SFact with SKnownFact] = FactCodecs.knownFactEncoder

    // vvvvvvv CAREFUL WHEN REMOVING IMPORTS (IntelliJ might wrongly mark them as unused)
    import PathCodecs._
    import SOMDocCodecs._

    import io.circe.generic.auto._
    import io.circe.generic.semiauto._
    // ^^^^^^^ END

    implicit val scrollReferenceEncoder: Encoder[ScrollReference] = (ref: ScrollReference) => mpathEncoder(ref.declaringTheory)
    implicit val scrollReferenceDecoder: Decoder[ScrollReference] = (c: HCursor) => mpathDecoder(c).map(ScrollReference)

    implicit val scrollApplicationDecoder: Decoder[SScrollApplication] = deriveDecoder
    implicit val scrollEncoder: Encoder[SScroll] = deriveEncoder

    // [[SScrollAssignments]] codecs
    //
    private val assignmentCodec: Codec[(FactReference, Term)] = Codec.forProduct2("fact", "assignment")((a: FactReference, b: Term) => (a,b))(identity)

    implicit val scrollAssignmentsEncoder: Encoder[SScrollAssignments] = scrollAssignments => {
      Json.arr(scrollAssignments.assignments.map(assignmentCodec.apply): _*)
    }
    // a helper function which can make use of early returns
    private def scrollAssignmentsDecode(c: HCursor): Decoder.Result[SScrollAssignments] = {
      // todo: the [[DecodingFailure decoding failures]] specify [[Nil]] as ops, this might lead to bad debugging messages
      val assignments = c.values
        .getOrElse(return Left(DecodingFailure("scroll assignments not array", Nil)))
        .map(asg => assignmentCodec(asg.hcursor).getOrElse(return Left(DecodingFailure("individual scroll assignment not decodable", Nil))))

      Right(SScrollAssignments(assignments.toList))
    }
    implicit val scrollAssignmentsDecoder: Decoder[SScrollAssignments] = scrollAssignmentsDecode

    private object CheckingError {
      private[datastructures] object config {
        implicit val errorConfig: Configuration = kindedJsonConfig(Map(
          classOf[SInvalidScrollAssignment] -> "invalidAssignment",
          classOf[SNonTotalScrollApplication] -> "nonTotal",
          classOf[SMiscellaneousError] -> "unknown"
        ))
      }
      // vvvvvvv CAREFUL WHEN REMOVING IMPORTS (IntelliJ might wrongly mark them as unused)
      import config._
      // ^^^^^^^ END

      val checkingErrorEncoder: Encoder[SCheckingError] = deriveConfiguredEncoder
      // no decoder for [[SCheckingError]] needed at the moment
    }

    implicit val checkingErrorEncoder: Encoder[SCheckingError] = CheckingError.checkingErrorEncoder

    implicit val dynamicScrollInfoEncoder: Encoder[SDynamicScrollInfo] = deriveEncoder
    implicit val dynamicScrollInfoDecoder: Decoder[SDynamicScrollInfo] = deriveDecoder

    implicit val scrollApplicationResultEncoder: Encoder[SScrollApplicationResult] = deriveEncoder
    implicit val scrollApplicationResultDecoder: Decoder[SScrollApplicationResult] = deriveDecoder
  }

  object MiscCodecs {
    implicit val mmtErrorEncoder: Encoder[api.Error] = (a: api.Error) => Json.obj(
      "msg" -> Json.fromString(a.shortMsg),
      "details" -> Json.fromString(a.toStringLong)
    )
  }
}
