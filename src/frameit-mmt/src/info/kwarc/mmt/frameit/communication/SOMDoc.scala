package info.kwarc.mmt.frameit.communication

// IMPORTANT: do NOT run IntelliJ's automatic "import clean-up" utility. It will remove necessary imports in this file.

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.{Declaration, FinalConstant, TermContainer, Visibility}
import info.kwarc.mmt.api.{GlobalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.frameit.archives.Foundation.{IntegerLiterals, RealLiterals, StringLiterals}
import info.kwarc.mmt.lf.ApplySpine
import io.circe.generic.extras.ConfiguredJsonCodec
import io.circe.{Decoder, Encoder}

import scala.util.Try

object SOMDoc {
  // IMPORTANT: keep the following lines. Do not change unless you know what you're doing
  //
  //            they control how the JSON en- and decoders treat subclasses of [[SimpleOMDoc.STerm]]
  import io.circe.Json
  import io.circe.syntax._
  import io.circe.generic.extras.semiauto._
  import io.circe.generic.extras.auto._
  import io.circe.generic.extras.Configuration
  // IMPORTANT: end

  object JsonConfig {
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

  // vvv DO NOT REMOVE (even if IntelliJ marks it as unused)
  // vvv
  import JsonConfig.jsonConfig

  object PathCodecs {
    implicit val mpathEncoder: Encoder[MPath] = Encoder.encodeString.contramap[MPath](_.toString)
    implicit val mpathDecoder: Decoder[MPath] = Decoder.decodeString.emapTry { str => {
      Try(Path.parseM(str, NamespaceMap.empty))
    }}

    implicit val globalNameEncoder: Encoder[GlobalName] = Encoder.encodeString.contramap[GlobalName](_.toString)
    implicit val globalNameDecoder: Decoder[GlobalName] = Decoder.decodeString.emapTry { str => {
      Try(Path.parseS(str, NamespaceMap.empty))
    }}
  }

  // vvv DO NOT REMOVE (even if IntelliJ marks it as unused)
  // vvv
  import PathCodecs._

  // IMPORTANT: do not naively rename parameter names of the following case classes!
  //            That would change the derived JSON encoders and decoders, too!
  //
  //            Instead rename and add io.circe's annotation to re-rename back to how the JSON should be.

  @ConfiguredJsonCodec
  sealed trait STerm

  @ConfiguredJsonCodec
  case class SOMS(uri: GlobalName) extends STerm

  @ConfiguredJsonCodec
  case class SOMA(applicant: STerm, arguments: List[STerm]) extends STerm

  @ConfiguredJsonCodec
  case class SInteger(value: Int) extends STerm

  @ConfiguredJsonCodec
  case class SFloatingPoint(float: Double) extends STerm

  @ConfiguredJsonCodec
  case class SString(string: String) extends STerm

  @ConfiguredJsonCodec
  case class SFinalConstant(uri: GlobalName, tp: STerm, df: Option[STerm])

  final case class ConversionException(private val message: String = "",
                                   private val cause: Throwable = None.orNull)
    extends Exception(message, cause)


  object OMDocBridge {
    def encode(decl: Declaration): SFinalConstant = decl match {
      case f: FinalConstant => f.tp match {
        case Some(tp) => SFinalConstant(f.path, encode(tp), f.df.map(encode))
        case _ => throw ConversionException("cannot convert Declaration not containing type to SimpleOMDoc")
      }
      case _ => throw ConversionException(s"cannot convert declarations other than FinalConstant to SimpleOMDoc; declaration was ${decl}")
    }

    def decode(sdecl: SFinalConstant): FinalConstant = {
      new FinalConstant(
        OMMOD(sdecl.uri.module),
        sdecl.uri.name,
        alias = Nil,
        tpC = TermContainer.asParsed(decode(sdecl.tp)),
        dfC = TermContainer.asParsed(sdecl.df.map(decode)),
        rl = None,
        notC = new NotationContainer,
        vs = Visibility.public
      )
    }

    def encode(tm: Term): STerm = tm match {
      case OMS(path) => SOMS(path)
      // Only support OMA applications in LF style
      case ApplySpine(fun, args) => SOMA(encode(fun), args.map(encode))

      case IntegerLiterals(value) => SInteger(value.intValue()) // TODO: overflow possible
      case RealLiterals(value) => SFloatingPoint(value)
      case StringLiterals(value) => SString(value)

      case _ => throw ConversionException(s"encountered term for which there is no SimpleOMDoc analogon: ${tm}")
    }

    def decode(stm: STerm): Term = stm match {
      case SOMS(uri) => OMS(uri)
      case SOMA(fun, arguments) => ApplySpine(decode(fun), arguments.map(decode): _*)
      case SInteger(value) => IntegerLiterals(value)
      case SFloatingPoint(value) => RealLiterals(value)
      case SString(value) => StringLiterals(value)
    }
  }

  object JSONBridge {
    def encodeDeclaration(decl: SFinalConstant): Json = decl.asJson

    def encode(stm: STerm): Json = stm.asJson
    def decodeTerm(str: String): STerm = io.circe.parser.decode[STerm](str).getOrElse(
      throw ConversionException(s"could not decode string to STerm: ${str}")
    )
  }
}
