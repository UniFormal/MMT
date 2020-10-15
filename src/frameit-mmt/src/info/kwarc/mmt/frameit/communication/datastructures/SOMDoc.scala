package info.kwarc.mmt.frameit.communication.datastructures

import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.frameit.archives.MitM.Foundation.{IntegerLiterals, RealLiterals, StringLiterals}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX.Tuple
import io.circe.Json
import io.circe.generic.extras.ConfiguredJsonCodec
import io.circe.syntax.EncoderOps

object SOMDoc {

  // vvvvvvv DO NOT REMOVE IMPORTS (even if IntelliJ marks it as unused)
  import Codecs.PathCodecs._
  import Codecs.TermCodecs._
  import Codecs.SOMDocCodecs._
  // ^^^^^^^ END: DO NOT REMOVE

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

  final case class ConversionException(private val message: String = "",
                                       private val cause: Throwable = None.orNull)
    extends Exception(message, cause)


  object OMDocBridge {

    private object NestedTuple {
      def unapply(t: Term): Option[List[Term]] = t match {
        case Tuple(left, right) => Some(unapply(left).getOrElse(List(left)) ::: unapply(right).getOrElse(List(right)))
        case _ => None
      }
    }

    def encode(tm: Term): STerm = tm match {
      case OMS(path) => SOMS(path)
      // special-case LFX' tuples, hacky workaround, TODO: keep?
      case NestedTuple(args) => SOMA(SOMS(Tuple.path), args.map(encode))

      // Only support OMA applications in LF style
      case ApplySpine(fun, args) => SOMA(encode(fun), args.map(encode))

      case IntegerLiterals(value) => SInteger(value.intValue()) // TODO: overflow possible
      case RealLiterals(value) => SFloatingPoint(value)
      case StringLiterals(value) => SString(value)

      case _ =>
        val errMsg = s"encountered term for which there is no SimpleOMDoc analogon: ${tm}"

        // also output on stderr because exceptions by encoders as instrumented by Finch are sometimes
        // happily swallowed
        System.err.println(s"ERROR: ${errMsg}")
        throw ConversionException(errMsg)
    }

    def decode(stm: STerm): Term = stm match {
      case SOMS(uri) => OMS(uri)
      case SOMA(fun, arguments) =>
        // special-case LFX' tuples, hacky workaround, TODO: keep?
        if (fun == SOMS(Tuple.path)) {
          Tuple(arguments.map(decode))
        } else {
          ApplySpine(decode(fun), arguments.map(decode): _*)
        }
      case SInteger(value) => IntegerLiterals(value)
      case SFloatingPoint(value) => RealLiterals(value)
      case SString(value) => StringLiterals(value)
    }
  }

  object JSONBridge {
    def encode(stm: STerm): Json = stm.asJson

    def decodeTerm(str: String): STerm = io.circe.parser.decode[STerm](str).getOrElse(
      throw ConversionException(s"could not decode string to STerm: ${str}")
    )
  }

}
