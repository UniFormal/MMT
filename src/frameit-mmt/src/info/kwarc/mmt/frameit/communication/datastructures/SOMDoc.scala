package info.kwarc.mmt.frameit.communication.datastructures

import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.{PosOrIntLiterals, RealLiterals, StringLiterals}
import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.objects.{OMA, OML, OMS, Term}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX.{Product, Tuple}
import io.circe.Json
import io.circe.generic.extras.ConfiguredJsonCodec
import io.circe.syntax.EncoderOps

object SOMDoc {

  // vvvvvvv DO NOT REMOVE IMPORTS (even if IntelliJ marks it as unused)
  import Codecs.PathCodecs._
  import Codecs.SOMDocCodecs._
  import Codecs.SOMDocCodecs.config._
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
  case class SInteger(decimal: Int) extends STerm

  @ConfiguredJsonCodec
  case class SFloatingPoint(float: Double) extends STerm

  @ConfiguredJsonCodec
  case class SString(string: String) extends STerm

  @ConfiguredJsonCodec
  case class SRecArg(name: String, value: STerm) extends STerm

  /**
    * OMDoc terms that could not be represented with other SOMDoc case classes.
    *
    * @param xml The OMDoc XML representation of the term.
    */
  @ConfiguredJsonCodec
  case class SRawOMDoc(xml: String) extends STerm

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

    private object NestedProduct {
      def unapply(t: Term): Option[List[Term]] = t match {
        case Product(left, right) => Some(unapply(left).getOrElse(List(left)) ::: unapply(right).getOrElse(List(right)))
        case _ => None
      }
    }

    def encode(tm: Term): STerm = tm match {
      case OMS(path) => SOMS(path)

      // support function applications from LFX (that work with raw OMAs)
      case OMA(OMS(fun), args) if fun.doc.toString.startsWith(FrameWorld.LFXPath.toString) =>
        SOMA(SOMS(fun), args.map(encode))

      // for everything not from LFX, only support LF function application
      case ApplySpine(fun, args) => SOMA(encode(fun), args.map(encode))

      case PosOrIntLiterals(value) =>
        if (value.isValidInt) {
          SInteger(value.intValue)
        } else {
          System.err.println(s"encountered term for positive or int literals that would overflow Scala's Int: `$tm``")
          SRawOMDoc(tm.toNode.toString())
        }
      case RealLiterals(value) => SFloatingPoint(value)
      case StringLiterals(value) => SString(value)

      case OML(name, _, Some(value), _, _) =>
        SRecArg(name.toString, encode(value))

      case _ =>
        System.err.println(s"encountered term for which there is no SimpleOMDoc analogon: `$tm``")
        SRawOMDoc("no raw OMDoc XML here for performance reasons")
        // raw XML could be returned via: SRawOMDoc(tm.toNode.toString())
    }

    def decode(stm: STerm): Term = stm match {
      case SOMS(uri) => OMS(uri)
      // support function applications from LFX (that work with raw OMAs)
      case SOMA(SOMS(fun), args) if fun.doc.toString.startsWith(FrameWorld.LFXPath.toString) =>
        OMA(OMS(fun), args.map(decode))
      // otherwise (whether correct or not) enforce LF function applications
      case SOMA(fun, args) => ApplySpine(decode(fun), args.map(decode): _*)
      case SInteger(value) => PosOrIntLiterals(value)
      case SFloatingPoint(value) => RealLiterals(value)
      case SString(value) => StringLiterals(value)
      case SRawOMDoc(rawXml) => ???
    }
  }

  object JSONBridge {
    def encode(stm: STerm): Json = stm.asJson

    def decodeTerm(str: String): STerm = io.circe.parser.decode[STerm](str).getOrElse(
      throw ConversionException(s"could not decode string to STerm: ${str}")
    )
  }

}
