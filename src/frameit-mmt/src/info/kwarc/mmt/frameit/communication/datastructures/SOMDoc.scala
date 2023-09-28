package info.kwarc.mmt.frameit.communication.datastructures

import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.{PosOrIntLiterals, RealLiterals, StringLiterals}
import info.kwarc.mmt.api.{GlobalName, ParseError}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.lf.{ApplySpine, FunTerm, FunType}
import info.kwarc.mmt.odk.LFX.{Product, Tuple}
import io.circe.Json
import io.circe.generic.extras.ConfiguredJsonCodec
import io.circe.syntax.EncoderOps

/**
  * Simplified OMDoc - a subset of OMDoc to represent formal knowledge just enough for FrameIT clients and use cases.
  *
  * [[STerm]] is the base class of all SOMDoc terms. Using [[Codecs]] and the [[io.circe]] JSON library (and its
  * Scala macros), any such term can be converted to and from JSON.
  * The class hierarchy of [[STerm]] and the JSON codecs are built such that the JSON representation is almost a
  * subset of the [[https://omjson.kwarc.info/ OpenMath-JSON standard]].
  */
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

  /**
    * Base class of every simplified OMDoc-represented term.
    *
    * The class hierarchy in conjunction with [[Codecs]] and the [[io.circe]] JSON library is intended to yield
    * a JSON representation that is almost a subset to the [[https://omjson.kwarc.info/ OpenMath-JSON standard]].
    * An important deviation is for the case of binders: we only support LF functions, function types, and lambdas;
    * no other binder.
    *
    * == IMPORTANT NOTICE WHEN ADDING OR MODIFYING STerm or any of its subclasses ==
    *
    * Every field name of every [[STerm]] (sub)class carries meaning and gets used when io.circe
    * (via Scala macro magic) derives corresponding JSON encoders and decoders, i.e., determines the JSON
    * representation.
    * Do not naively rename any field names!
    * If you must, instead rename and add io.circe's annotation to re-rename back to how the JSON should be.
    *
    * The name of STerm and its subclasses on the other hand do not carry meaning. You can simply rename them.
    *
    * When you add a new subclass of [[STerm]], be sure to add a corresponding case in [[Codecs.SOMDocCodecs]].
    */
  @ConfiguredJsonCodec
  sealed trait STerm

  @ConfiguredJsonCodec
  case class SOMS(uri: GlobalName) extends STerm

  @ConfiguredJsonCodec
  case class SOMA(applicant: STerm, arguments: List[STerm]) extends STerm

  @ConfiguredJsonCodec
  case class SOML(name: String, tp: Option[STerm], df: Option[STerm]) extends  STerm

  // Encoder does not like AnyVal
  @ConfiguredJsonCodec
  case class OMLITBool(`type`: String, value: Boolean) extends STerm
  @ConfiguredJsonCodec
  case class OMLITByte(`type`: String, value: Byte) extends STerm
  @ConfiguredJsonCodec
  case class OMLITShort(`type`: String, value: Short) extends STerm
  @ConfiguredJsonCodec
  case class OMLITInt(`type`: String, value: Int) extends STerm
  @ConfiguredJsonCodec
  case class OMLITLong(`type`: String, value: Long) extends STerm
  @ConfiguredJsonCodec
  case class OMLITFloat(`type`: String, value: Float) extends STerm
  @ConfiguredJsonCodec
  case class OMLITDouble(`type`: String, value: Double) extends STerm
  @ConfiguredJsonCodec
  case class OMLITChar(`type`: String, value: Char) extends STerm
  @ConfiguredJsonCodec
  case class OMLITString(`type`: String, value: String) extends STerm

  @ConfiguredJsonCodec
  case class SVariable(name: String) extends STerm

  // no @ConfiguredJsonCodec here!
  // see Codecs.SOMDocCodecs for the custom JSON codec (an implicit val variable)
  //@ConfiguredJsonCodec
  case class SFunction(params: List[(String, STerm)], body: STerm) extends STerm
  //case class SFunctionParam(name: String, `type`: STerm) extends STerm

  @ConfiguredJsonCodec
  case class SFunctionType(params: List[STerm], ret: STerm) extends STerm

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
      case OMS(path) =>
        SOMS(path)

      // support function applications from LFX (that work with raw OMAs)
      case OMA(OMS(fun), args) if fun.doc.toString.startsWith(FrameWorld.LFXPath.toString) =>
        SOMA(SOMS(fun), args.map(encode))

      // for everything not from LFX, only support LF function application
      case ApplySpine(fun, args) =>
        SOMA(encode(fun), args.map(encode))

      case FunTerm(params, body) if params.nonEmpty =>
        // TODO: this might swallow named parameters
        SFunction(params.map { case (name, tp) => (name.toString, encode(tp)) }, encode(body))

      case FunType(params, ret) if params.nonEmpty =>
        // TODO: this might swallow named parameters
        SFunctionType(params.map(_._2).map(encode), encode(ret))


      case OMLIT(anyvalue, rt) =>
        anyvalue match {
          case bool: Boolean =>
            OMLITBool(rt.synType.toString, bool)
          case byte: Byte =>
            OMLITByte(rt.synType.toString, byte)
          case short: Short =>
            OMLITShort(rt.synType.toString, short)
          case int: Int =>
            OMLITInt(rt.synType.toString, int)
          case long: Long =>
            OMLITLong(rt.synType.toString, long)
          case float: Float =>
            OMLITFloat(rt.synType.toString, float)
          case double: Double =>
            OMLITDouble(rt.synType.toString, double)
          case char: Char =>
            OMLITChar(rt.synType.toString, char)
          case string: String =>
            OMLITString(rt.synType.toString, string)
        }

      case UnknownOMLIT(unknown, synType) =>
        tm match {
          case RealLiterals(double) =>
            OMLITDouble(synType.toString, double)
          case StringLiterals(string) =>
            OMLITString(synType.toString, string)
          case PosOrIntLiterals(value) =>
            if (value.isValidInt) {
              OMLITInt(synType.toString, value.intValue)
            } else {
              System.err.println(s"encountered term for positive or int literals that would overflow Scala's Int: `$tm``")
              SRawOMDoc(tm.toNode.toString())
            }
          case _ =>
            if (false.toString == unknown || true.toString == unknown)
              OMLITBool(synType.toString, unknown.toBoolean)
            else
              SRawOMDoc(tm.toNode.toString())
        }

      case OML(name, tp, df, _, _) =>
        SOML(name.toString, tp.map(encode), df.map(encode))

      case OMV(x) =>
        SVariable(x.toString)

      case _ =>
        System.err.println(s"encountered term for which there is no SimpleOMDoc analogon: `$tm``")
        //SRawOMDoc("no raw OMDoc XML here for performance reasons")
        //raw XML could be returned via:
        SRawOMDoc(tm.toNode.toString())
    }

    def decode(stm: STerm): Term = stm match {
      case SOMS(uri) => OMS(uri)
      // support function applications from LFX (that work with raw OMAs)
      case SOMA(SOMS(fun), args) if fun.doc.toString.startsWith(FrameWorld.LFXPath.toString) =>
        OMA(OMS(fun), args.map(decode))
      // otherwise (whether correct or not) enforce LF function applications
      case SOMA(fun, args) => ApplySpine(decode(fun), args.map(decode): _*)

      case OMLITInt(_, value) => PosOrIntLiterals(value)
      case OMLITFloat(_, value) => RealLiterals(value)
      case OMLITDouble(_, value) => RealLiterals(value)
      case OMLITString(_, value) => StringLiterals(value)

      // This is a LIE: The following cases so far would never be transmitted from the game engine
      // to MMT, thus we don't handle them.

      case SRawOMDoc(rawXml) => ???

      case _ | SFunction(_, _) | SFunctionType(_, _) | SVariable(_) =>
        throw ParseError("Received SOMDoc construct from game engine that we had chosen not to handle (decode) so far.")
    }
  }

  object JSONBridge {
    def encode(stm: STerm): Json = stm.asJson

    def decodeTerm(str: String): STerm = io.circe.parser.decode[STerm](str).getOrElse(
      throw ConversionException(s"could not decode string to STerm: ${str}")
    )
  }

}
