package info.kwarc.mmt.frameit.communication.datastructures

import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.{PosOrIntLiterals, RealLiterals, StringLiterals}
import info.kwarc.mmt.api.{GlobalName, ParseError}
import info.kwarc.mmt.api.objects.{OMA, OML, OMS, OMV, Term}
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

  //

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
  case class SInteger(decimal: Int) extends STerm

  @ConfiguredJsonCodec
  case class SFloatingPoint(float: Double) extends STerm

  @ConfiguredJsonCodec
  case class SString(string: String) extends STerm

  @ConfiguredJsonCodec
  case class SRecArg(name: String, value: STerm) extends STerm

  @ConfiguredJsonCodec
  case class SVariable(name: String) extends STerm

  // see Codecs.SOMDocCodecs for the custom JSON codec
  case class SFunction(params: List[(String, STerm)], body: STerm) extends STerm

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
      case OMS(path) => SOMS(path)

      // support function applications from LFX (that work with raw OMAs)
      case OMA(OMS(fun), args) if fun.doc.toString.startsWith(FrameWorld.LFXPath.toString) =>
        SOMA(SOMS(fun), args.map(encode))

      // for everything not from LFX, only support LF function application
      case ApplySpine(fun, args) => SOMA(encode(fun), args.map(encode))
      case FunTerm(params, body) if params.nonEmpty =>
        // TODO: this might swallow named parameters
        SFunction(params.map { case (name, tp) => (name.toString, encode(tp)) }, encode(body))
      case FunType(params, ret) if params.nonEmpty =>
        // TODO: this might swallow named parameters
        SFunctionType(params.map(_._2).map(encode), encode(ret))

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

      case OMV(x) => SVariable(x.toString)

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

      // The following cases so far would never be transmitted from the game engine
      // to MMT, thus we don't handle them.
      case SFunction(_, _) | SFunctionType(_, _) | SVariable(_) |
            SRecArg(_, _) =>
        throw ParseError("Received SOMDoc construct from game engine that we had chosen not to handle (decode) so far.")

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
