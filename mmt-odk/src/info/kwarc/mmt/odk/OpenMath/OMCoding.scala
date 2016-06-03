package info.kwarc.mmt.odk.OpenMath

import scala.xml.{Node, Text, TextBuffer, Elem, Attribute, Null}

/**
  * Represents an OpenMath Coding
  * @tparam T Type this OpenMath Coding codes from / to
  */
abstract class OMCoding[T] {

  /**
    * Decodes an OpenMath Object
    *
    * @param t object to decode
    * @return
    */
  def decode(t : T) : OMAny

  /** decodes an OMNode object **/
  def decodeAnyVal(t : T) : OMAnyVal

  private def decodeAsVal[V <: OMAnyVal](t : T) : V = decodeAnyVal(t) match { case v : V => v}

  def decodeExpression(t : T): OMExpression = decodeAsVal(t)
  def decodeReference(t : T) : OMReference = decodeAsVal(t)
  def decodeInteger(t : T) : OMInteger = decodeAsVal(t)
  def decodeFloat(t : T) : OMFloat = decodeAsVal(t)
  def decodeString(t : T) : OMString = decodeAsVal(t)
  def decodeBytes(t : T) : OMBytes = decodeAsVal(t)
  def decodeVariable(t : T) : OMVariable = decodeAsVal(t)
  def decodeSymbol(t : T) : OMSymbol = decodeAsVal(t)
  def decodeDerived(t : T) : OMDerivedElement = decodeAsVal(t)
  def decodeForeign(t : T) : OMForeign = decodeAsVal(t)
  def decodeCompound(t : T) : OMCompoundElement = decodeAsVal(t)
  def decodeApplication(t : T) : OMApplication = decodeAsVal(t)
  def decodeAttribution(t : T) : OMAttribution = decodeAsVal(t)
  def decodeBinding(t : T) : OMBinding = decodeAsVal(t)
  def decodeError (t : T) : OMError = decodeAsVal(t)


  /**
    * encodes an OpenMath related object
    *
    * @param om OpenMath Object to encode
    * @return
    */
  def encode(om : OMAny) : T

  def apply(om : OMAny) = encode(om)
  def apply[S <: OMAny ](t : T) : S = decode(t) match { case s : S => s}
}

object OMCoding {
  /**
    * Converts a hexadecimal Double representation into a double
 *
    * @param hex length 16 hex string representing the number
    * @return
    */
  def hex2Double(hex : String) : Double = {
    throw new Exception("Hexadecimal floats are not currently supported")
    if(hex.length != 16){
      throw new Exception("The hex must be exactly of length 16")
    }

    // Don't ask me how this works, its adpated from a really old implementation
    // see [[fr.inria.openmath.omapi.implementation.XMLParser.parseLongHexInvertEndianess]]
    val longHexInvertEndianess : Long = (0 to 7).foldRight(0 : Long)({
      case (i, result) =>
        val MSNib = Character.digit(hex.charAt(i * 2), 16)
        val LSNib = Character.digit(hex.charAt(i * 2 + 1), 16)
        (result << 8) | (MSNib << 4) | LSNib
    })

    java.lang.Double.longBitsToDouble(longHexInvertEndianess)
  }

  def bytes2Hex(bytes : List[Byte]) : String = new sun.misc.BASE64Encoder().encode(bytes.toArray)
  def hex2Bytes(hex : String) : List[Byte] = new sun.misc.BASE64Decoder().decodeBuffer(hex).toList
}