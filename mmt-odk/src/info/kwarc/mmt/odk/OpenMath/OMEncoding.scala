package info.kwarc.mmt.odk.OpenMath

import scala.xml.Node

abstract class OMEncoding[T] {

  /**
    * Decodes an OpenMath Object
    * @param t object to decode
    * @return
    */
  def decode(t : T) : OMAny

  /** decodes an OMNode object **/
  def decodeNode(t : T) : OMNode

  // DECODE convenience functions
  // scala type inference for the win
  private def dnp[S <: OMNode](t : T) : S = decodeNode(t) match { case s : S => s}

  def decodeObject(t : T) : OMElement = dnp(t)
  def decodeElement(t : T) : OMElement = dnp(t)
  def decodeReference(t : T) : OMReference = dnp(t)
  def decodeInteger(t : T) : OMInteger = dnp(t)
  def decodeFloat(t : T) : OMFloat = dnp(t)
  def decodeString(t : T) : OMString = dnp(t)
  def decodeBytes(t : T) : OMBytes = dnp(t)
  def decodeVariable(t : T) : OMVariable = dnp(t)
  def decodeSymbol(t : T) : OMSymbol = dnp(t)
  def decodeDerived(t : T) : OMDerivedElement = dnp(t)
  def decodeForeign(t : T) : OMForeign = dnp(t)
  def decodeCompound(t : T) : OMCompoundElement = dnp(t)
  def decodeApplication(t : T) : OMApplication = dnp(t)
  def decodeAttribution(t : T) : OMAttribution = dnp(t)
  def decodeBinding(t : T) : OMBinding = dnp(t)
  def decodeError (t : T) : OMError = dnp(t)


  /**
    * encodes an OpenMath related object
    * @param om OpenMath Object to encode
    * @return
    */
  def encode(om : OMAny) : T

  def apply(om : OMAny) = encode(om)
  def apply[S <: OMAny](n : T) : S = decode(n) match { case s : S => s}
}

class OMXMLEncoding extends OMEncoding[Node] {

  private val ATTR_ID = "id"
  private val ATTR_HREF = "href"
  private val ATTR_NAME = "name"
  private val ATTR_CD = "cd"
  private val ATTR_CDBASE = "cdbase"
  private val ATTR_DEC = "dec"
  private val ATTR_HEX = "hex"
  private val ATTR_ENCODING = "encoding"
  private val ATTR_VERSION = "version"

  private def getAttr(v : Node, attr : String) : String = v.attribute(attr).get.text
  private def getOAttr(v : Node, attr : String) : Option[String] = v.attribute(attr).map(_.text)

  private def Double2Hex(dbl : Double) : String = ???
  private def Hex2Double(hex : String) : Double = ???

  private def Bytes2Hex(bytes : List[Byte]) : String = new sun.misc.BASE64Encoder().encode(bytes.toArray)
  private def Hex2Bytes(hex : String) : List[Byte] = new sun.misc.BASE64Decoder().decodeBuffer(hex).toList

  private def decodeAttributionPairs( v : Node ) : OMAttributionPairs = v match {
    case <OMATP>{children @ _*}</OMATP> =>
      val pairs = children.toList.sliding(2, 2).map({case a :: b :: Nil => (decodeSymbol(a), decodeNode(b))}).toList

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMAttributionPairs(pairs, id, cdbase)
  }

  private def decodeVar(v : Node) : OMVar = v match {
    case <OMV>{_}</OMV> => OMVarVar(decodeVariable(v))
    case <OMATTR>{nodes}</OMATTR> =>
      var pairs = decodeAttributionPairs(nodes(0))
      val value = decodeVar(nodes(1))

      val id = getOAttr(v, ATTR_ID)

      OMAttVar(pairs, value, id)
  }

  private def decodeBindVariables (v : Node) : OMBindVariables = v match {
    case <OMBVAR>{children}</OMBVAR> =>
      val vars = children.map(decodeVar).toList

      val id = getOAttr(v, ATTR_ID)

      OMBindVariables(vars, id)
  }

  def decodeNode(v : Node) : OMNode = v match {
    // Basic Elements
    case <OMR></OMR> =>
      var href = getAttr(v, ATTR_HREF)

      val id = getOAttr(v, ATTR_ID)

      OMReference(href, id)
    case <OMI></OMI> =>
      val int = BigInt(v.text)

      val id = getOAttr(v, ATTR_ID)

      OMInteger(int, id)
    case <OMF></OMF> =>
      val dec = getOAttr(v, ATTR_DEC)
      val hex = getOAttr(v, ATTR_HEX)

      val dbl : Double = if(dec.isDefined){
        dec.get.toDouble
      } else {
        Hex2Double(hex.get)
      }

      val id = getOAttr(v, ATTR_ID)

      OMFloat(dbl, id)
    case <OMSTR></OMSTR> =>
      val text = v.text

      val id = getOAttr(v, ATTR_ID)

      OMString(v.text, id)
    case <OMB>{b : Node}</OMB> =>
      val bytes = Hex2Bytes(b.text)

      val id = getOAttr(v, ATTR_ID)

      OMBytes(bytes, id)
    case <OMS></OMS> =>
      val name = getAttr(v, ATTR_NAME)
      val cd = getAttr(v, ATTR_CD)

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMSymbol(name, cd, id, cdbase)
    case <OMV></OMV> =>
      val name = getAttr(v, ATTR_NAME)

      val id = getOAttr(v, ATTR_ID)

      OMVariable(name, id)


    // Derived Elements
    case <OMFOREIGN>{fn : Node}</OMFOREIGN> =>
      val obj : Any = try {decode(fn)} catch {case e: Exception => fn}
      val encoding = getOAttr(v, ATTR_ENCODING)

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMForeign(obj, encoding, id, cdbase)

    // Compound elements
    case <OMA>{nodes}</OMA> =>
      val elem = decodeElement(v(0))
      val args = nodes.drop(1).toList.map(decodeElement)

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMApplication(elem, args, id, cdbase)
    case <OMATTR>{nodes}</OMATTR> =>
      val pairs = decodeAttributionPairs(nodes(0))
      val obj = decodeElement(nodes(1))

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMAttribution(pairs, obj, id, cdbase)
    case <OMBIND>{nodes}</OMBIND> =>
      val A = decodeElement(nodes(0))
      val vars  = decodeBindVariables(nodes(1))
      val C = decodeElement(nodes(2))

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMBinding(A, vars, C, id, cdbase)
    case <OME>{nodes}</OME> =>
      val name = decodeSymbol(nodes(0))
      val params = nodes.drop(1).map(decodeNode).toList

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMError(name, params, id, cdbase)
  }

  def decode(v : Node) : OMAny = v match {
    case <OMOBJ>{c}</OMOBJ> =>
      val version = getOAttr(v, ATTR_VERSION)
      val omel = decodeElement(c(0))

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMObject(omel, version, id, cdbase)
    case <OMATP>{_}</OMATP> => decodeAttributionPairs(v)
    case <OMATTR>{_}</OMATTR> => decodeVar(v)
    case <OMBVAR>{_}</OMBVAR> => decodeBindVariables(v)
    case _ => decodeNode(v)
  }


  /*

  private def encodeBasic (om : OMBasicObject) : Node = om match {
    case OMInteger(int, id) => <OMI id={id orNull}>{int.toString}</OMI>
    case OMFloat(double, id) => <OMF id={id orNull} dec={double.toString}></OMF>
    case OMString(chars, id) => <OMS id={id orNull}>{chars.toString}</OMS>
    case OMByteArray(bytes, id) => <OMB id={id orNull}>{new sun.misc.BASE64Encoder().encode(bytes.toArray)}</OMB>
    case OMSymbol(name, cd, base, id) => <OMS name={name} cd={cd} base={base orNull} id={id orNull}></OMS>
    case OMVariable(name, id) => <OMV name={name} id={id orNull}></OMV>
  }

  private def encodeCompound (om : OMCompoundObject) = om match {
    case OMApplication(a, id, base) => <OMA id={id orNull}>{a map encode}</OMA>
    case OMAttribution(a, as, id, base) => <OMATTR id={id orNull}>{<OMATP>as.flatten.map(encode)</OMATP>}{encode(a)}</OMATTR>
    case OMBinding(b, v, c, id, base) => <OMBIND id={id orNull}>{encode(b)}{v.map(n => <OMBVAR>{n}</OMBVAR>.asInstanceOf[Node])}{encode(c)}</OMBIND>
    case OMError(s, as, id, base) => <OME id={id orNull}>{encode(s)}{as.map(n => <OMBVAR>{n}</OMBVAR>.asInstanceOf[Node])}</OME>
  }

  private def encodeDerived(om : OMDerivedObject) = om match {
    case OMForeign(a, encoding) => <OMFOREIGN encoding={encoding orNull}>{a}</OMFOREIGN>
  }
  */

  def encode(om : OMAny) : Node =  ???
}