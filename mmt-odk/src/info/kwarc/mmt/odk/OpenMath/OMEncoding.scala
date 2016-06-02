package info.kwarc.mmt.odk.OpenMath

import scala.xml.{Node, Text, TextBuffer, Elem, Attribute, Null}

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
  def apply[S <: OMAny](t : T) : S = decode(t) match { case s : S => s}
}

object OMEncoding {
  /**
    * Converts a hexadecimal Double representation into a double
    * @param hex length 16 hex string representing the number
    * @return
    */
  def Hex2Double(hex : String) : Double = {
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

  def Bytes2Hex(bytes : List[Byte]) : String = new sun.misc.BASE64Encoder().encode(bytes.toArray)
  def Hex2Bytes(hex : String) : List[Byte] = new sun.misc.BASE64Decoder().decodeBuffer(hex).toList
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

  private def setAttr(e : Elem, av : String*) : Elem = {
    av.toList.sliding(2, 2).map({
      case a::v::Nil => (a, v)
    }).foldLeft(e)({
      case (f, (attr, null)) => f
      case (f, (attr, value)) => f % Attribute(None, attr, Text(value), Null)
    })
  }


  private def decodeAttributionPairs( v : Node ) : OMAttributionPairs = v match {
    case <OMATP>{cnodes @ _*}</OMATP> =>
      val pairs = cnodes.toList.sliding(2, 2).map({case a :: b :: Nil => (decodeSymbol(a), decodeNode(b))}).toList

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMAttributionPairs(pairs, id, cdbase)
  }

  private def decodeVar(v : Node) : OMVar = v match {
    case <OMV>{_*}</OMV> => OMVarVar(decodeVariable(v))
    case <OMATTR>{pnode}{vnode}</OMATTR> =>
      var pairs = decodeAttributionPairs(pnode)
      val value = decodeVar(vnode)

      val id = getOAttr(v, ATTR_ID)

      OMAttVar(pairs, value, id)
  }

  private def decodeBindVariables(v : Node) : OMBindVariables = v match {
    case <OMBVAR>{cnodes @ _*}</OMBVAR> =>
      val vars = cnodes.map(decodeVar).toList

      val id = getOAttr(v, ATTR_ID)

      OMBindVariables(vars, id)
  }

  def decodeNode(v : Node) : OMNode = OMXMLEncoding.trimNoFinalText(v) match {
    case <OMR></OMR> =>
      var href = getAttr(v, ATTR_HREF)

      val id = getOAttr(v, ATTR_ID)

      OMReference(href, id)
    // Basic Elements
    case <OMI>{iNode : Text}</OMI> =>
      val int = BigInt(iNode.text)

      val id = getOAttr(v, ATTR_ID)

      OMInteger(int, id)
    case <OMF></OMF> =>
      val dec = getOAttr(v, ATTR_DEC)
      val hex = getOAttr(v, ATTR_HEX)

      val dbl : Double = if(dec.isDefined){
        dec.get.toDouble
      } else {
        OMEncoding.Hex2Double(hex.get)
      }

      val id = getOAttr(v, ATTR_ID)

      OMFloat(dbl, id)
    case <OMSTR>{tNode : Text}</OMSTR> =>
      val text = tNode.text

      val id = getOAttr(v, ATTR_ID)

      OMString(text, id)
    case <OMB>{bNode : Text}</OMB> =>
      val bytes = OMEncoding.Hex2Bytes(bNode.text)

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
    case <OMFOREIGN>{fNode}</OMFOREIGN> =>
      val obj : Any = try {decode(fNode)} catch {case e: Exception => fNode}
      val encoding = getOAttr(v, ATTR_ENCODING)

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMForeign(obj, encoding, id, cdbase)

    // Compound elements
    case <OMA>{enode}{anodes @ _*}</OMA> =>
      val elem = decodeElement(enode)
      val args = anodes.map(decodeElement).toList

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMApplication(elem, args, id, cdbase)
    case <OMATTR>{pnode}{onode}</OMATTR> =>
      val pairs = decodeAttributionPairs(pnode)
      val obj = decodeElement(onode)

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMAttribution(pairs, obj, id, cdbase)
    case <OMBIND>{anode}{vnode}{cnode}</OMBIND> =>
      val A = decodeElement(anode)
      val vars  = decodeBindVariables(vnode)
      val C = decodeElement(cnode)

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMBinding(A, vars, C, id, cdbase)
    case <OME>{nnode}{pnodes @ _*}</OME> =>
      val name = decodeSymbol(nnode)
      val params = pnodes.map(decodeNode).toList

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE)

      OMError(name, params, id, cdbase)
  }

  /**
    * Decodes an XML encoded OpenMath object
    * @param node Node to decode
    * @return any OpenMath related object
    */
  def decode(node : Node) : OMAny = OMXMLEncoding.trimNoFinalText(node) match {

    // match a top level object in here
    case onode @ <OMOBJ>{enode}</OMOBJ> =>
      val version = getOAttr(node, ATTR_VERSION)
      val omel = decodeElement(enode)

      val id = getOAttr(onode, ATTR_ID)
      val cdbase = getOAttr(onode, ATTR_CDBASE)

      OMObject(omel, version, id, cdbase)

    // special cases => match specific functionc
    case onode @ <OMATP>{_*}</OMATP> => decodeAttributionPairs(onode)
    case onode @ <OMATTR>{_*}</OMATTR> => decodeVar(onode)
    case onode @ <OMBVAR>{_*}</OMBVAR> => decodeBindVariables(onode)

    // fallback to matching a generic OpenMath NOde
    case onode => decodeNode(onode)
  }

  private def encodeAttributionPairs(p : OMAttributionPairs) : Node = p match {
    case OMAttributionPairs(pairs, id, cdbase) =>
      setAttr(
        <OMATP>{pairs.flatMap({case (a,b) => encode(a) :: encode(b) :: Nil})}</OMATP>,

        ATTR_ID, id orNull
      )
  }

  private def encodeVar(v : OMVar) : Node = v match {
    case OMVarVar(vv) => encodeNode(vv)
    case OMAttVar(pairs, value, id) =>
      setAttr(
        <OMATTR>{encodeAttributionPairs(pairs)}{encodeVar(value)}</OMATTR>,

        ATTR_ID, id orNull
      )
  }
  private def encodeBindVariables(v : OMBindVariables) : Node = v match {
    case OMBindVariables(vars, id) =>
      setAttr(
        <OMBVAR>{vars.map(encodeVar)}</OMBVAR>,

        ATTR_ID, id orNull
      )
  }

  private def encodeNode(om : OMNode) = om match {
    case OMReference(href, id) =>
      setAttr(
        <OMR></OMR>,

        ATTR_HREF, href,

        ATTR_ID, id orNull
      )
    // Basic Elements
    case OMInteger(int, id) =>
      setAttr(
        <OMI>{int.toString}</OMI>,

        ATTR_ID, id orNull
      )
    case OMFloat(dbl, id) =>
      setAttr(
        <OMF></OMF>,

        ATTR_DEC, dbl.toString,

        ATTR_ID, id orNull
      )
    case OMString(text, id) =>
      setAttr(
        <OMSTR>{Text(text)}</OMSTR>,

        ATTR_ID, id orNull
      )
    case OMBytes(bytes, id) =>
      setAttr(
        <OMB>{OMEncoding.Bytes2Hex(bytes)}</OMB>,

        ATTR_ID, id orNull
      )
    case OMSymbol(name, cd, id, cdbase) =>
      setAttr(
        <OMS></OMS>,

        ATTR_NAME, name,
        ATTR_CD, cd,

        ATTR_ID, id orNull,
        ATTR_CDBASE, cdbase orNull
      )
    case OMVariable(name, id) =>
      setAttr(
        <OMV></OMV>,

        ATTR_NAME, name,

        ATTR_ID, id orNull
      )

    // Derived Elements
    case OMForeign(obj, encoding, id, cdbase) =>
      setAttr(
        <OMFOREIGN>{obj match {
          case a: OMAny => encode(a)
          case n : Node => n
          case _ => Text(obj.toString)
        }}</OMFOREIGN>,

        ATTR_ENCODING, encoding orNull,

        ATTR_ID, id orNull,
        ATTR_CDBASE, cdbase orNull
      )

    // Compound elements
    case OMApplication(elem, args, id, cdbase) =>
      setAttr(
        <OMA>
          {encode(elem)}{args.map(encode)}
        </OMA>,

        ATTR_ID, id orNull,
        ATTR_CDBASE, cdbase orNull
      )
    case OMAttribution(pairs, obj, id, cdbase) =>
      setAttr(
        <OMATTR>
          {encodeAttributionPairs(pairs)}{encode(obj)}
        </OMATTR>,

        ATTR_ID, id orNull,
        ATTR_CDBASE, cdbase orNull
      )
    case OMBinding(a, vars, c, id, cdbase) =>
      setAttr(
        <OMBIND>
          {encode(a)}{encodeBindVariables(vars)}{encode(c)}
        </OMBIND>,

        ATTR_ID, id orNull,
        ATTR_CDBASE, cdbase orNull
      )
    case OMError(name, params, id, cdbase) =>
      setAttr(
        <OME>
          {encode(name)}{params.map(encode)}
        </OME>,

        ATTR_ID, id orNull,
        ATTR_CDBASE, cdbase orNull
      )
  }

  /**
    * Encodes an OpenMath object as XML
    * @param om OpenMath Object to encode
    * @return
    */
  def encode(om : OMAny) : Node = om match {
    // match a top level object in here
    case OMObject(omel, version, id, cdbase) =>
      setAttr(
        <OMOBJ>
          {encodeNode(omel)}
        </OMOBJ>,

        ATTR_VERSION, version orNull,

        ATTR_ID, id orNull,
        ATTR_CDBASE, cdbase orNull
      )

    // special cases => match specific functionc
    case p: OMAttributionPairs => encodeAttributionPairs(p)
    case v : OMVar => encodeVar(v)
    case b : OMBindVariables => encodeBindVariables(b)

    // fallback to matching a generic OpenMath NOde
    case n : OMNode => encodeNode(n)
  }
}

object OMXMLEncoding {
  /**
    * Like [[scala.xml.Utility.trim]] except that it leaves element with ONLY TEXT in them untouched
    */
  def trimNoFinalText(x : Node) : Node = x match {
    case Elem(pre, lab, md, scp, child@_*) =>

      // map the children except if we only have a single Text child
      val children = child.toList match {
        case (t: Text) :: Nil => t :: Nil
        case l => l flatMap trimNoFinalTextProper
      }

      // and return a cleaned element
      Elem(pre, lab, md, scp, children.isEmpty, children: _*)
  }

  /**
    * Like [[scala.xml.Utility.trimProper]] except that it does not trim elements with ONLY text in them
    */
  def trimNoFinalTextProper(x: Node): Seq[Node] = x match {
    case _ : Elem => trimNoFinalText(x)
    case Text(s) =>
      new TextBuffer().append(s).toText
    case _ => x
  }
}