package info.kwarc.mmt.odk.OpenMath.Coding

import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.api.utils.URI

import scala.xml.{TextBuffer, _}

/**
  * Represents OpenMath encoded as XML
  */
class OMXMLCoding extends OMCoding[Node] {

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
      val pairs = cnodes.toList.sliding(2, 2).map({case a :: b :: Nil => (decodeSymbol(a), decodeAnyVal(b))}).toList

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE).map(URI.apply)

      OMAttributionPairs(pairs, id, cdbase)
  }

  private def decodeVar(v : Node) : OMVar = v match {
    case <OMV>{_*}</OMV> => OMVarVar(decodeVariable(v))
    case <OMATTR>{pnode}{vnode}</OMATTR> =>
      val pairs = decodeAttributionPairs(pnode)
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

  def decodeAnyVal(v : Node): OMAnyVal = OMXMLCoding.trimNoFinalText(v) match {
    case <OMR></OMR> =>
      var href = URI(getAttr(v, ATTR_HREF))

      val id = getOAttr(v, ATTR_ID)

      OMReference(href, id)
    // Basic Elements
    case <OMI>{iNode : Text}</OMI> =>
      val int = BigInt(iNode.text.trim)

      val id = getOAttr(v, ATTR_ID)

      OMInteger(int, id)
    case <OMF></OMF> =>
      val dec = getOAttr(v, ATTR_DEC)
      val hex = getOAttr(v, ATTR_HEX)

      val dbl : Double = if(dec.isDefined){
        dec.get.trim.toDouble
      } else {
        OMCoding.hex2Double(hex.get.trim)
      }

      val id = getOAttr(v, ATTR_ID)

      OMFloat(dbl, id)
    case <OMSTR>{tNode : Text}</OMSTR> =>
      val text = tNode.text

      val id = getOAttr(v, ATTR_ID)

      OMString(text, id)
    case <OMSTR></OMSTR> =>
      val id = getOAttr(v, ATTR_ID)
      OMString("", id)
    case <OMB>{bNode : Text}</OMB> =>
      val bytes = OMCoding.hex2Bytes(bNode.text)

      val id = getOAttr(v, ATTR_ID)

      OMBytes(bytes, id)
    case <OMS></OMS> =>
      val name = getAttr(v, ATTR_NAME)
      val cd = getAttr(v, ATTR_CD)

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE).map(URI.apply)

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
      val cdbase = getOAttr(v, ATTR_CDBASE).map(URI.apply)

      OMForeign(obj, encoding, id, cdbase)

    // Compound elements
    case <OMA>{enode}{anodes @ _*}</OMA> =>
      val elem = decodeExpression(enode)
      val args = anodes.map(decodeExpression).toList

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE).map(URI.apply)

      OMApplication(elem, args, id, cdbase)
    case <OMATTR>{pnode}{onode}</OMATTR> =>
      val pairs = decodeAttributionPairs(pnode)
      val obj = decodeExpression(onode)

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE).map(URI.apply)

      OMAttribution(pairs, obj, id, cdbase)
    case <OMBIND>{anode}{vnode}{cnode}</OMBIND> =>
      val B = decodeExpression(anode)
      val vars  = decodeBindVariables(vnode)
      val C = decodeExpression(cnode)

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE).map(URI.apply)

      OMBinding(B, vars, C, id, cdbase)
    case <OME>{nnode}{pnodes @ _*}</OME> =>
      val name = decodeSymbol(nnode)
      val params = pnodes.map(decodeAnyVal).toList

      val id = getOAttr(v, ATTR_ID)
      val cdbase = getOAttr(v, ATTR_CDBASE).map(URI.apply)

      OMError(name, params, id, cdbase)
  }

  /**
    * Decodes an XML encoded OpenMath object
    *
    * @param node Node to decode
    * @return any OpenMath related object
    */
  def decode(node : Node) : OMAny = OMXMLCoding.trimNoFinalText(node) match {

    // match a top level object in here
    case onode @ <OMOBJ>{enode}</OMOBJ> =>
      val omel = decodeExpression(enode)
      val version = getOAttr(node, ATTR_VERSION)

      val id = getOAttr(onode, ATTR_ID)
      val cdbase = getOAttr(onode, ATTR_CDBASE).map(URI.apply)

      OMObject(omel, version, id, cdbase)

    // special cases => match specific functionc
    case onode @ <OMATP>{_*}</OMATP> => decodeAttributionPairs(onode)
    case onode @ <OMBVAR>{_*}</OMBVAR> => decodeBindVariables(onode)

    // fallback to matching a generic OpenMath NOde
    case n => decodeAnyVal(n)
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

  private def encodeNode(om: OMAnyVal) = om match {
    case OMReference(href, id) =>
      setAttr(
        <OMR></OMR>,

        ATTR_HREF, href.toString,

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
        <OMB>{OMCoding.bytes2Hex(bytes)}</OMB>,

        ATTR_ID, id orNull
      )
    case OMSymbol(name, cd, id, cdbase) =>
      val base = cdbase.map(b => if (b.toString.endsWith("/")) b.toString else b.toString + "/") orNull

      setAttr(
        <OMS></OMS>,

        ATTR_NAME, name,
        ATTR_CD, cd,

        ATTR_ID, id orNull,
        ATTR_CDBASE, base
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
        ATTR_CDBASE, cdbase.map(_.toString) orNull
      )

    // Compound elements
    case OMApplication(elem, args, id, cdbase) =>
      setAttr(
        <OMA>
          {encode(elem)}{args.map(encode)}
        </OMA>,

        ATTR_ID, id orNull,
        ATTR_CDBASE, cdbase.map(_.toString) orNull
      )
    case OMAttribution(pairs, obj, id, cdbase) =>
      setAttr(
        <OMATTR>
          {encodeAttributionPairs(pairs)}{encode(obj)}
        </OMATTR>,

        ATTR_ID, id orNull,
        ATTR_CDBASE, cdbase.map(_.toString) orNull
      )
    case OMBinding(a, vars, c, id, cdbase) =>
      setAttr(
        <OMBIND>
          {encode(a)}{encodeBindVariables(vars)}{encode(c)}
        </OMBIND>,

        ATTR_ID, id orNull,
        ATTR_CDBASE, cdbase.map(_.toString) orNull
      )
    case OMError(name, params, id, cdbase) =>
      setAttr(
        <OME>
          {encode(name)}{params.map(encode)}
        </OME>,

        ATTR_ID, id orNull,
        ATTR_CDBASE, cdbase.map(_.toString) orNull
      )
  }

  /**
    * Encodes an OpenMath object as XML
    *
    * @param om OpenMath Object to encode
    * @return
    */
  def encode(om : OMAny) : Node = om match {
    // match a top level object in here
    case OMObject(omel, version, id, cdbase) =>
      setAttr(
        <OMOBJ xmlns="http://www.openmath.org/OpenMath" version="2.0">
          {encodeNode(omel)}
        </OMOBJ>,

        ATTR_VERSION, version orNull,

        ATTR_ID, id orNull,
        ATTR_CDBASE, cdbase.map(_.toString) orNull
      )

    // special cases => match specific functionc
    case p: OMAttributionPairs => encodeAttributionPairs(p)
    case v : OMVar => encodeVar(v)
    case b : OMBindVariables => encodeBindVariables(b)

    // fallback to matching a generic OpenMath NOde
    case n: OMAnyVal => encodeNode(n)
  }
}

object OMXMLCoding {
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
