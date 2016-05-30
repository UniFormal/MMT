package info.kwarc.mmt.odk.SCSCP.OpenMath

import scala.xml.Node

abstract class OMEncoding[T] {
  def decode(v : T) : OMAny
  def decodeObject(v : T) : OMObject = decode(v) match {case o:OMObject => o}
  def decodeDerived(v : T) : OMDerivedObject = decode(v) match {case d:OMDerivedObject => d}

  def encode(om : OMAny) : T
}

class OMXMLEncoding extends OMEncoding[Node] {

  // helpers to get a single attribute
  private def getAttr(v : Node, attr : String) : String = v.attribute(attr).get.text
  private def getOAttr(v : Node, attr : String) : Option[String] = v.attribute(attr).map(_.text)

  private def decodeBasic(v : Node) : Option[OMBasicObject] = v match {
    case <OMI></OMI> => Some(OMInteger(BigInt(v.text)))
    case <OMF></OMF> => {
      val dec = getOAttr(v, "dec")
      if(dec.isDefined){
        Some(OMFloat(dec.get.toDouble))
      } else {
        // TODO: hex support
        None
      }
    }
    case <OMSTR></OMSTR> => Some(OMString(v.text))
    case <OMB>{b : Node}</OMB> => Some(OMByteArray(new sun.misc.BASE64Decoder().decodeBuffer(b.text).toList))
    case <OMS></OMS> => Some(OMSymbol(getAttr(v, "name"), getAttr(v, "cd"), getOAttr(v, "base")))
    case <OMV></OMV> => Some(OMVariable(getAttr(v, "name")))

    case _ => None
  }

  private def decodeKVPairs(nodes : List[Node]) : List[((OMSymbol, OMAny))] = nodes.flatMap {
    case <OMATP>{children}</OMATP> =>
      children.sliding(2, 2).map(ns => {
        val oms = decodeObject(ns(0)).asInstanceOf[OMSymbol]
        val arg = decode(ns(1))
        (oms, arg)
      })
  }

  private def decodeCompound(v : Node) : Option[OMCompoundObject] = v match {
    case <OMA>{nodes}</OMA> => Some(OMApplication(nodes.map(decodeObject).toList))
    case <OMATTR>{nodes}</OMATTR> =>
      val A = decodeObject(nodes(-1))
      val AS = decodeKVPairs(nodes.slice(0, -1).toList)
      Some(OMAttribution(A, AS))
    case <OMBIND>{nodes}</OMBIND> =>
      val B = decodeObject(nodes(0))
      val v = nodes.slice(1, -1).map { case <OMBVAR>{omv}</OMBVAR> => decodeObject(omv).asInstanceOf[OMVariable] } // TODO: Unhack this
      val C = decodeObject(nodes(-1))
      Some(OMBinding(B, v.toList, C))
    case <OME>{nodes}</OME> =>
      val S = decodeObject(nodes(0)).asInstanceOf[OMSymbol]
      val AS = nodes.tail.map(decode).toList
      Some(OMError(S, AS))
    case _ => None
  }

  private def decodeDerivedObj(v : Node) : Option[OMDerivedObject] = v match {
    case <OMFOREIGN>{fn : Node}</OMFOREIGN> => Some(OMForeign(fn, getOAttr(v, "encoding")))
  }

  def decode(v : Node) : OMAny = v match {
    // TODO: OMR
    case <OMOBJ>v</OMOBJ> =>
      val basic = decodeBasic(v)
      if(basic.isDefined) {
        basic.get
      } else {
        val compound = decodeCompound(v)
        if (compound.isDefined){
          compound.get
        } else {
          decodeDerivedObj(v).get
        }
      }
  }


  private def encodeBasic (om : OMBasicObject) : Node = om match {
    case OMInteger(int) => <OMI>{int.toString}</OMI>
    case OMFloat(double) => <OMF dec={double.toString}></OMF>
    case OMString(chars) => <OMS>{chars.toString}</OMS>
    case OMByteArray(bytes) => <OMB>{new sun.misc.BASE64Encoder().encode(bytes.toArray)}</OMB>
    case OMSymbol(name, cd, base) => <OMS name={name} cd={cd} base={base orNull}></OMS>
    case OMVariable(name) => <OMV name={name}></OMV>
  }

  private def encodeCompound (om : OMCompoundObject) = om match {
    case OMApplication(a) => <OMA>{a map encode}</OMA>
    case OMAttribution(a, as) => <OMATTR>{<OMATP>as.flatten.map(encode)</OMATP>}{encode(a)}</OMATTR>
    case OMBinding(b, v, c) => <OMBIND>{encode(b)}{v.map(n => <OMBVAR>{n}</OMBVAR>.asInstanceOf[Node])}{encode(c)}</OMBIND>
    case OMError(s, as) => <OME>{encode(s)}{as.map(n => <OMBVAR>{n}</OMBVAR>.asInstanceOf[Node])}</OME>
  }

  private def encodeDerived(om : OMDerivedObject) = om match {
    case OMForeign(a, encoding) => <OMFOREIGN encoding={encoding orNull}>{a}</OMFOREIGN>
  }

  def encode(om : OMAny) : Node = om match {
    case b : OMBasicObject => encodeBasic(b)
    case c : OMCompoundObject => encodeCompound(c)
    case d : OMDerivedObject => encodeDerived(d)
  }
}