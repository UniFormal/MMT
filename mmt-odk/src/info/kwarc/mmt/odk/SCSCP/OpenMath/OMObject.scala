package info.kwarc.mmt.odk.SCSCP.OpenMath

abstract class OMAny
abstract class OMObject extends OMAny

// basic OpenMath objects (section 2.1.1)
abstract class OMBasicObject extends OMObject

case class OMInteger(int : BigInt) extends OMBasicObject
case class OMFloat(double : Double) extends OMBasicObject
case class OMString(chars : String) extends OMBasicObject
case class OMByteArray(bytes : Seq[Byte]) extends OMBasicObject
case class OMSymbol(name : String, cd : String, base : Option[String] = None) extends OMBasicObject
case class OMVariable(name : String) extends OMBasicObject

// derived OpenMath objects (section 2.1.2)

abstract class OMDerivedObject extends OMAny

case class OMForeign(A : Any, encoding : Option[String]) extends OMDerivedObject

// OpenMath compound objects (section 2.1.3)
class OMCompoundObject extends OMObject

case class OMApplication(A : List[OMObject]) extends OMCompoundObject {
  if(A.isEmpty) {
    throw new Error("Application takes at least one Object")
  }
}
case class OMAttribution(A : OMObject, AS : List[(OMSymbol, OMAny)]) extends OMCompoundObject {

  if(AS.isEmpty) {
    throw new Error("Attribution takes at least one Symbol and Object or DerivedObject")
  }

  def apply(s : OMSymbol) : List[OMAny] = AS.filter(_._1 == s).map(_._2)

  val values = AS.map(_._1)
  val keys = AS.map(_._2)
}


case class OMBinding(B : OMObject, v : List[OMVariable], C: OMObject) extends OMCompoundObject
case class OMError(S : OMSymbol, AS : List[OMAny]) extends OMCompoundObject
