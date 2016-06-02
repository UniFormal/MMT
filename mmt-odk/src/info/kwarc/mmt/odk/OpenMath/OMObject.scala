package info.kwarc.mmt.odk.OpenMath

/**
  * Shared base class for all OpenMath objects + parts
  */
sealed abstract class OMAny

/**
  * Shared base class for all stand-alone meanigful OpenMath objects
  */
sealed abstract class OMNode extends OMAny

/**
  * Represents an OpenMath object
  * @param omel OpenMath element wrapped by this object
  * @param id Identifier
  * @param version OpenMath object version
  * @param cdbase CD Base URI
  */
case class OMObject(omel : OMElement, version: Option[String], id : Option[String], cdbase : Option[String]) extends OMNode with CompoundAttributes

/**
  * Shared base class for all OpenMath object values
  */
sealed abstract class OMElement extends OMNode

/**
  * An OpenMath Reference
  * @param href Element that is refered
  * @param id Idenfitier
  */
case class OMReference(href : String, id : Option[String]) extends OMElement with CommonAttributes


/**
  * trait for OpenMath Objects with a cdbase attribute
  */
sealed trait CDBaseAttribute {
  /**
    * The base CD of this object
    */
  val cdbase : Option[String]
}

/**
  * Basic OpenMath objects
  */
sealed abstract class OMBasicElement extends OMElement

/**
  * trait for OpenMath Objects with common attributes
  */
sealed trait CommonAttributes {
  /**
    * the id of this object
    */
  val id : Option[String]
}

/**
  * An OpenMath integer
  * @param int Integer value
  * @param id Identifier
  */
case class OMInteger(int : BigInt, id : Option[String]) extends OMBasicElement with CommonAttributes

/**
  * An OpenMath Floating Point Number
  * @param dbl  Double value
  * @param id Identifier
  */
case class OMFloat(dbl : Double, id : Option[String]) extends OMBasicElement with CommonAttributes

/**
  * An OpenMath String
  * @param text String
  * @param id Identifier
  */
case class OMString(text : String, id : Option[String]) extends OMBasicElement with CommonAttributes

/**
  * An OpenMath list of bytes
  * @param bytes List of bytes
  * @param id Identifier
  */
case class OMBytes(bytes : List[Byte], id : Option[String]) extends OMBasicElement with CommonAttributes

/**
  * An OpenMath Variable
  * @param name Name
  * @param id Identifier
  */
case class OMVariable(name : String, id : Option[String]) extends OMBasicElement with CommonAttributes

/**
  * An OpenMath Symbol
  * @param name Name
  * @param cd Content Directory
  * @param cdbase CD Base URI
  * @param id Identifier
  */
case class OMSymbol(name : String, cd : String, id : Option[String], cdbase : Option[String] ) extends OMBasicElement with CommonAttributes with CDBaseAttribute

/**
  * Derived OpenMath objects
  */
sealed abstract class OMDerivedElement extends OMNode

/**
  * An OpenMath Foreign Object
  * @param obj Foreign Object
  * @param encoding Encoding
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class OMForeign(obj : Any, encoding : Option[String], id : Option[String], cdbase : Option[String]) extends OMDerivedElement with CompoundAttributes

/**
  * trait for OpenMath Objects with compund attributes
  */
sealed trait CompoundAttributes extends CommonAttributes with CDBaseAttribute

/**
  * Compound OpenMath objects
  */
sealed abstract class OMCompoundElement extends OMElement

/**
  * An OpenMath Application Object
  * @param elem OpenMath element to apply
  * @param arguments Arguments to apply elem to
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class OMApplication( elem : OMElement, arguments : List[OMElement], id : Option[String], cdbase : Option[String]) extends OMCompoundElement with CompoundAttributes


/**
  * An OpenMath Attribution
  * @param pairs List of pairs that are attributed
  * @param obj object to attribute to
  * @param id Idenfitier
  * @param cdbase CD Base URI
  */
case class OMAttribution(pairs : OMAttributionPairs, obj : OMElement, id : Option[String], cdbase : Option[String]) extends OMCompoundElement with CompoundAttributes

/**
  * List of Attribution pairs
  * @param pairs Pairs to that are actually attributed
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class OMAttributionPairs(pairs : List[(OMSymbol, OMNode)], id : Option[String], cdbase : Option[String]) extends OMAny with CompoundAttributes


/**
  * Represents a list of bound variables
  */
sealed abstract class OMVar extends OMAny

/**
  * Represents a single bound variable
  * @param omv Variable to be bound
  */
case class OMVarVar(omv : OMVariable) extends OMVar

/**
  * Reprents an attributed variable
  * @param pairs Pairs of arrtirbutes variables
  * @param value further list of variable that is bound
  * @param id Identifier
  */
case class OMAttVar(pairs: OMAttributionPairs, value : OMVar, id : Option[String]) extends OMVar with CommonAttributes

/**
  * List of OpenMath bound variables
  * @param vars List of bound variables
  * @param id Identifier
  */
case class OMBindVariables(vars : List[OMVar], id : Option[String]) extends OMAny with CommonAttributes


/**
  * An OpenMath Binding
  * @param A
  * @param vars variables to bind
  * @param C
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class OMBinding(A : OMElement, vars: OMBindVariables, C : OMElement, id : Option[String], cdbase : Option[String]) extends OMCompoundElement with CommonAttributes


/**
  * An OpenMath Error
  * @param name Error name
  * @param params Parameters to the error
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class OMError(name : OMSymbol, params : List[OMNode], id : Option[String], cdbase : Option[String]) extends OMCompoundElement with CompoundAttributes