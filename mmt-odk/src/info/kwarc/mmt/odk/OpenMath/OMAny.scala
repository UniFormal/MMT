package info.kwarc.mmt.odk.OpenMath
import java.net.URI

/**
  * Class for all OpenMath related objects
  */
sealed abstract class OMAny

/**
  * Shared Class for OpenMath Expressions + OpenMath Derived Objects
  */
sealed abstract class OMAnyVal extends OMAny {

  private def as[T <: OMAnyVal] : T = this match {case t: T => t}

  def asExpression : OMExpression = as
  def asReference: OMReference = as
  def asInteger : OMInteger = as
  def asFloat : OMFloat = as
  def asString : OMString = as
  def asBytes : OMBytes = as
  def asVariable : OMVariable = as
  def asSymbol : OMSymbol = as
  def asDerived : OMDerivedElement = as
  def asForeign : OMForeign = as
  def asCompound : OMCompoundElement = as
  def asApplication : OMApplication = as
  def asAttribution : OMAttribution = as
  def asBinding : OMBinding = as
  def asError : OMError = as
}

/**
  * Represents a single OpenMath object
 *
  * @param omel OpenMath element wrapped by this object
  * @param id Identifier
  * @param version OpenMath object version
  * @param cdbase CD Base URI
  */
case class OMObject(omel: OMExpression, version: Option[String], id : Option[String], cdbase : Option[URI]) extends OMAnyVal with CompoundAttributes

/**
  * Base class for all OpenMath Expressions (i.e. proper objects according to the specification)
  */
sealed abstract class OMExpression extends OMAnyVal {

  /**
    * Applies this OpenMath Expression to a list of other expressions
 *
    * @param id Optional. Identifier,
    * @param cdbase Optional. CD Base URI.
    * @param arguments Arguments to pass to the application
    * @return
    */
  def apply(id : Option[String], cdbase : Option[URI], arguments: OMExpression*) = OMApplication(this, arguments.toList, id, cdbase)

  def apply(id : Option[String], arguments: OMExpression*) : OMApplication = apply(None, None, arguments:_*)
  def apply(arguments: OMExpression*) : OMApplication = apply(None, arguments:_*)
}

/**
  * An OpenMath reference
  *
  * @param href Element that is refered
  * @param id Idenfitier
  */
case class OMReference(href : URI, id : Option[String]) extends OMExpression with CommonAttributes


/**
  * trait for OpenMath Objects with a cdbase attribute
  */
sealed trait CDBaseAttribute {
  /**
    * The base CD of this object
    */
  val cdbase : Option[URI]
}

/**
  * Basic OpenMath objects (section 2.1.1)
  */
sealed abstract class OMBasicElement extends OMExpression

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
 *
  * @param int Integer value
  * @param id Identifier
  */
case class OMInteger(int : BigInt, id : Option[String]) extends OMBasicElement with CommonAttributes

/**
  * An OpenMath Floating Point Number
 *
  * @param dbl  Double value
  * @param id Identifier
  */
case class OMFloat(dbl : Double, id : Option[String]) extends OMBasicElement with CommonAttributes

/**
  * An OpenMath String
 *
  * @param text String
  * @param id Identifier
  */
case class OMString(text : String, id : Option[String]) extends OMBasicElement with CommonAttributes

/**
  * An OpenMath list of bytes
 *
  * @param bytes List of bytes
  * @param id Identifier
  */
case class OMBytes(bytes : List[Byte], id : Option[String]) extends OMBasicElement with CommonAttributes

/**
  * An OpenMath Symbol
 *
  * @param name Name
  * @param cd Content Directory
  * @param cdbase CD Base URI
  * @param id Identifier
  */
case class OMSymbol(name : String, cd : String, id : Option[String], cdbase : Option[URI] ) extends OMBasicElement with CommonAttributes with CDBaseAttribute {
  /**
    * Checks if this symbol semantically points to the same OMSymbol
    *
    * @param other
    */
  def === (other : OMSymbol): Boolean = {
    (other.name == name) && (other.cdbase.map(_.toString).getOrElse("") + cd == cdbase.map(_.toString).getOrElse("") + cd)
  }
}

/**
  * An OpenMath Variable
 *
  * @param name Name
  * @param id Identifier
  */
case class OMVariable(name : String, id : Option[String]) extends OMBasicElement with CommonAttributes

/**
  * Derived OpenMath objects (section 2.1.2)
  */
sealed abstract class OMDerivedElement extends OMAnyVal

/**
  * An OpenMath Foreign Object
 *
  * @param obj Foreign Object
  * @param encoding Encoding
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class OMForeign(obj : Any, encoding : Option[String], id : Option[String], cdbase : Option[URI]) extends OMDerivedElement with CompoundAttributes

/**
  * trait for OpenMath Objects with compund attributes
  */
sealed trait CompoundAttributes extends CommonAttributes with CDBaseAttribute

/**
  * Compound OpenMath objects (section 2.1.3)
  */
sealed abstract class OMCompoundElement extends OMExpression

/**
  * An OpenMath Application Object
 *
  * @param elem OpenMath element to apply
  * @param arguments Arguments to apply elem to
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class OMApplication( elem : OMExpression, arguments : List[OMExpression], id : Option[String], cdbase : Option[URI]) extends OMCompoundElement with CompoundAttributes


/**
  * An OpenMath Attribution
 *
  * @param pairs  List of pairs that are attributed
  * @param A object to attribute to
  * @param id     Idenfitier
  * @param cdbase CD Base URI
  */
case class OMAttribution(pairs: OMAttributionPairs, A: OMExpression, id: Option[String], cdbase: Option[URI]) extends OMCompoundElement with CompoundAttributes

/**
  * List of Attribution pairs
 *
  * @param pairs Pairs to that are actually attributed
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class OMAttributionPairs(pairs : List[(OMSymbol, OMAnyVal)], id : Option[String], cdbase : Option[URI]) extends OMAny with CompoundAttributes {

  if(pairs.isEmpty){
    throw new Exception("Malformed OpenMath: OMAttributionPairs must have at least one attributed variable. ")
  }

  /**
    * The keys of these OMAttributionPairs
    */
  val keys : List[OMSymbol] = pairs.map(_._1)

  /**
    * Returns the value of a given key
    */
  def apply(key : OMSymbol) : Option[OMAnyVal] = pairs.filter(_._1 === key) match {
    case Nil => None
    case h :: _ => Some(h._2)
  }

  /**
    * Turns this OMAttributionPair to a map
 *
    * @return
    */
  def toMap : Map[OMSymbol, OMAnyVal] = pairs.toMap
}

/**
  * An OpenMath Binding
  *
  * @param B Binding object
  * @param vars variables to bind
  * @param C
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class OMBinding(B: OMExpression, vars: OMBindVariables, C: OMExpression, id: Option[String], cdbase: Option[URI]) extends OMCompoundElement with CommonAttributes

/**
  * Represents a Variable or Attributed Variable
  */
sealed abstract class OMVar extends OMAny {
  /**
    * Turns this into a standard OpenMath Expression
 *
    * @return
    */
  def toExpression : OMExpression
}

object OMVar {
  /**
    * Turns an OpenMath Expression representing a Variable or AttributedVariable
    * into an OMVar representation
 *
    * @param expr
    * @return
    */
  def fromExpression(expr : OMExpression) : OMVar = expr match {
    case omv : OMVariable => OMVarVar(omv)
    case OMAttribution(pairs, a, id, cdbase) =>
      OMAttVar(pairs, fromExpression(a), id)
  }
}

/**
  * Represents a single bound variable
 *
  * @param omv Variable to be bound
  */
case class OMVarVar(omv : OMVariable) extends OMVar {
  def toExpression : OMExpression = omv
}

/**
  * Represents an attributed variable
 *
  * @param pairs Pairs of attributed variables
  * @param A further list of variable that is bound
  * @param id Identifier
  */
case class OMAttVar(pairs: OMAttributionPairs, A : OMVar, id : Option[String]) extends OMVar with CommonAttributes {
  def toExpression : OMExpression = OMAttribution(pairs, A.toExpression, id, None)
}

/**
  * List of OpenMath bound variables
 *
  * @param vars List of bound variables
  * @param id Identifier
  */
case class OMBindVariables(vars : List[OMVar], id : Option[String]) extends OMAny with CommonAttributes

/**
  * An OpenMath Error
 *
  * @param name Error name
  * @param params Parameters to the error
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class OMError(name : OMSymbol, params: List[OMAnyVal], id : Option[String], cdbase : Option[URI]) extends OMCompoundElement with CompoundAttributes