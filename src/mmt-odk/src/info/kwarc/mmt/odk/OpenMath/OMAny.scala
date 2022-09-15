package info.kwarc.mmt.odk.OpenMath

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils.URI

import scala.collection.{GenTraversableOnce, SeqView}
import scala.collection.mutable.ListBuffer

/**
  * Class for all OpenMath related objects
  */
sealed abstract class OMAny {
  /**
    * Resolves all CDBase attributes relative to a URI.
    *
    * @param uri URI to resolve relative to
    */
  def absolutize(uri : URI) : OMAny

  /**
    * Maps all structural components of this OpenMath object.
    *
    * @param f Function to apply. Is expected to main subtypes [[OMAnyVal]], [[OMObject]], [[OMExpression]],
    *          [[OMAttributionPairs]], [[OMSymbol]] within [[OMAttributionPairs]] and [[OMError]], [[OMVar]], [[OMBindVariables]]
    */
  def mapComponents(f : OMAny => OMAny) : OMAny

  /**
    * Extracts all components from this OpenMath object
    *
    * @return
    */
  def components : List[OMAny] = {

    // we want to cache all the components
    val buf = ListBuffer.empty[OMAny]

    // map them
    mapComponents(x =>{
      buf += x
      x
    })

    // and return the buffer
    buf.toList
  }

  /**
    * Applies a function to each component and returns a list of components.
    *
    * @param f Function to apply
    * @tparam A return type of the function.
    * @return
    */
  def foreachComponent[A](f : OMAny => A) : SeqView[A] = {
    components.view.map(f)
  }

  /**
    * Gets an OpenMath Element by an identifier.
    * @param id Id of element to find
    * @return
    */
  def getAnyById(id : String) : Option[CommonAttributes] = components.collectFirst({case x: CommonAttributes if x.id.contains(id) => x})

  /**
    * De-reference OMRefs
    * @param f Function to use for dereferencing.
    */
  def deReference(f : OMReference => OMExpression) : OMAny = mapComponents({
    case r: OMReference => f(r)
    case x => x
  })

  /**
    * De-reference OMRefs that point into the local element.
    * @return
    */
  def deReferenceLocals() : OMAny = deReference({
    case OMReference(URI(None, None, Nil, false, None, Some(s)), _) => this.getAnyById(s).asInstanceOf[OMExpression]
    case x => x
  })
}

object OMAny {
  /**
    * Absolutizes a single URI with respect to a CDBase attribute.
    *
    * @param base base uri to resolve relative for
    * @param relative Relative URI to resolve.
    * @return
    */
  def absolutizeURI(base: URI, relative: Option[URI]) = {
    relative match {
      case None => base
      case Some(x) => base.resolve(x)
    }
  }
}

/**
  * Shared Class for OpenMath Expressions + OpenMath Derived Objects
  */
sealed abstract class OMAnyVal extends OMAny {

  def absolutize(uri : URI) : OMAnyVal
  def mapComponents(f : OMAny => OMAny) : OMAnyVal

  def asExpression : OMExpression = asInstanceOf[OMExpression]
  def asReference: OMReference = asInstanceOf[OMReference]
  def asInteger : OMInteger = asInstanceOf[OMInteger]
  def asFloat : OMFloat = asInstanceOf[OMFloat]
  def asString : OMString = asInstanceOf[OMString]
  def asBytes : OMBytes = asInstanceOf[OMBytes]
  def asVariable : OMVariable = asInstanceOf[OMVariable]
  def asSymbol : OMSymbol = asInstanceOf[OMSymbol]
  def asDerived : OMDerivedElement = asInstanceOf[OMDerivedElement]
  def asForeign : OMForeign = asInstanceOf[OMForeign]
  def asCompound : OMCompoundElement = asInstanceOf[OMCompoundElement]
  def asApplication : OMApplication = asInstanceOf[OMApplication]
  def asAttribution : OMAttribution = asInstanceOf[OMAttribution]
  def asBinding : OMBinding = asInstanceOf[OMBinding]
  def asError : OMError = asInstanceOf[OMError]
}

/**
  * Represents a single OpenMath object
 *
  * @param omel OpenMath element wrapped by this object
  * @param id XML-Identifier
  * @param version OpenMath object version
  * @param cdbase CD Base URI
  */
case class OMObject(omel: OMExpression, version: Option[String], id : Option[String], cdbase : Option[URI]) extends OMAnyVal with CompoundAttributes {
  def absolutize(uri : URI) : OMObject = {
    val r = OMAny.absolutizeURI(uri, cdbase)

    OMObject(omel.absolutize(r), version, id, Some(r))
  }

  def mapComponents(f : OMAny => OMAny) : OMObject = {
    val mEl = omel.mapComponents(f)
    val mObj = OMObject(mEl, version, id, cdbase)

    f(mObj).asInstanceOf[OMObject]
  }
}

/**
  * Base class for all OpenMath Expressions (i.e. proper objects according to the specification)
  */
sealed abstract class OMExpression extends OMAnyVal {

  def absolutize(uri : URI) : OMExpression

  def mapComponents(f : OMAny => OMAny) : OMExpression

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
case class OMReference(href : URI, id : Option[String]) extends OMExpression with CommonAttributes {
  def absolutize(uri : URI) : OMReference = this

  def mapComponents(f : OMAny => OMAny) : OMExpression = {
    f(this).asInstanceOf[OMExpression]
  }

}


/**
  * trait for OpenMath Objects with a cdbase attribute
  */
sealed trait CDBaseAttribute extends OMAny {
  /**
    * The base CD of this object
    */
  val cdbase : Option[URI]
}

/**
  * Basic OpenMath objects (section 2.1.1)
  */
sealed abstract class OMBasicElement extends OMExpression {
  def absolutize(uri : URI) : OMBasicElement

  def mapComponents(f : OMAny => OMAny) : OMExpression
}

/**
  * trait for OpenMath Objects with common attributes
  */
sealed trait CommonAttributes extends OMAny {
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
case class OMInteger(int : BigInt, id : Option[String]) extends OMBasicElement with CommonAttributes {
  def absolutize(uri : URI) : OMInteger = this

  def mapComponents(f : OMAny => OMAny) : OMExpression = {
    f(this).asInstanceOf[OMExpression]
  }

}

/**
  * An OpenMath Floating Point Number
 *
  * @param dbl  Double value
  * @param id Identifier
  */
case class OMFloat(dbl : Double, id : Option[String]) extends OMBasicElement with CommonAttributes {
  def absolutize(uri : URI) : OMFloat = this

  def mapComponents(f : OMAny => OMAny) : OMExpression = {
    f(this).asInstanceOf[OMExpression]
  }

}

/**
  * An OpenMath String
 *
  * @param text String
  * @param id Identifier
  */
case class OMString(text : String, id : Option[String]) extends OMBasicElement with CommonAttributes {
  def absolutize(uri : URI) : OMString = this

  def mapComponents(f : OMAny => OMAny) : OMExpression = {
    f(this).asInstanceOf[OMExpression]
  }

}

/**
  * An OpenMath list of bytes
 *
  * @param bytes List of bytes
  * @param id Identifier
  */
case class OMBytes(bytes : List[Byte], id : Option[String]) extends OMBasicElement with CommonAttributes {
  def absolutize(uri : URI) : OMBytes = this

  def mapComponents(f : OMAny => OMAny) : OMExpression = {
    f(this).asInstanceOf[OMExpression]
  }

}

/**
  * An OpenMath Symbol
 *
  * @param name Name
  * @param cd Content Directory
  * @param cdbase CD Base URI
  * @param id Identifier
  */
case class OMSymbol(name : String, cd : String, id : Option[String], cdbase : Option[URI] ) extends OMBasicElement with CommonAttributes with CDBaseAttribute {

  def absolutize(uri : URI) : OMSymbol = {
    val r = OMAny.absolutizeURI(uri, cdbase)

    OMSymbol(name, cd, id, Some(r))
  }

  def mapComponents(f : OMAny => OMAny) : OMExpression = {
    f(this).asInstanceOf[OMExpression]
  }

  /**
    * Checks if this symbol semantically points to the same OMSymbol
    *
    * @param other other Symbol to check.
    */
  def === (other : OMSymbol): Boolean = {
    (other.name == name) && (other.cdbase.map(_.toString).getOrElse("") + cd == cdbase.map(_.toString).getOrElse("") + cd)
  }
}

object OMSymbol {
  def apply(p: GlobalName): OMSymbol = OMSymbol(p.name.toPath, p.module.name.toPath, None, None)// Some(p.doc.uri)) 
}

/**
  * An OpenMath Variable
 *
  * @param name Name
  * @param id Identifier
  */
case class OMVariable(name : String, id : Option[String]) extends OMBasicElement with CommonAttributes {
  def absolutize(uri : URI) : OMVariable = this

  def mapComponents(f : OMAny => OMAny) : OMExpression = {
    f(this).asInstanceOf[OMExpression]
  }

}

/**
  * Derived OpenMath objects (section 2.1.2)
  */
sealed abstract class OMDerivedElement extends OMAnyVal {
  def absolutize(uri : URI) : OMDerivedElement

  def mapComponents(f : OMAny => OMAny) : OMAnyVal
}

/**
  * An OpenMath Foreign Object
 *
  * @param obj Foreign Object
  * @param encoding Encoding
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class OMForeign(obj : Any, encoding : Option[String], id : Option[String], cdbase : Option[URI]) extends OMDerivedElement with CompoundAttributes {
  def absolutize(uri : URI) : OMForeign = {
    val r = OMAny.absolutizeURI(uri, cdbase)

    OMForeign(obj, encoding, id, Some(r))
  }

  def mapComponents(f : OMAny => OMAny) : OMAnyVal = {
    f(this).asInstanceOf[OMAnyVal]
  }

}

/**
  * trait for OpenMath Objects with compund attributes
  */
sealed trait CompoundAttributes extends OMAny with CommonAttributes with CDBaseAttribute

/**
  * Compound OpenMath objects (section 2.1.3)
  */
sealed abstract class OMCompoundElement extends OMExpression {
  def absolutize(uri : URI) : OMCompoundElement

  def mapComponents(f : OMAny => OMAny) : OMExpression
}

/**
  * An OpenMath Application Object
 *
  * @param elem OpenMath element to apply
  * @param arguments Arguments to apply elem to
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class OMApplication( elem : OMExpression, arguments : List[OMExpression], id : Option[String], cdbase : Option[URI]) extends OMCompoundElement with CompoundAttributes {
  def absolutize(uri : URI) : OMApplication = {
    val r = OMAny.absolutizeURI(uri, cdbase)

    val mElem = elem.absolutize(r)
    val mArguments = arguments.map(_.absolutize(r))

    OMApplication(mElem, mArguments, id, Some(r))
  }

  def mapComponents(f : OMAny => OMAny) : OMExpression = {
    val mElem = elem.mapComponents(f)
    val mArgs = arguments.map(_.mapComponents(f))

    val mApps = OMApplication(mElem, mArgs, id, cdbase)
    f(mApps).asInstanceOf[OMExpression]
  }

}


/**
  * An OpenMath Attribution
 *
  * @param pairs  List of pairs that are attributed
  * @param A object to attribute to
  * @param id     Idenfitier
  * @param cdbase CD Base URI
  */
case class OMAttribution(pairs: OMAttributionPairs, A: OMExpression, id: Option[String], cdbase: Option[URI]) extends OMCompoundElement with CompoundAttributes {
  def absolutize(uri : URI) : OMAttribution = {
    val r = OMAny.absolutizeURI(uri, cdbase)

    val mPairs = pairs.absolutize(r)
    val mA = A.absolutize(r)

    OMAttribution(mPairs, mA, id, Some(r))
  }

  def mapComponents(f : OMAny => OMAny) : OMExpression = {
    val mPairs = pairs.mapComponents(f)
    val mA = A.mapComponents(f)

    val mAttribution = OMAttribution(mPairs, mA, id, cdbase)

    f(mAttribution).asInstanceOf[OMExpression]
  }

}

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

  def absolutize(uri : URI) : OMAttributionPairs = {
    val r = OMAny.absolutizeURI(uri, cdbase)

    val mPairs = pairs.map({
      case (k, v) => (k.absolutize(r), v.absolutize(r))
    })

    OMAttributionPairs(mPairs, id, Some(r))
  }

  def mapComponents(f : OMAny => OMAny) : OMAttributionPairs = {
    val mPairs = pairs.map({
      case (k, v) => (k.mapComponents(f).asInstanceOf[OMSymbol], v.mapComponents(f))
    })

    val mAttPairs = OMAttributionPairs(mPairs, id, cdbase)
    f(mAttPairs).asInstanceOf[OMAttributionPairs]
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
  * @param C Bound Term
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class  OMBinding(B: OMExpression, vars: OMBindVariables, C: OMExpression, id: Option[String], cdbase: Option[URI]) extends OMCompoundElement with CompoundAttributes {
  def absolutize(uri : URI) : OMBinding = {
    val r = OMAny.absolutizeURI(uri, cdbase)

    val mB = B.absolutize(r)
    val mVars = vars.absolutize(r)
    val mC = C.absolutize(r)

    OMBinding(mB, mVars, mC, id, Some(r))
  }

  def mapComponents(f : OMAny => OMAny) : OMExpression = {
    val mB = B.mapComponents(f)
    val mVars = vars.mapComponents(f)
    val mC = C.mapComponents(f)

    val mBinding = OMBinding(mB, mVars, mC, id, cdbase)
    f(mBinding).asInstanceOf[OMExpression]
  }

}

/**
  * Represents a Variable or Attributed Variable
  */
sealed abstract class OMVar extends OMAny {
  /**
    * Turns this into a standard OpenMath Expression
 *
    * @return
    */
  def name : String
  def toExpression : OMExpression

  def mapComponents(f : OMAny => OMAny) : OMVar

  def absolutize(uri : URI) : OMVar
}

object OMVar {
  /**
    * Turns an OpenMath Expression representing a Variable or AttributedVariable
    * into an OMVar representation
 *
    * @param expr Expression
    * @return
    */
  def fromExpression(expr : OMExpression) : OMVar = expr match {
    case omv : OMVariable => OMVarVar(omv)
    case OMAttribution(pairs, a, id, cdbase) =>
      OMAttVar(pairs, fromExpression(a), id)
    case _ => throw ImplementationError("non-variable passed to fromExpression()")
  }
}

/**
  * Represents a single bound variable
 *
  * @param omv Variable to be bound
  */
case class OMVarVar(omv : OMVariable) extends OMVar {
  def toExpression : OMExpression = omv

  def absolutize(uri : URI) : OMVarVar = {
    OMVarVar(omv.absolutize(uri))
  }

  def mapComponents(f : OMAny => OMAny) : OMVar = {
    val momv = omv.mapComponents(f)

    val mvarvar = OMVarVar(omv)
    f(mvarvar).asInstanceOf[OMVar]
  }

  def name = omv.name
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

  def absolutize(uri : URI) : OMAttVar = {
    val mPairs = pairs.absolutize(uri)
    val mA = A.absolutize(uri)

    OMAttVar(mPairs, mA, id)
  }

  def mapComponents(f : OMAny => OMAny) : OMVar = {
    val mPairs = pairs.mapComponents(f)
    val mA = A.mapComponents(f)

    val mAttVar = OMAttVar(mPairs, mA, id)
    f(mAttVar).asInstanceOf[OMVar]
  }

  def name = A.name
}

/**
  * List of OpenMath bound variables
 *
  * @param vars List of bound variables
  * @param id Identifier
  */
case class OMBindVariables(vars : List[OMVar], id : Option[String]) extends OMAny with CommonAttributes {
  def absolutize(uri : URI) : OMBindVariables = {
    val mVars = vars.map(_.absolutize(uri))

    OMBindVariables(mVars, id)
  }

  def mapComponents(f : OMAny => OMAny) : OMBindVariables = {
    val mVars = vars.map(_.mapComponents(f))

    val mBindVariables = OMBindVariables(mVars, id)
    f(mBindVariables).asInstanceOf[OMBindVariables]
  }

}

/**
  * An OpenMath Error
 *
  * @param name Error name
  * @param params Parameters to the error
  * @param id Identifier
  * @param cdbase CD Base URI
  */
case class OMError(name : OMSymbol, params: List[OMAnyVal], id : Option[String], cdbase : Option[URI]) extends OMCompoundElement with CompoundAttributes {
  def absolutize(uri : URI) : OMError = {

    val r = OMAny.absolutizeURI(uri, cdbase)

    val mName = name.absolutize(r)
    val mParams = params.map(_.absolutize(r))

    OMError(mName, mParams, id, Some(r))
  }

  def mapComponents(f : OMAny => OMAny) : OMExpression = {
    val mName = name.mapComponents(f).asInstanceOf[OMSymbol]
    val mParams = params.map(_.mapComponents(f))

    val mError = OMError(mName, mParams, id, cdbase)
    f(mError).asInstanceOf[OMExpression]
  }

}
