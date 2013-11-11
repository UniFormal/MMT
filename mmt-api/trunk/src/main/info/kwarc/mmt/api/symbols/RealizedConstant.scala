package info.kwarc.mmt.api.symbols

import info.kwarc.mmt.api._
import objects._

/** A RealizedConstant is a semantic entity whose precise structure is not accessible to the syntax
 *  but which exposes functionality that the syntax can make use of.
 */
abstract class RealizedConstant extends Declaration {
   def components = List(presentation.StringLiteral(name.toPath))
   def role = Role_Constant(None)
   def toNode = <realizedconstant name={name.toPath}/>
   override def toString = name.toString
   def getComponents = Nil
   def getDeclarations = Nil
}

/** A RealizedConstant representing the hidden realization of a type */
class RealizedTypeConstant(val home : Term, val name : LocalName, val real: objects.RealizedType) extends RealizedConstant

/** A RealizedConstant representing the hidden realization of an operator/function */
class RealizedOperatorConstant(val home : Term, val name : LocalName, val real: objects.RealizedOperator) extends RealizedConstant
