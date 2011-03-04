package hol

/**
 * simple type theory
 */
abstract class Name
case class SimpleName(name : String) extends Name
case class PolymorphicName(name : String, tp : Type) extends Name //needed for forall, exists, equal

class Signature(types : List[String], cons : List[(String, Type)]) {
   def isType(name : String) : Boolean = {false} //TODO
   def getType(name : Name) : Option[Type] = {None} //TODO
   def wf : Boolean = {false} //TODO
}

class Context(vars : List[(String, Type)]) {
   def getType(name : String) : Option[Type] = {None} //TODO
   def wf(sig : Signature) : Boolean = {false} //TODO
}

class Substitution(maps : List[(String, Term)]) {
   def getMap(name : String) : Option[Term] = {None} //TODO
   def wf(sig : Signature, dom : Context, cod : Context) : Boolean = {false} //TODO
   def apply(t : Term) : Term = {null} //TODO (you may ignore variable capture)
}

abstract class Type {
   def wf(sig : Signature) : Boolean = {false} //TODO
}
case class BaseType(name : String) extends Type
//TO DO: other types

abstract class Term {
   def wf(sig : Signature, dom : Context) : Boolean = {inferType(sig, dom) != None}
   def inferType(sig : Signature, dom : Context) : Option[Type] = {None} //TODO
}
case class Constant(name : Name) extends Term
//TO DO: other terms

/**
 * extension to higher-order logic
 */

class HOLSignature(types : List[String], cons : List[(String, Type)]) extends Signature(types, cons) {
   override def isType(name : String) : Boolean = {name == "o" || super.isType(name)}
   override def getType(name : Name) : Option[Type] = {None} //TODO
}

class HOLTerm(val term : Term) {
   def isForm : Boolean = {false} //TODO
   def expandAbbrevs : HOLTerm = {null} //TODO
}