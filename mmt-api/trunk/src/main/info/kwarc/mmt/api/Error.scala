package info.kwarc.mmt.api

abstract class Error(val msg : String) extends java.lang.Throwable(msg)

case class ParseError(s : String) extends Error("parse error: " + s)
case class AddError(s : String) extends Error("add error: " + s)
case class DeleteError(s : String) extends Error("delete error: " + s)
case class GetError(s : String) extends Error("get error: " + s)
case class ObjError(s : String) extends Error(s)
case class Invalid(s : String) extends Error("validation error: " + s)      
case class PresentationError(s : String) extends Error(s)
case class ImplementationError(s : String) extends Error("implementation error: " + s)      

case class LookupError(name : String) extends Error("variable " + name + " not declared in context")
case class SubstitutionUndefined(name: String) extends Error("Substitution undefined at " + name)
