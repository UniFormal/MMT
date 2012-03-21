package info.kwarc.mmt.api

/** The superclass of all Errors generated by MMT
 * @param msg the error message 
 */
abstract class Error(val shortMsg : String) extends java.lang.Throwable(shortMsg) {
   private var causedBy: Option[java.lang.Throwable] = None
   def setCausedBy(e: java.lang.Throwable): Error = {causedBy = Some(e); this}
   def getCausedBy : Option[java.lang.Throwable] = causedBy
   def msg : String = shortMsg + "\n" + causedBy.map("\ncaused by\n" + _.getMessage).getOrElse("")
   def stackTrace : String = getStackTrace.map(_.toString).mkString("","\n","")
}

/** errors that occur during parsing */
case class ParseError(s : String) extends Error("parse error: " + s)
/** errors that occur when adding a knowledge item */
case class AddError(s : String) extends Error("add error: " + s)
/** errors that occur when updating a knowledge item */
case class UpdateError(s : String) extends Error("update error: " + s)
/** errors that occur when deleting a knowledge item */
case class DeleteError(s : String) extends Error("delete error: " + s)
/** errors that occur when retrieving a knowledge item */
case class GetError(s : String) extends Error("get error: " + s)
/** errors that occur when checking a knowledge item (generated by the Checker classes) */
case class Invalid(s : String) extends Error("validation error: " + s)      
/** errors that occur when presenting a knowledge item */
case class PresentationError(s : String) extends Error(s)
/** errors that occur when registering extensions  */
case class ExtensionError(s : String) extends Error(s)
/** errors that are not supposed to occur, e.g., when input violates the precondition of a method */
case class ImplementationError(s : String) extends Error("implementation error: " + s)      
/** errors that occur during substitution with name of the variable for which the substitution is defined */
case class SubstitutionUndefined(name: String, m: String) extends Error("Substitution undefined at " + name + "; " + m)

// are these even used?
case class ObjError(s : String) extends Error(s)
case class LookupError(name : String) extends Error("variable " + name + " not declared in context")

