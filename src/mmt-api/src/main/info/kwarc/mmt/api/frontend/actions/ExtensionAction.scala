package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.ontology.MathWebSearch
import info.kwarc.mmt.api.utils.URI

/** Shared base class for Actions relating to adding and removing Extensions */
sealed abstract class ExtensionAction extends ActionImpl {}

/** registers an extension
  *
  * concrete syntax: extenstion cls:CLASS args:STRING*
  *
  * @param cls  the name of a class implementing Compiler, e.g., "info.kwarc.mmt.api.lf.Twelf"
  * @param args a list of arguments that will be passed to the compiler's init method
  */
case class AddExtension(cls: String, args: List[String]) extends ExtensionAction {
  def apply(controller: Controller) = controller.extman.addExtension(cls, args)
  def toParseString = s"extension $cls${args.map(" " + _).mkString}"
}
object AddExtensionCompanion extends ActionCompanionImpl[AddExtension]("registers an extension", "extension"){
  import Action._
  def parserActual(implicit state: ActionState) = str ~ (strMaybeQuoted *) ^^ { case c ~ args => AddExtension(c, args) }
}


/** add MathWebSearch as a web service */
case class AddMWS(uri: URI) extends ExtensionAction {
  def apply(controller: Controller) = controller.extman.addExtension(new MathWebSearch(uri.toURL))
  def toParseString = s"mws $uri"
}
object AddMWSCompanion extends ActionCompanionImpl[AddMWS]("add MathWebSearch as a web service", "mws"){
  import Action._
  def parserActual(implicit state: ActionState) = uri ^^ { u => AddMWS(u) }
}