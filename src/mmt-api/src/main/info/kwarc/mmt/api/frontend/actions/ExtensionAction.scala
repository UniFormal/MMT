package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.frontend.{Controller, Extension, ExtensionManager}
import info.kwarc.mmt.api.ontology.MathWebSearch
import info.kwarc.mmt.api.utils.URI

/** Shared base class for Actions relating to adding and removing Extensions */
sealed abstract class ExtensionAction extends Action {
  protected def extman = controller.extman
}

case object ListExtensions extends ExtensionAction with ResponsiveAction {
  def apply(): Unit = {
    respond("the following extensions are active: ")

    logGroup {
      extman.extensions.foreach { e =>
        respond(e.getClass.getName)
      }
    }

    respond("Use 'extension <name> <args...>' to add an extension. ")
    respond("Use 'unload <name>' to remove an extension. ")
  }
  def toParseString = s"show extensions"
}
object ListExtensionsCompanion extends ObjectActionCompanion(ListExtensions, "list all extensions", "show extensions")

case class AddExtension(cls: String, args: List[String]) extends ExtensionAction {
  def apply(): Unit = {extman.addExtension(cls, args)}
  def toParseString = s"extension $cls${args.map(" " + _).mkString}"
}
object AddExtensionCompanion extends ActionCompanion("registers an extension", "extension"){
  import Action._
  def parserActual(implicit state: ActionState) = str ~ (strMaybeQuoted *) ^^ { case c ~ args => AddExtension(c, args) }
}

case class RemoveExtension(cls: String) extends ExtensionAction {
  def apply(): Unit = {extman.extensions.foreach {
    case e: Extension if e.getClass.getName == cls => extman.removeExtension(e)
  }}
  def toParseString = s"unload $cls"
}
object RemoveExtensionCompanion extends ActionCompanion("remove an extension", "unload"){
  import Action._
  def parserActual(implicit state: ActionState) = str ^^ RemoveExtension
}


case class AddMWS(uri: URI) extends ExtensionAction {
  def apply(): Unit = {extman.addExtension(new MathWebSearch(uri.toURL))}
  def toParseString = s"mws $uri"
}
object AddMWSCompanion extends ActionCompanion("add MathWebSearch as a web service", "mws"){
  import Action._
  def parserActual(implicit state: ActionState) = uri ^^ { u => AddMWS(u) }
}
