package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.ParseError
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.File

/** shared base class for actions defining and using procedures */
sealed abstract class DefineAction extends ActionImpl {}

/** bind all following commands to a name without executing them
  *
  * the folder of the containing msl file provides the namespace of this binding
  * concrete syntax: define name:STRING
  */
case class Define(name: String) extends DefineAction {
  def apply(controller: Controller) = controller.enterDefine(name)
  def toParseString = s"define $name"
}
object DefineCompanion extends ActionCompanionImpl[Define]("bind all following commands to a name without executing them", "define") {
  import Action._
  def parserActual(implicit state: ActionState) = str ^^ { s => Define(s) }
}

/** ends a [[Define]]
  *
  * concrete syntax: end
  */
case object EndDefine extends DefineAction {
  def apply(controller: Controller) = controller.endDefine()
  def toParseString = "end"
}
object EndDefineCompanion extends ActionObjectCompanionImpl[EndDefine.type]("ends binding commands using 'define'", "end")

/** run a previously named list of commands
  *
  * concrete syntax: do [folder:FILE] name:STRING
  * if folder is omitted, this refers to the most recently defined action with the right name
  */
case class Do(file: Option[File], name: String) extends DefineAction {
  def apply(controller: Controller) = controller.runDefinition(file, name)
  def toParseString = s"do ${file.getOrElse("")} $name"
}
object DoCompanion extends ActionCompanionImpl[Do]("run a previously named list of commands", "do"){
  import Action._
  def parserActual(implicit state: ActionState) = str ~ (file ?) ^^ { case s ~ f => Do(f, s) }
}

/** stores a command binding done with [[Define]] */
case class Defined(file: File, name: String, body: List[Action])

/** helper functions of [[DefineAction]]s */
trait DefineActionHandling {
  self: Controller =>

  /** enters a definition of the given name */
  def enterDefine(name: String) : Unit = state.currentActionDefinition match {
    case None =>
      state.currentActionDefinition = Some(Defined(state.home, name, Nil))
    case Some(_) =>
      throw ParseError("end of definition expected")
  }

  /** ends a definition */
  def endDefine(): Unit = state.currentActionDefinition match {
    case Some(a) =>
      state.actionDefinitions ::= a
      state.currentActionDefinition = None
    case None =>
      throw ParseError("no definition to end")
  }

  /** runs a given definition */
  def runDefinition(file: Option[File], name: String) {
    state.actionDefinitions.find { a => (file.isEmpty || a.file == file.get) && a.name == name } match {
      case Some(Defined(_, _, actions)) =>
        actions foreach (f => handle(f))
      case None =>
        logError("not defined")
    }
  }
}