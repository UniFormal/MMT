package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.ParseError
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.File

/** shared base class for actions defining and using procedures */
sealed abstract class DefineAction extends Action {}

case class InspectDefine(name: Option[String]) extends DefineAction with ResponsiveAction {
  def apply() = name match {
    case None =>
      respond("The following definitions are known: ")

      logGroup {
        controller.getDefinitions.foreach { d =>
          respond(s"${d.name} (in ${d.file})")
        }
      }

      respond("use 'show definition <name>' to show more information about a definition. ")
    case Some(x: String) =>
      controller.getDefinition(x) match {
        case Some(d) =>
          respond(s"${d.name} (in ${d.file}): ")

          logGroup {
            d.body.foreach { a =>
              respond(a.toParseString)
            }
          }
        case None =>
          respond(s"No definition $x defined. Use 'show definition' to show a list of current definition. ")
      }
  }
  def toParseString: String = s"show definition${name.map(" " +).getOrElse("")}"
}
object InspectDefineCompanion extends ActionCompanion("inspect the set of defined actions", "show definition") {
  import Action._
  def parserActual(implicit state: ActionState) = (str?) ^^ InspectDefine
}

case class Define(name: String) extends DefineAction {
  def apply() = controller.enterDefine(name)
  def toParseString = s"define $name"
}
object DefineCompanion extends ActionCompanion("bind all following commands to a name without executing them", "define") {
  import Action._
  def parserActual(implicit state: ActionState) = str ^^ { s => Define(s) }
}

case object EndDefine extends DefineAction {
  def apply() = controller.endDefine()
  def toParseString = "end"
}
object EndDefineCompanion extends ObjectActionCompanion(EndDefine, "ends binding commands using 'define'", "end")

case class Do(file: Option[File], name: String) extends DefineAction {
  def apply() = controller.runDefinition(file, name)
  def toParseString = s"do ${file.getOrElse("")} $name"
}
object DoCompanion extends ActionCompanion("run a previously named list of commands", "do"){
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

  /** returns all known action definitions */
  def getDefinitions: List[Defined] = state.actionDefinitions
  /** return the definition of a given name */
  def getDefinition(name : String) : Option[Defined] = state.actionDefinitions.find(_.name == name)


  /** ends a definition */
  def endDefine(): Unit = state.currentActionDefinition match {
    case Some(a) =>
      state.actionDefinitions ::= a
      state.currentActionDefinition = None
    case None =>
      throw ParseError("no definition to end")
  }

  /** runs a given definition */
  def runDefinition(file: Option[File], name: String): Unit = {
    state.actionDefinitions.find { a => (file.isEmpty || a.file == file.get) && a.name == name } match {
      case Some(Defined(_, _, actions)) =>
        actions foreach (f => handle(f))
      case None =>
        logError("not defined")
    }
  }
}
