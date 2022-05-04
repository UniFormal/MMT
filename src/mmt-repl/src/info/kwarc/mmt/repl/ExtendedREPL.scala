package info.kwarc.mmt.repl

import info.kwarc.mmt.api.frontend._
import actions._


class ExtendedREPL extends REPLImpl with REPLExtension  {
  override def enter(args: ShellArguments) {
    super.enter(args)
    if (isDumb()) {
      println("Unable to create a system terminal. No history or tab completion; prefix a partial command with '@' to see suggestions")
    }
  }

  def eval(line: String): Boolean = {
    if (line.startsWith("@")) {
      printSuggestions(line.substring(1))
    } else {
      handleLine(line)
    }
    false
  }

  private def printSuggestions(line: String) = {
    suggestions(line).foreach(println)
  }
  private def handleLine(line: String) = {
    controller.tryHandleLine(line) match {
      case ActionResultOK() =>
      case ae: ActionResultError =>
        report(ae.error)
        // FR: this used to be as below, but it's wrong to suppress the stack trace
        //println("\u001b[31;1m" + ae.error + "\u001b[0m")
    }
    true
  }

  def suggestions(line: String) : List[String] = Action.completeAct(controller, line)

  def promptLeft : Option[String] = Some(controller.getCurrentActionDefinition match {
    case Some(name : String) => s"mmt [define $name]>"
    case None => "mmt>"
  })
  def promptRight : Option[String] = None

  def exit() : Unit = {}
}
