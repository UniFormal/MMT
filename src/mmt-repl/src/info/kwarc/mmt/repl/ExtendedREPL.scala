package info.kwarc.mmt.repl

import info.kwarc.mmt.api.frontend.{REPLExtension, ShellArguments}


class ExtendedREPL extends REPLImpl with REPLExtension  {
  val completionGrammar = new ActionGrammar()

  override def enter(args: ShellArguments): Unit = {
    super.enter(args)

    if (isDumb()) {
      println(
        """
          |Unable to create a system terminal, falling back to dumb implementation.
          |This terminal does not support history or tab completion.
          |You may prefix a partial command with '@' to get a list of suggestions printed out.
        """.stripMargin)
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
    controller.handleLine(line)
  }

  def suggestions(line: String) : List[String] = {
    completionGrammar.controller = controller
    completionGrammar.action(line)
  }

  def promptLeft : Option[String] = Some(controller.currentActionDefinition match {
    case Some(name : String) => s"mmt [define $name]>"
    case None => "mmt>"
  })
  def promptRight : Option[String] = None

  def exit() : Unit = {}
}