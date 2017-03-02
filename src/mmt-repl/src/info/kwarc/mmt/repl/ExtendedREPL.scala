package info.kwarc.mmt.repl

import info.kwarc.mmt.api.frontend.REPLExtension

class ExtendedREPL extends REPLImpl with REPLExtension  {

  def eval(line : String) : Boolean = {
    controller.handleLine(line)
    false
  }

  def suggestions(line: String) : List[String] = Nil

  def promptLeft : Option[String] = Some(controller.currentActionDefinition match {
    case Some(name : String) => s"mmt [define $name]>"
    case None => "mmt>"
  })
  def promptRight : Option[String] = None

  def exit() : Unit = {}
}