package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils._

/**
  * the state used by [[ActionCompanion]]s
  */
class ActionState(c: Controller) {
  def nsMap = c.getNamespaceMap
  def home = c.getHome
  def actionDefinitions = c.getActionDefinitions
  def currentActionDefinition = c.getCurrentActionDefinition
  def actionCompanions = c.extman.get(classOf[ActionCompanion])
}

/** the result of parsing and/or evaluation an action */
sealed abstract class ActionResult
case class ActionResultOK() extends ActionResult

sealed abstract class ActionResultError extends ActionResult {
  def error: Error
}

case class ActionParsingError(error: ParseError) extends ActionResultError
case class ActionExecutionError(error: Error) extends ActionResultError
