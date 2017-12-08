package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.{Error, NamespaceMap, ParseError, Path}
import info.kwarc.mmt.api.frontend.{Controller, ControllerState, Report}
import info.kwarc.mmt.api.utils.File

/**
  * The State used by [[ActionCompanion]]s
  */
abstract class ActionState {
  def nsMap: NamespaceMap
  def home: File
  def actionDefinitions: List[Defined]
  def currentActionDefinition: Option[Defined]
}

object ActionState {
  /** Creates a new [[ActionState]] given a [[ControllerState]] */
  def apply(cs: ControllerState): ActionState = new ActionState {
    def nsMap: NamespaceMap = cs.nsMap
    def home: File = cs.home
    def actionDefinitions: List[Defined] = cs.actionDefinitions
    def currentActionDefinition: Option[Defined] = cs.currentActionDefinition
  }
  /** Creates a new [[ActionState]] given a NamespaceMap and a Home */
  def apply(nMap: NamespaceMap, hme: File): ActionState = new ActionState {
    val nsMap: NamespaceMap = nMap
    val home: File = hme
    val actionDefinitions: List[Defined] = List()
    val currentActionDefinition: Option[Defined] = None
  }
  /** Creates a new, default, [[ActionState]] */
  def apply() : ActionState = apply(NamespaceMap.empty, File(System.getProperty("user.dir")))
}

/** the result of parsing and/or evaluation an action */
sealed abstract class ActionResult
case class ActionResultOK() extends ActionResult

sealed abstract class ActionResultError extends ActionResult {
  def error: Error
}

case class ActionParsingError(error: ParseError) extends ActionResultError
case class ActionExecutionError(error: Error) extends ActionResultError