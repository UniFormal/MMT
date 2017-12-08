package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.web.RemoteAdminServer

/** Shared base class for Actions that are meta and do not actually do something */
sealed abstract class MetaAction extends ActionImpl {}

/** A simple Action that does absolutely nothing */
case object NoAction extends MetaAction {
  def apply(controller: Controller): Unit = Unit
  def toParseString = "noop"
}
object NoActionCompanion extends ActionObjectCompanionImpl[NoAction.type]("do nothing", "noop")

/**
  * run an action on a remotely administered client
  *
  * concrete syntax: remote id:STRING ACTION
  */
case class RemoteAction(id: String, action: Action) extends MetaAction {
  def apply(controller: Controller) : Unit = {
    controller.extman.get(classOf[RemoteAdminServer]).headOption match {
      case None => controller.report("error", "no admin server loaded")
      case Some(ras) => ras(this)
    }
  }
  def toParseString = s"remote $id $action"
}
object RemoteActionCompanion extends ActionCompanionImpl[RemoteAction]("run an action on a remotely administered client", "remote") {
  import Action._
  def parserActual(implicit state: ActionState) = str ~ action() ^^ {case id ~ act => RemoteAction(id, act)}
}