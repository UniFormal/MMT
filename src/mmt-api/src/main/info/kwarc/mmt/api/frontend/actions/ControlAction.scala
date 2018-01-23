package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.frontend.Controller

/** shared base class for actions controlling the shell or other actions */
sealed abstract class ControlAction extends ActionImpl

case object Clear extends ControlAction {
  def apply(implicit controller: Controller): Unit = {
    controller.clear
  }
  def toParseString = "clear"
}
object ClearCompanion extends ActionObjectCompanionImpl[Clear.type]("clear the current state of the controller", "clear")

/** release all resources and exit
  *
  * concrete syntax: exit
  */
case object Exit extends ControlAction {
  def apply(implicit controller: Controller): Unit = {
    controller.cleanup
    sys.exit()
  }
  def toParseString = "exit"
}
object ExitCompanion extends ActionObjectCompanionImpl[Exit.type]("release all resources and exit MMT", "exit") {
  def parse(implicit state: ActionState) = Exit
}

case class SetBase(base: Path) extends ControlAction {
  def apply(implicit controller: Controller): Unit = controller.setBase(base)
  def toParseString = s"base $base"
}
object SetBaseCompanion extends ActionCompanionImpl[SetBase]("set the current base path", "base") {
  import Action._
  def parserActual(implicit state: ActionState) = path ^^ { p => SetBase(p) }
}

/** Implements handling of [[ControlAction]]s */
trait ControlActionHandling {
  self: Controller =>

  /** set the base path of this [[Controller]] */
  def setBase(base: Path): Unit = {
    state.nsMap = state.nsMap(base)
    report("response", "base: " + getBase)
  }
}
