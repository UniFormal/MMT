package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.parser.SourceRef

/** shared base class for actions controlling the shell or other actions */
sealed abstract class ControlAction extends Action

case object Clear extends ControlAction {
  def apply() {
    controller.clear
  }
  def toParseString = "clear"
}
object ClearCompanion extends ObjectActionCompanion(Clear, "clear the current state of the controller", "clear")

/** release all resources and exit
  *
  * concrete syntax: exit
  */
case object Exit extends ControlAction {
  def apply() {
    controller.cleanup
    sys.exit()
  }
  def toParseString = "exit"
}
object ExitCompanion extends ObjectActionCompanion(Exit, "release all resources and exit MMT", "exit")

case object NoAction extends ControlAction {
  def apply() {}
  def toParseString = "noop"
}
object NoActionCompanion extends ObjectActionCompanion(NoAction, "do nothing", "noop")

case class SetBase(base: Path) extends ControlAction {
  def apply() =controller.setBase(base)
  def toParseString = s"base $base"
}
object SetBaseCompanion extends ActionCompanion("set the current base path", "base") {
  import Action._
  def parserActual(implicit state: ActionState) = path ^^ { p => SetBase(p) }
}

case class Navigate(p: Path) extends ControlAction {
  def apply() {controller.navigate(p)}
  def toParseString = s"navigate $p"
}
object NavigateCompanion extends ActionCompanion("navigate to knowledge item", "navigate") {
  import Action._
  def parserActual(implicit state: ActionState) = path ^^ { p => Navigate(p) }
}

case class NavigateSource(ref: SourceRef) extends ControlAction {
  def apply() {controller.navigateSource(ref)}
  def toParseString = s"navigateSource $ref"
}
object NavigateSourceCompanion extends ActionCompanion("navigate to physical location", "navigateSource") {
  import Action._
  def parserActual(implicit state: ActionState) = str ^^ { s => NavigateSource(SourceRef.fromString(s)) }
}


/** Implements handling of [[ControlAction]]s */
trait ControlActionHandling {
  self: Controller =>

  /** set the base path of this [[Controller]] */
  def setBase(base: Path): Unit = {
    state.nsMap = state.nsMap(base)
    report("response", "base: " + getBase)
  }

  /** navigates to a given path, handling [[Navigate]] */
  def navigate(p: Path): Unit = {
    notifyListeners.onNavigate(p)
  }
  /** navigates to a given path, handling [[Navigate]] */
  def navigateSource(r: SourceRef): Unit = {
    notifyListeners.onNavigateSource(r)
  }
}
