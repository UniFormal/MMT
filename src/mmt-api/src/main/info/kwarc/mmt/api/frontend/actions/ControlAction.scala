package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.parser.SourceRef

/** shared base class for actions controlling the shell or other actions */
sealed abstract class ControlAction extends Action

case object Clear extends ControlAction {
  def apply(): Unit = {
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
  def apply(): Unit = {
    controller.cleanup
    sys.exit()
  }
  def toParseString = "exit"
}
object ExitCompanion extends ObjectActionCompanion(Exit, "release all resources and exit MMT", "exit")

case object NoAction extends ControlAction {
  def apply(): Unit = {}
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
  def apply(): Unit = {controller.navigate(p)}
  def toParseString = s"navigate $p"
}
object NavigateCompanion extends ActionCompanion("navigate to knowledge item", "navigate") {
  import Action._
  def parserActual(implicit state: ActionState) = path ^^ { p => Navigate(p) }
}

case class NavigateSource(ref: SourceRef) extends ControlAction {
  def apply(): Unit = {controller.navigateSource(ref)}
  def toParseString = s"navigateSource $ref"
}
object NavigateSourceCompanion extends ActionCompanion("navigate to physical location", "navigateSource") {
  import Action._
  def parserActual(implicit state: ActionState) = str ^^ { s => NavigateSource(SourceRef.fromString(s)) }
}

case class SuppressErrors(act: Action) extends ControlAction with ActionWithErrorRecovery {
  override def init(c: Controller): Unit = {
    super.init(c)
    act.init(c)
  }
  def apply(errorCont: Option[ErrorHandler]) = {
    act match {
      case a: ActionWithErrorRecovery =>
        val onlyFatalErrors = errorCont.map(ec => new HandlerWithTreshold(ec, Level.Fatal))
        a(onlyFatalErrors)
      case a => a()
    }
  }
  def toParseString = s"suppressErrors " + act.toParseString
}
object SuppressErrorsCompanion extends ActionCompanion("run an action without reporting errors", "suppressErrors") {
  import Action._
  def parserActual(implicit state: ActionState) = action ^^ { a => SuppressErrors(a) }
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
