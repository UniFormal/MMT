package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.gui.WindowManager

/** Shared base class for Actions that are related to Windows */
sealed abstract class WindowAction extends ActionImpl {
  protected def winman(implicit controller: Controller) : WindowManager = controller.winman
}


/** close a window with a given ID
  *
  * See ToWindow on how to open windows
  * concrete syntax: window window:STRING close
  */
case class WindowClose(window: String) extends WindowAction {
  def apply(implicit controller: Controller) = winman.deleteWindow(window)
  def toParseString = s"window $window close"
}
object WindowCloseCompanion extends ActionCompanionImpl[WindowClose]("close a window with a given ID", "window") {
  import Action._
  def parserActual(implicit state: ActionState) = str <~ "close" ^^ { s => WindowClose(s) }
}

/** position a window with a given ID
  *
  * See ToWindow on how to open windows
  * concrete syntax: window window:STRING position x:INT y:INT
  */
case class WindowPosition(window: String, x: Int, y: Int) extends WindowAction {
  def apply(implicit controller: Controller): Unit = winman.getWindow(window).setLocation(x, y)
  def toParseString = s"window $window position $x $y"
}
object WindowPositionCompanion extends ActionCompanionImpl[WindowPosition]("position a window with a given ID", "window"){
  import Action._
  override val addKeywords = false
  def parserActual(implicit state: ActionState) = ("window" ~> str <~ "position") ~ int ~ int ^^ { case s ~ x ~ y => WindowPosition(s, x, y) }
}

/** show the GUI window */
case object GUIOn extends WindowAction {
  def apply(implicit controller: Controller) : Unit = winman.openBrowser
  def toParseString: String = "gui on"
}
object GUIOnCompanion extends ActionObjectCompanionImpl[GUIOn.type]("show the GUI window", "gui on")


/** hides the GUI window */
case object GUIOff extends WindowAction {
  def apply(implicit controller: Controller) : Unit = winman.closeBrowser
  def toParseString: String = "gui off"
}
object GUIOffCompanion extends ActionObjectCompanionImpl[GUIOff.type]("hide the GUI window", "gui off")
