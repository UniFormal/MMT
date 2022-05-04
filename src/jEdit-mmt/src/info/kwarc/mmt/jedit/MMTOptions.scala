package info.kwarc.mmt.jedit

import info.kwarc.mmt.api.utils._
import org.gjt.sp.jedit._

/** interface to the jedit settings file and the options dialog */
abstract class MMTOption[A] {
  def toString(a: A): String = a.toString

  def fromString(s: String): A

  /** an identifier for this option (must be unique among the MMTOptions) */
  val key: String
  val jeditKey = "info.kwarc.mmt.jedit.MMTPlugin." + key

  /** get the current setting from jedit */
  def get: Option[A] = {
    val p = jEdit.getProperty(jeditKey)
    if (p == null || p == "")
      None
    else
      Some(fromString(p))
  }

  /** set the setting in jedit */
  def set(a: A) {
    jEdit.setProperty(jeditKey, toString(a))
  }

  val label: String
  val guiComponent: java.awt.Component

  def fromGUIComponent: A

  def toGUIComponent(a: A)

  /** add an interface component to the jedit plugin options pane */
  def init(op: AbstractOptionPane) {
    get.foreach { a => toGUIComponent(a) }
    op.addComponent(label, guiComponent)
  }

  /** save the values of the interface component to the jedit settings */
  def save {
    set(fromGUIComponent)
  }
}

/** an option that is displayed as a text field */
abstract class TextOption[A] extends MMTOption[A] {
  val guiComponent = new java.awt.TextField

  def fromGUIComponent = fromString(guiComponent.getText)

  def toGUIComponent(a: A) = guiComponent.setText(toString(a))
}

/** a string-valued option */
class StringOption(val key: String, val label: String) extends TextOption[String] {
  def fromString(s: String) = s
}

/** a File-valued option */
class FileOption(val key: String, val label: String) extends TextOption[File] {
  def fromString(s: String) = new java.io.File(s)
}

/** a Boolean-valued option */
class BooleanOption(val key: String, val label: String) extends MMTOption[Boolean] {
  def fromString(s: String) = s match {
    case "true" => true
    case "false" => false
  }

  val guiComponent = new java.awt.Checkbox

  def fromGUIComponent = guiComponent.getState

  def toGUIComponent(b: Boolean) = guiComponent.setState(b)
}

/** contains the various MMT options
 *
 *  to add an option, just create it here and add it to the list of all options
 */
object MMTOptions {
  val startup = new StringOption("startup", "custom startup msl file")
  val config = new StringOption("config", "custom configuration file")
  val archives = new StringOption("archives", "local folder that contains archives (mathhub root)")
  val lowAsciiDelims = new BooleanOption("lowAsciiDelims", "use ASCII 28-31 as delimiters")

  // taken out because it does not work well: MMT-parsing every time is too slow to run all the time
  // But otherwise, the source references are not uptodate.
  // val semantichighlighting = new BooleanOption("SemanticHighlighting","Semantic highlighting")

  /** the list of all options */
  val all = List(startup, config, archives, lowAsciiDelims)
}

/** the MMT plugin options pane using the options defined in the companion object */
class MMTOptions extends AbstractOptionPane("info.kwarc.mmt.jedit.MMTPlugin") {
  override def _init {
    addSeparator("all paths are relative to MMT plugin home")
    MMTOptions.all foreach { c => c.init(this) }
  }

  override def _save {
    MMTOptions.all foreach { c => c.save }
  }
}
