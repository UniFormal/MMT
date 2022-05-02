package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.utils.File

/** shared base class for actions related to execution of Action code */
sealed abstract class ExecAction extends Action

/** load a file containing commands and execute them, fails on first error if any
  *
  * concrete syntax: file file:FILE
  */
case class ExecFile(file: File, name: Option[String]) extends ExecAction with ActionWithErrorRecovery {
  def apply(errorCont: Option[ErrorHandler]) {controller.runMSLFile(file, name, true, errorCont)}
  def toParseString = s"file $file ${name.map(" " + _).getOrElse("")}"
}
object ExecFileCompanion extends ActionCompanion("load a file containing commands and execute them", "file") {
  import Action._
  def parserActual(implicit state: ActionState) = file ~ (str ?) ^^ { case f ~ s => ExecFile(f, s) }
}

/** run a Scala interpreter or evaluate a Scala expression */
case class Scala(init: Option[String]) extends ExecAction {
  def apply() {
    val interp = new MMTILoop(controller)
    interp.run(init)
  }
  def toParseString = s"scala${init.map(" " + _).getOrElse("")}"
}
object ScalaCompanion extends ActionCompanion("run a Scala interpreter or evaluate a Scala expression", "scala"){
  import Action._
  def parserActual(implicit state: ActionState) = ("[^\\n]*" r) ^^ { s => val t = s.trim; Scala(if (t == "") None else Some(t)) }
}

/** helper functions for [[ExecAction]]s */
trait ExecActionHandling{self: Controller =>

  /** runs a given file, handling [[ExecFile]] */
  def runMSLFile(f: File, nameOpt: Option[String], showLog: Boolean, errorCont: Option[ErrorHandler]) {
    val folder = f.getParentFile
    // store old state, and initialize fresh state
    val oldHome = state.home
    val oldCAD = state.currentActionDefinition
    state.home = folder
    state.currentActionDefinition = None
    // execute the file
    File.read(f).split("\\n").foreach(f => handleLine(f, showLog, errorCont))
    if (state.currentActionDefinition.isDefined)
      throw ParseError("end of definition expected")
    // restore old state
    state.home = oldHome
    state.currentActionDefinition = oldCAD
    // run the actionDefinition, if given
    nameOpt foreach { name =>
      runDefinition(Some(folder), name)
    }
  }
}
