package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.{Checker, CheckingEnvironment, RelationHandler}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.Context

/** shared base class for actions checking objects */
sealed abstract class CheckAction extends Action

case class Check(p: Path, id: String) extends CheckAction {
  def apply() =controller.checkPath(p, id)(this)
  def toParseString = s"check $p $id"
}
object CheckCompanion extends ActionCompanion("check a knowledge item with respect to a certain checker", "check") {
  import Action._
  def parserActual(implicit state: ActionState) =  path ~ (str ?) ^^ {case p ~ idOpt => Check(p, idOpt.getOrElse("mmt"))}
}

case class CheckTerm(s: String) extends CheckAction {
  def apply() {controller.checkTerm(s)}
  def toParseString = "term \""+s+"\""
}
object CheckTermCompanion extends ActionCompanion("check a knowledge item with respect to a certain checker", "term") {
  import Action._
  def parserActual(implicit state: ActionState) = quotedStr ^^ { s => CheckTerm(s)}
}

case class Navigate(p: Path) extends CheckAction {
  def apply() {controller.navigate(p)}
  def toParseString = s"navigate $p"
}
object NavigateCompanion extends ActionCompanion("navigate to knowledge item", "navigate") {
  import Action._
  def parserActual(implicit state: ActionState) = path ^^ { p => Navigate(p) }
}

case class Compare(p: Path, r: Int) extends CheckAction {
  def apply() {???}
  override def toParseString = s"diff ${p.toPath}:$r"
}
object CompareCompanion extends ActionCompanion("Compare two objects", "diff") {
  import Action._
  def parserActual(implicit state: ActionState) = path ~ ("diff" ~> int) ^^ { case p ~ i => Compare(p, i) }
}

/** Implements handling of [[CheckAction]]s */
trait CheckActionHandling {self: Controller =>

  /** Checks a path using the [[Checker]] of the given ID, handling [[CheckAction]] */
  def checkPath(p: Path, id: String)(implicit task: MMTTask) {
    val checker = extman.get(classOf[Checker], id).getOrElse {
      throw GeneralError(s"no checker $id found")
    }
    checker(p)(new CheckingEnvironment(simplifier, new ErrorLogger(report), RelationHandler.ignore, task))
  }

  /** Checks a term relative to the current base path, handling [[CheckTerm]] */
  def checkTerm(s: String): Unit = getBase match {
    case m: ContentPath =>
      handle(EvaluateMessage(Some(Context(m.module)), "mmt", s, "present-text-notations")) match {
        case ObjectResponse(sC, _) =>
          report("user", sC)
        case ErrorResponse(msg) =>
          report("user", msg)
        case _ => // impossible
      }
    case _ => report("error", "base must be content path")
  }

  /** navigates to a given path, handling [[Navigate]] */
  def navigate(p: Path): Unit = {
    notifyListeners.onNavigate(p)
  }
}
