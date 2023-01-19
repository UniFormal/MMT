package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.{Checker, RelationHandler, CheckingEnvironment}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.Context
import info.kwarc.mmt.api.parser.SourceRef

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
  def apply(): Unit = {controller.checkTerm(s)}
  def toParseString = "term \""+s+"\""
}
object CheckTermCompanion extends ActionCompanion("check a knowledge item with respect to a certain checker", "term") {
  import Action._
  def parserActual(implicit state: ActionState) = quotedStr ^^ { s => CheckTerm(s)}
}

/** Implements handling of [[CheckAction]]s */
trait CheckActionHandling {self: Controller =>

  /** Checks a path using the [[Checker]] of the given ID, handling [[CheckAction]] */
  def checkPath(p: Path, id: String)(implicit task: MMTTask): Unit = {
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
}
