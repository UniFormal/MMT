package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api._
import frontend._
import parser._
import frontend.actions._
import documents._
import info.kwarc.mmt.api.uom.SimplificationUnit
import objects._

/**
  * An auxilary class to split the [[Controller]] into several files and implement the handling of [[Action]]s.
  */
trait ActionHandling extends
  ArchiveActionHandling with
  CheckActionHandling with
  ControlActionHandling with
  DefineActionHandling with
  ExecActionHandling with
  MathPathActionHandling with
  PrintActionHandling with
  LMHActionHandling {self: Controller =>

  // ******************************** handling actions
    
  // some actions are defined in separate methods below
  /** the name of the current action definition (if any) */
  def currentActionDefinition : Option[String] = state.currentActionDefinition.map(_.name)

  /** executes a string command */
  def handleLine(l: String, showLog: Boolean = true, errorCont: Option[ErrorHandler] = None) {
    val act = Action.parseAct(this, l)
    handle(act, showLog, errorCont)
  }

  /** executes an Action */
  def handle(act: Action, showLog: Boolean = true, errorCont: Option[ErrorHandler] = None) {
    act.init(this)
    state.currentActionDefinition match {
      case Some(Defined(file, name, acts)) if act != EndDefine =>
        state.currentActionDefinition = Some(Defined(file, name, acts ::: List(act)))
        if (showLog) report("user", "  " + name + ":  " + act.toString)
      case _ =>
        if (act != NoAction && showLog) {
          report("user", s"'$act'")
          report.indent
        }
        act match {
          case a: ActionWithErrorRecovery => a(errorCont)
          case a => a()
        }
        if (act != NoAction && showLog) {
          report.unindent
          report("user", s"'$act' finished")
        }
    }
  }

  /** executes an Action without throwing exceptions */
  def tryHandleLine(l: String, showLog: Boolean = true): ActionResult = {
    // parse and run the line
    val act = try {
      Action.parseAct(this, l)
    } catch {
      case pe: ParseError => return ActionParsingError(pe)
    }
    tryHandle(act, showLog)
  }
  /** executes an Action without throwing exceptions */
  def tryHandle(act: Action, showLog: Boolean = true): ActionResult = {
    try {
      handle(act, showLog)
      ActionResultOK()
    } catch {
      case e: Error => ActionExecutionError(e)
    }
  }

  // ******************************** handling messages

  /** processes a message, see [[web.MessageHandler]] */

  // TODO: Where does this belong?
  def handle(message: Message): Response = try {
    message match {
      case EvaluateMessage(contOpt, in, text, out) =>
        val interpreter = extman.get(classOf[checking.Interpreter], in).getOrElse {
          return ErrorResponse("no interpreter found for " + in)
        }
        val presenter = extman.get(classOf[presentation.Presenter], out).getOrElse {
          return ErrorResponse("no presenter found for " + out)
        }
        val context = contOpt.getOrElse(Context.empty)
        val pu = ParsingUnit(SourceRef.anonymous(text), context, text, InterpretationInstructionContext(getNamespaceMap))
        val checked = interpreter(pu)(ErrorThrower)
        val simplified = simplifier(checked.term, SimplificationUnit(context, false, true))
        val presented = presenter.asString(simplified)
        ObjectResponse(presented, "html")

      case _ =>
        ErrorResponse("not implemented yet: " + message.toString)
    }
  } catch {
    case e: Error => ErrorResponse(e.toString)
  }
}
