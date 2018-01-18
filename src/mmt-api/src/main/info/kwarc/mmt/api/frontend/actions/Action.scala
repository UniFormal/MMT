package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.{Error, Level, MMTTask, NamespaceMap, ParseError, Path}
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.archives.{Build, BuildDepsFirst, Clean, Update}
import info.kwarc.mmt.api.frontend.{Controller, Report}

import scala.reflect.ClassTag

/**
  * An instance of the [[Action]] class represents an atomic command that can be run by a
  * [[info.kwarc.mmt.api.frontend.Controller]].
  *
  * Each Action, or group thereof, is defined within a single file within this package.
  * This is achieved using the [[ActionImpl]] class.
  *
  * Actions themselves consist only of the grammar, that is they only define the parameters they depend on.
  * However, each Action has an apply method, that is used to execute the action.
  * If necessary to access private controller variables, this method simply delegates to a method of the
  * [[ActionHandling]] trait.
  *
  * Together with each action, an [[ActionCompanion]] is needed to implement static methods of the class.
  * By convention, the companion belonging to a class named &lt;Action&gt; is called &lt;Action&gt;Companion.
  */
sealed abstract class Action extends MMTTask with ImplicitLogger {
  val logPrefix: String = "user"

  /** executes this Action in the given [[ActionHandling]] instance */
  def apply(implicit controller: Controller): Unit

  /**
    * Gets the [[ActionCompanion]] corresponding to this action.
    */
  def getParsingCompanion: ActionCompanion[this.type] = ActionCompanion.find(this).asInstanceOf[ActionCompanion[this.type]]

  /**
    * Turns this Action into a parsable string.
    *
    * @return
    */
  def toParseString: String

  override def toString: String = toParseString
}

/** exactly like Logger, but takes an implicit controller as parameter */
trait ImplicitLogger {
  def logPrefix: String

  /** the report being used implicitly */
  protected def report(implicit controller: Controller) : Report = controller.report

  /** logs a message with this logger's logprefix */
  protected def log(s: => String, subgroup: Option[String] = None)(implicit controller: Controller) =
    controller.report(logPrefix + subgroup.map("-" + _).getOrElse(""), s)

  /** temporary logging - always logged */
  // calls to this method are for debugging; if they are committed, they should be removed
  protected def logTemp(s: => String)(implicit controller: Controller) =
    controller.report("temp", s"($logPrefix) $s")

  /** log as an error message */
  protected def logError(s: => String)(implicit controller: Controller) = controller.report("error", s"($logPrefix) $s")

  /** logs an error - always logged */
  protected def log(e: Error)(implicit controller: Controller) = controller.report(e)

  /** wraps around a group to create nested logging */
  protected def logGroup[A](a: => A)(implicit controller: Controller): A = {
    report.indent
    try {
      a
    } finally {
      report.unindent
    }
  }
}

/** Intermediate Class to limit the scope of [[Action]] to the actions namespace */
private[actions] abstract class ActionImpl extends Action {}

/**
  * Implements general parsing code for [[Action]]s
  */
object Action extends CompRegexParsers {

  private def commented(implicit state: ActionState) =
    (comment ^^ { c => NoAction }) |
      (action() ~ opt(comment) ^^ { case a ~ _ => a }) |
      success(NoAction)

  private def comment(implicit state: ActionState) = "//.*" r

  /**
    * Returns a parser for all known actions
    *
    * @param includeProtected If set to true, includes Actions which are marked as protected
    * @param state            Context needed to parse actions
    * @return
    */
  private[actions] def action(includeProtected: Boolean = false)(implicit state: ActionState): Parser[Action] = {
    val allParsers = ActionCompanion.all.filter(includeProtected || !_.isProtected).map(_.parser)
    allParsers.tail.foldLeft(allParsers.head)((l, r) => l | r)
  }

  // shared non-terminals
  private[actions] def path(implicit state: ActionState) = str ^^ { s => Path.parse(s, state.nsMap) }

  private[actions] def mpath(implicit state: ActionState) = str ^^ { s => Path.parseM(s, state.nsMap) }

  // [str_1,...,str_n] or str
  private[actions] def stringList(implicit state: ActionState) = ("\\[.*\\]" r) ^^ { s => stringToList(s.substring(1, s.length - 1), ",") } |
    str ^^ { s => List(s) }

  private[actions] def file(implicit state: ActionState) = str ^^ { s => File(state.home.resolve(s)) }

  private[actions] def uri(implicit state: ActionState) = str ^^ { s => URI(s) }

  private[actions] def int(implicit state: ActionState) = str ^^ { s => s.toInt }

  private[actions] def strMaybeQuoted(implicit state: ActionState) = quotedStr | str

  /** regular expression for non-empty word without whitespace */
  private[actions] def str(implicit state: ActionState) = "\\S+" r

  /** regular expression for quoted string (that may contain whitespace) */
  private[actions] def quotedStr(implicit state: ActionState) = ("\".*\"" r) ^^ { s => s.substring(1, s.length - 1) }

  /** build modifiers */
  private[actions] def keyMod(implicit state: ActionState) = str ^^ { case km =>
    if (km.startsWith("-"))
      (km.tail, Clean)
    else if ("*!&012345".contains(km.last))
      (km.init, km.last match {
        case '!' => Build(Update(Level.Error))
        case '&' => BuildDepsFirst(Update(Level.Error))
        case '*' => Build(Update(Level.Ignore))
        case d => Build(Update(d.asDigit - 1))
      })
    else (km, Build)
  }

  /** parses an action from a string, relative to a base path */
  def parseAct(s: String, b: Path, h: File): Action = {
    val p =
      try {
        parse(commented(ActionState(NamespaceMap(b), h)), s)
      }
      catch {
        case e: Exception =>
          throw ParseError("unknown parse error: " + e.getMessage).setCausedBy(e)
      }
    p match {
      case Success(tree, _) => tree
      case e: NoSuccess => throw ParseError(s + "\n  error: " + e.msg)
    }
  }

  def completeAct(s: String): List[String] = complete(commented(ActionState()), s).results.map(_.mkString(""))
}

/**
  * common superclass of all companion objects of subclasses of [[Action]]
  *
  * This mostly includes an [[Action.Parser]] along with help strings
  *
  * @param helpText the help text associated to this [[Action]] class
  * @param mainKeyword Main Keyword the parser for this [[Action]] should trigger on
  * @param alternatives Alternative Keywords that the parser for this [[Action]] should trigger on
  * @tparam T the sub-class of actions this companion is for
  */
sealed abstract class ActionCompanion[+T <: Action](val helpText: String, val mainKeyword: String, val alternatives: String*)(implicit ct: ClassTag[T]) extends AccessibleCompanion[T] {
  val keywords = mainKeyword :: alternatives.toList

  /**
    * Mark this [[ActionCompanion]] as protected. Protected [[Action]]s are not automatically added to the parser
    * generated by [[Action.action]]s. This flag should be set to true if the parser for this class recurses into
    * [[Action.action]] to prevent an infinite recursion.
    */
  private[actions] val isProtected: Boolean = false

  /** checks if this [[ActionCompanion]] matches a given keyword string */
  def matches(word: String): Boolean = keywords.exists(keyword => {
    if (keyword.contains(" ")) keyword.startsWith(word) else keyword == word
  })

  /**
    * internal function to generate an [[Action.Parser]] for the [[Action]] class this companion belongs to
    *
    * @param state the state used during the generation of this parser
    * @return
    */
  protected def parserActual(implicit state: ActionState): Action.Parser[T]

  /** boolean indicating if the parser method should automatically add the keywords in front of this parser */
  protected val addKeywords: Boolean = true
  import Action._

  /** parser for any of the keywords of this [[ActionCompanion]] */
  lazy val keyWordParser: Action.Parser[String] = {
    keywords.tail.foldLeft[Action.Parser[String]](keywords.head)(_ | _)
  }

  /**
    * generates an [[Action.Parser]] for the [[Action]] corresponding to this parser
    * parses any of the keywords, followed by what [[parserActual]] parses
    *
    * @param state the state used during the generation of this parser
    * @return
    */
  def parser(implicit state: ActionState): Action.Parser[T] = {
    if (addKeywords) {
      keyWordParser ~> parserActual
    } else {
      parserActual
    }
  }
}

/** Intermediate Class to limit the scope of [[ActionCompanion]] to the actions namespace */
private[actions] abstract class ActionCompanionImpl[+T <: Action](helpText: String, mainKeyword: String, alternatives: String*)(implicit ct: ClassTag[T]) extends ActionCompanion[T](helpText, mainKeyword, alternatives: _*)(ct)

/**
  * Represents an [[ActionCompanion]] for an [[Action]] that is represented as a single case object
  *
  * @param helpText the help text associated to this [[Action]] class
  * @param mainKeyword Main Keyword the parser for this [[Action]] should trigger on
  * @param alternatives Alternative Keywords that the parser for this [[Action]] should trigger on
  * @tparam T the sub-class of actions this companion is for
  */
sealed abstract class ActionObjectCompanion[+T <: Action with Singleton](helpText: String, mainKeyword: String, alternatives: String*)(implicit ct: ClassTag[T]) extends ActionCompanion[T](helpText, mainKeyword, alternatives: _*) with SingletonAccessibleCompanion[T] {
  /** a parser that returns the [[Action]] object instance */
  protected def parserActual(implicit state: ActionState): Action.Parser[T] = Action.success(companionInstance)
}

/** Intermediate Class to limit the scope of [[ActionObjectCompanion]] to the actions namespace */
private[actions] abstract class ActionObjectCompanionImpl[+T <: Action with Singleton](helpText: String, mainKeyword: String, alternatives: String*)(implicit ct: ClassTag[T]) extends ActionObjectCompanion(helpText, mainKeyword, alternatives: _*)(ct)

/**
  * Companion object to the [[ActionCompanion]] object manages a list of known [[ActionCompanion]]s
  */
object ActionCompanion extends AccessibleCompanionCollection[Action, ActionCompanion[Action]] {
  /** finds an [[ActionCompanion]] belonging to a given keyword */
  def find(keyword: String): List[ActionCompanion[Action]] = filter(_.matches(keyword))

  // =============================================
  // Register all the Companions
  // ==============================================

  // MetaAction
  register(NoActionCompanion)
  register(RemoteActionCompanion)

  // LoggingAction
  register(ListReportGroupsCompanion)
  register(AddReportHandlerCompanion)
  register(LoggingOnCompanion)
  register(LoggingOffCompanion)

  // ExecAction
  register(ExecFileCompanion)
  register(ScalaCompanion)
  register(MBTCompanion)

  // DefineAction
  register(InspectDefineCompanion)
  register(DefineCompanion)
  register(EndDefineCompanion)
  register(DoCompanion)

  // Checking
  register(CheckCompanion)
  register(CheckTermCompanion)
  register(NavigateCompanion)
  register(CompareCompanion)

  // MathPathAction
  register(LocalCompanion)
  register(AddArchiveCompanion)
  register(AddMathPathFSCompanion)
  register(AddMathPathJavaCompanion)
  register(ReadCompanion)

  // ServerAction
  register(ServerInfoActionCompanion)
  register(ServerOnCompanion)
  register(ServerOffCompanion)

  // Printing
  register(PrintAllCompanion)
  register(PrintAllXMLCompanion)
  register(PrintConfigCompanion)
  register(HelpActionCompanion)

  // OAFAction
  register(SetOAFRootCompanion)
  register(GetOAFRootCompanion)
  register(OAFInitCompanion)
  register(OAFCloneCompanion)
  register(OAFShowCompanion)
  register(OAFPullCompanion)
  register(OAFPushCompanion)
  register(OAFSetRemoteCompanion)

  // ControlAction
  register(ClearCompanion)
  register(ExitCompanion)
  register(SetBaseCompanion)

  // Extension
  register(ListExtensionsCompanion)
  register(AddExtensionCompanion)
  register(RemoveExtensionCompanion)
  register(AddMWSCompanion)

  // WindowAction
  register(WindowCloseCompanion)
  register(WindowPositionCompanion)
  register(GUIOnCompanion)
  register(GUIOffCompanion)

  // ArchiveAction
  register(ArchiveBuildCompanion)
  register(ConfBuildCompanion)
  register(MakeActionCompanion)
  register(ArchiveMarCompanion)

  // GetAction
  // This **must** be at the end, to act as a default for stuff
  register(GetActionCompanion)
}