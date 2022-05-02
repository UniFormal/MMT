package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api._
import frontend._
import utils._
import archives._

import scala.util.matching.Regex

/**
  * An instance of the [[Action]] class represents an atomic command that can be run by a [[Controller]].
  * 
  * All [[Action]] instances are extensions and therefore have access to the [[Controller]]. (They are initialized immediately after parsing.)
  *
  * All subclasses of [[Action]] must have a companion object of type [[ActionCompanion]] that implements parsing for that action.
  */
trait Action extends Extension with MMTTask {
  override def logPrefix = "user"
  /** executes this Action */
  def apply(): Unit
  /** returns this action as a parsable string */
  def toParseString: String
  override def toString: String = toParseString
}

trait ActionWithErrorRecovery extends Action {
  def apply(errorCont: Option[ErrorHandler]): Unit
  def apply() = apply(None)
}

/** parsing of [[Action]]s relative to the parser provided by companion objects (of type [[ActionCompanion]] of the subclasses of [[Action]] */
object Action extends CompRegexParsers {

  /** parses an action from a string, relative to a base path */
  def parseAct(controller: Controller, s: String): Action = {
    val state = new ActionState(controller)
    val p =
      try {
        parse(commented(state), s.trim)
      } catch {case e: Exception =>
        throw ParseError("unknown parse error: " + e.getMessage).setCausedBy(e)
      }
    p match {
      case Success(tree, _) =>
        tree.init(controller)
        tree
      case e: NoSuccess =>
        // try adding an extension and then parse again
        val keyword = s.takeWhile(!_.isWhitespace)
        val applicable = controller.extman.get(classOf[ActionCompanion], keyword)
        if (applicable.isEmpty) {
          val acO = controller.extman.getOrAddExtension(classOf[ActionCompanion], keyword)
          if (acO.isDefined) {
            // recurse only if we found an extension that we didn't have before
            return parseAct(controller, s)
          }
        }
        throw ParseError(s + "\n  error: " + e.msg)
    }
  }

  /** completes a partially typed action */
  def completeAct(c: Controller, s: String): List[String] = complete(commented(new ActionState(c)), s).results.map(_.mkString(""))

  /** production for commented action */
  private def commented(implicit state: ActionState) =
    (comment ^^ { c => NoAction }) |
      (action(state) ~ opt(comment) ^^ { case a ~ _ => a }) |
      success(NoAction)

  /** production for a commented */
  private def comment(implicit state: ActionState) = "//.*" r

  /** production for any known action (the overall list is accessible via state) */
  def action(implicit state: ActionState): Parser[Action] = {
    val allParsers = state.actionCompanions.map(_.parser)
    allParsers.tail.foldLeft(allParsers.head)((l, r) => l | r)
  }

  // shared non-terminals
  
  def path(implicit state: ActionState) = str ^^ { s => Path.parse(s, state.nsMap) }

  def mpath(implicit state: ActionState) = str ^^ { s => Path.parseM(s, state.nsMap) }

  // [str_1,...,str_n] or str
  def stringList(implicit state: ActionState) = ("\\[.*\\]" r) ^^ { s => stringToList(s.substring(1, s.length - 1), ",") } |
    str ^^ { s => List(s) }

  def file(implicit state: ActionState) = str ^^ { s => File(state.home.resolve(s)) }

  def uri(implicit state: ActionState) = str ^^ { s => URI(s) }

  def int(implicit state: ActionState) = str ^^ { s => s.toInt }

  def strMaybeQuoted(implicit state: ActionState) = quotedStr | str

  /** regular expression for non-empty word without whitespace */
  def str(implicit state: ActionState) = "\\S+" r

  /** repeating strings with a prefix
    * precondition: prefix should be a regex with a single  */
  def strs(prefix: String)(implicit state: ActionState) = {
    val pat = s"$prefix((\\s+\\S+)*\\s*)".r
    pat ^^ {s =>
      val matcher = pat.pattern.matcher(s)
      matcher.matches()
      Option(matcher.group(2)).getOrElse("").trim match {
        case "" => List()
        case p => p.split("\\s+").toList
      }
    }
  }

  /** regular expression for quoted string (that may contain whitespace) */
  def quotedStr(implicit state: ActionState) = ("\".*\"" r) ^^ { s => s.substring(1, s.length - 1) }

  /** build modifiers */
  def keyMod(implicit state: ActionState) = str ^^ { case km =>
    BuildTargetModifier.parse(km)
  }
}

/**
  * common superclass of all companion objects of subclasses of [[Action]]
  *
  * @param helpText the help text associated to this [[Action]] class
  * @param mainKeyword Main Keyword the parser for this [[Action]] should trigger on
  * @param alternatives Alternative Keywords that the parser for this [[Action]] should trigger on
  */
abstract class ActionCompanion(val helpText: String, val mainKeyword: String, val alternatives: String*) extends FormatBasedExtension {
  val keywords = mainKeyword :: alternatives.toList
  def isApplicable(word: String) = keywords contains word

  /** checks if this [[ActionCompanion]] matches a given keyword string */
  def matches(word: String) = keywords.exists(k => k == word || k.startsWith("word "))

  /**
    * internal function to generate an [[Action.Parser]] for the [[Action]] class this companion belongs to
    *
    * @param state the state used during the generation of this parser
    * @return
    */
  protected def parserActual(implicit state: ActionState): Action.Parser[Action]

  /** boolean indicating if the parser method should automatically add the keywords in front of this parser */
  protected val addKeywords: Boolean = true
  import Action._

  /** parser for any of the keywords of this [[ActionCompanion]] */
  protected lazy val keyWordParser: Action.Parser[String] = {
    keywords.tail.foldLeft[Action.Parser[String]](keywords.head)(_ | _)
  }

  /** a regex representing any of the keywords */
  protected lazy val keyRegEx: String = {
    val regWords = keywords.map({kw =>
      kw.split(" ").map(Regex.quote).mkString("\\s+")
    })
    regWords.tail.foldLeft(regWords.head)({case (a, b) => s"((?:$a)|(?:$b))"})
  }

  /**
    * generates an [[Action.Parser]] for the [[Action]] corresponding to this parser
    * parses any of the keywords, followed by what [[parserActual]] parses
    *
    * @param state the state used during the generation of this parser
    * @return
    */
  def parser(implicit state: ActionState): Action.Parser[Action] = {
    if (addKeywords) {
      keyWordParser ~> parserActual
    } else {
      parserActual
    }
  }
}

/**
  * convenience class for [[ActionCompanion]] objects of [[Action]]s that do not take any arguments and thus have a trivial parser
  * @param action the corresponding action
  */
abstract class ObjectActionCompanion(action: Action, helpText: String, mainKeyword: String, alternatives: String*)
         extends ActionCompanion(helpText, mainKeyword, alternatives: _*) {
  /** a parser that returns the [[Action]] object instance */
  protected def parserActual(implicit state: ActionState) = Action.success(action)
}