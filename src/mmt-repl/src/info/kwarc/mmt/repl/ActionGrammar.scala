package info.kwarc.mmt.repl

import info.kwarc.mmt.api.frontend.{Controller, Extension, Logger}
import info.kwarc.mmt.repl.CompletionEngine.{Builder, ContextualStringValue, IntegerValue, StringValue}
import info.kwarc.mmt.repl.CompletionEngine.BuilderConversions._ // for implicit conversions

sealed class ActionGrammar() {

  private[repl] var controller : Controller = null

  val action = (log | mathpath | archive | oaf | extension | mws | server |
    windowaction | execfile | defactions | scala | mbt |
    setbase | read | interpret | check | checkTerm | navigate |
    printall | printallxml | printConfig | diff | clear | exit | getaction).! // getaction must be at end for default get

  private def log = logfilets | logfile | loghtml | logconsole | logon | logoff
  private def logfile = "log file" ~ filepath
  private def logfilets = "log filets" ~ filepath
  private def logconsole = "log console"
  private def loghtml = "log html" ~ filepath
  private def logon = "log+" ~ inactivelogprefix
  private def logoff = "log-" ~ activelogprefix

  private def inactivelogprefix = (u:Unit) => {
    extensions.map(_.logPrefix)
  }
  private def activelogprefix = (u:Unit) => {
    controller.report.active
  }


  private def mathpath = "mathpath" ~ (mathpathArchive | mathpathLocal | mathpathFS | mathpathJava)
  private def mathpathArchive = "archive" ~ dirpath
  private def mathpathLocal = "local"
  private def mathpathFS = "fs" ~ uri ~ anypath
  private def mathpathJava = "java" ~ classpath

  private def archive = archopen | archdim | archmar | archbuild | filebuild | confbuild
  private def archopen = "archive" ~ "add" ~ dirpath
  private def optFilePath = filepath?

  private def archbuild = "build" ~ stringList ~ keyMod ~ (anypath ?)
  private def keyMod = StringValue("*!&012345".split("").toList)


  private def confbuild = "cbuild" ~ str ~ stringList ~ str
  private def filebuild = ("make" | "rbuild") ~ str ~ (str*)
  private def archdim = "archive" ~ stringList ~ dimension ~ optFilePath
  private def dimension = "check" | "validate" | "relational" | "integrate" | "test" | "close"
  private def archmar = "archive" ~ loadedarchivename ~ ("mar" ~ anypath)

  private def loadedarchivename = (u:Unit) => {
    controller.backend.getArchives.map(_.id)
  }

  private def archivename = (u: Unit) => {
    List("MMT/examples")
  }

  private def oaf = "oaf" ~ (oafInit | oafClone | oafPull | oafPush)
  private def oafInit = "init" ~ dirpath
  private def oafClone = "clone" ~ archivename
  private def oafPull = "pull"
  private def oafPush = "push"

  private def server = serveron | serveroff
  private def serveron = "server" ~ "on" ~ (int ~ (str?))
  private def serveroff = "server" ~ "off"

  private def execfile = "file " ~ filepath ~ (str ?)
  private def defactions = define | enddefine | dodefined
  private def define = "define " ~ str
  private def enddefine = "end"
  private def dodefined = "do " ~ str ~ (anypath ?)

  private def scala = "scala" ~ stringList
  private def mbt = "mbt" ~ anypath

  private def read = "read" ~ anypath
  private def interpret = "interpret" ~ anypath
  private def check = "check" ~ path ~ (str ?)

  private def checkTerm = "term" ~ quotedStr

  private def printall = "printAll"
  private def printallxml = "printXML"
  private def printConfig = "printConfig"

  private def extension = "extension" ~ extensionName ~ (strMaybeQuoted *)
  private def extensionName = (u:Unit) => {
    extensions.map(_.getClass.getCanonicalName)
  }

  private def mws = "mws" ~ uri
  private def navigate = "navigate" ~ path
  private def setbase = "base" ~ path
  private def clear = "clear"
  private def exit = "exit"

  private def diff = path ~ ("diff" ~ int)

  private def getaction = tofile | towindow | respond | print
  // print is default
  private def tofile = presentation ~ ("write" ~ filepath)
  private def towindow = presentation ~ ("window" ~ str)
  private def respond = presentation ~ "respond"
  private def print = presentation

  private def presentation = present | deps | defaultPresent
  private def present = content ~ ("present" ~ str)
  private def deps = path ~ "deps"
  private def defaultPresent = content
  private def content = "get" ~ (closure | elaboration | component | get)
  private def closure = path ~ "closure"
  private def elaboration = path ~ "elaboration"
  private def component = path ~ "component" ~ str
  private def get = path

  private def windowaction = windowclose | windowpos | gui
  private def windowclose = "window" ~ str ~ "close"
  private def windowpos = "window" ~ str ~ "position" ~ int ~ int
  private def gui = "gui" ~ ("on" | "off")


  private def path = str

  private def anypath = filepath | dirpath
  private def filepath = str
  private def dirpath = str
  private def classpath = dirpath

  private def uri = StringValue(List("uri"))
  private def int = IntegerValue(Nil)
  private def strMaybeQuoted = quotedStr | str

  /** regular expression for non-empty word without whitespace */
  private def str = Builder(StringValue(Nil))
  private def quotedStr = str
  private def stringList = str +

  /** helpers */
  private def extensions : List[Extension] = controller.extman.get(classOf[Extension])
}