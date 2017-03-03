package info.kwarc.mmt.repl

import info.kwarc.mmt.repl.CompletionEngine.{Builder, IntegerValue, StringValue}
import info.kwarc.mmt.repl.CompletionEngine.BuilderConversions._

object ActionGrammar {

  def action = log | mathpath | archive | oaf | extension | mws | server |
    windowaction | execfile | defactions | scala | mbt |
    setbase | read | interpret | check | checkTerm | navigate |
    printall | printallxml | printConfig | diff | clear | exit | getaction // getaction must be at end for default get

  private def log = logfilets | logfile | loghtml | logconsole | logon | logoff
  private def logfile = "log file" ~> file
  private def logfilets = "log filets" ~> file
  private def logconsole = "log console"
  private def loghtml = "log html" ~> file
  private def logon = "log+" ~> str
  private def logoff = "log-" ~> str

  private def mathpath = "mathpath" ~> (mathpathArchive | mathpathLocal | mathpathFS | mathpathJava)
  private def mathpathArchive = "archive" ~> file
  private def mathpathLocal = "local"
  private def mathpathFS = "fs" ~> uri ~ file
  private def mathpathJava = "java" ~> file

  private def archive = archopen | archdim | archmar | archbuild | filebuild | confbuild
  private def archopen = "archive" ~> "add" ~> file
  private def optFilePath = (str ?)
  private def archbuild = "build" ~> stringList ~ keyMod ~ optFilePath
  private def confbuild = "cbuild" ~> str ~ stringList ~ str
  private def filebuild = ("make" | "rbuild") ~> str ~ (str *)
  private def archdim = "archive" ~> stringList ~ dimension ~ optFilePath
  private def dimension = "check" | "validate" | "relational" | "integrate" | "test" | "close"
  private def archmar = "archive" ~> str ~ ("mar" ~> file)

  private def oaf = "oaf" ~> (oafInit | oafClone | oafPull | oafPush)
  private def oafInit = "init" ~> str
  private def oafClone = "clone" ~> str
  private def oafPull = "pull"
  private def oafPush = "push"

  private def server = serveron | serveroff
  private def serveron = "server" ~> "on" ~> (int ~ (str?))
  private def serveroff = "server" ~> "off"

  private def execfile = "file " ~> file ~ (str ?)
  private def defactions = define | enddefine | dodefined
  private def define = "define " ~> str
  private def enddefine = "end"
  private def dodefined = "do " ~> str ~ (file ?)

  private def scala = "scala" ~> stringList
  private def mbt = "mbt" ~> file

  private def read = "read" ~> file
  private def interpret = "interpret" ~> file
  private def check = "check" ~> path ~ (str ?)

  private def checkTerm = "term" ~> quotedStr

  private def printall = "printAll"
  private def printallxml = "printXML"
  private def printConfig = "printConfig"

  private def extension = "extension" ~> str ~ (strMaybeQuoted *)
  private def mws = "mws" ~> uri
  private def navigate = "navigate" ~> path
  private def setbase = "base" ~> path
  private def clear = "clear"
  private def exit = "exit"

  private def diff = path ~ ("diff" ~> int)

  private def getaction = tofile | towindow | respond | print
  // print is default
  private def tofile = presentation ~ ("write" ~> file)
  private def towindow = presentation ~ ("window" ~> str)
  private def respond = (presentation <~ "respond")
  private def print = presentation
  private def presentation = present | deps | defaultPresent
  private def present = content ~ ("present" ~> str)
  private def deps = path <~ "deps"
  private def defaultPresent = content
  private def content = "get" ~> (closure | elaboration | component | get)
  private def closure = path <~ "closure"
  private def elaboration = path <~ "elaboration"
  private def component = (path <~ "component") ~ str
  private def get = path

  private def windowaction = windowclose | windowpos | gui
  private def windowclose = "window" ~> str <~ "close"
  private def windowpos = ("window" ~> str <~ "position") ~ int ~ int
  private def gui = "gui" ~> ("on" | "off")

  /* common non-terminals */
  private def path = StringValue(List("path"))
  private def mpath = StringValue(List("mpath"))
  private def file = StringValue(List("file"))
  private def uri = StringValue(List("uri"))
  private def int = IntegerValue(Nil)
  private def strMaybeQuoted = quotedStr | str
  /** regular expression for non-empty word without whitespace */
  private def str = StringValue(List("string"))
  private def stringList = str ~> str*
  /** regular expression for quoted string (that may contain whitespace) */
  private def quotedStr = StringValue(Nil)

  private def keyMod = StringValue("*!&012345".split("").toList)
}