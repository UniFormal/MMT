package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.utils._

import scala.language.postfixOps
import scala.util.parsing.combinator._

/** parser for Actions
  *
  * This object implements a combinator parser for Actions. It is used in particular by the [[Controller]]
  * It is straightforward to understand the grammar from the source code.
  */
object Action extends  RegexParsers {
  private var nsMap: NamespaceMap = NamespaceMap.empty
  private var home: File = File(System.getProperty("user.dir"))

  private def commented =
    (comment ^^ { c => NoAction }) |
      (action ~ opt(comment) ^^ { case a ~ _ => a }) |
      success(NoAction)

  private def comment = "//.*" r

  private def action = log | mathpath | archive | oaf | extension | mws | server |
    windowaction | execfile | defactions | scala | mbt |
    setbase | envvar | read | interpret | graph | check | navigate |
    printall | printallxml | printConfig | diff | clear | exit | getaction // getaction must be at end for default get

  private def log = logfilets | logfile | loghtml | logconsole | logon | logoff

  private def logfile = "log file" ~> file ^^ { f => AddReportHandler(new TextFileHandler(f, false)) }

  private def logfilets = "log filets" ~> file ^^ { f => AddReportHandler(new TextFileHandler(f, true)) }

  private def logconsole = "log console" ^^ { case _ => AddReportHandler(ConsoleHandler) }

  private def loghtml = "log html" ~> file ^^ { f => AddReportHandler(new HtmlFileHandler(f)) }

  private def logon = "log+" ~> str ^^ { s => LoggingOn(s) }

  private def logoff = "log-" ~> str ^^ { s => LoggingOff(s) }

  private def mathpath = "mathpath" ~> (mathpathArchive | mathpathLocal | mathpathFS | mathpathJava)

  private def mathpathArchive = "archive" ~> file ^^ { f => AddArchive(f) }

  private def mathpathLocal = "local" ^^ { case _ => Local }

  private def mathpathFS = "fs" ~> uri ~ file ^^ { case u ~ f => AddMathPathFS(u, f) }

  private def mathpathJava = "java" ~> file ^^ { f => AddMathPathJava(f) }

  private def archive = archopen | archdim | archmar | archbuild | filebuild

  private def archopen = "archive" ~> "add" ~> file ^^ { f => AddArchive(f) }

  private def optFilePath = (str ?) ^^ { case in => FilePath(stringToList(in.getOrElse(""), "/")) }

  private def archbuild = "build" ~> stringList ~ keyMod ~ optFilePath ^^ {
    case ids ~ km ~ in =>
      ArchiveBuild(ids, km._1, km._2, in)
  }

  private def filebuild = ("make" | "rbuild") ~> str ~ (str *) ^^ {
    case key ~ args => MakeAction(key, args)
  }

  private def archdim = "archive" ~> stringList ~ dimension ~ optFilePath ^^ {
    case ids ~ dim ~ s =>
      ArchiveBuild(ids, dim, Build, s)
  }

  private def dimension = "check" | "validate" | "relational" | "integrate" | "test" | "close"

  private def archmar = "archive" ~> str ~ ("mar" ~> file) ^^ { case id ~ trg => ArchiveMar(id, trg) }

  private def oaf = "oaf" ~> (oafInit | oafClone | oafPull | oafPush)

  private def oafInit = "init" ~> str ^^ { case s => OAFInit(s) }

  private def oafClone = "clone" ~> str ^^ { case s => OAFClone(s) }

  private def oafPull = "pull" ^^ { _ => OAFPull }

  private def oafPush = "push" ^^ { _ => OAFPush }

  private def extension = "extension" ~> str ~ (strMaybeQuoted *) ^^ { case c ~ args => AddExtension(c, args) }

  private def mws = "mws" ~> uri ^^ { u => AddMWS(u) }

  private def server = serveron | serveroff

  private def serveron = "server" ~> "on" ~> int ^^ { i => ServerOn(i) }

  private def serveroff = "server" ~> "off" ^^ { _ => ServerOff }

  private def execfile = "file " ~> file ~ (str ?) ^^ { case f ~ s => ExecFile(f, s) }

  private def defactions = define | enddefine | dodefined

  private def define = "define " ~> str ^^ { s => Define(s) }

  private def enddefine = "end" ^^ { case _ => EndDefine }

  private def dodefined = "do " ~> str ~ (file ?) ^^ { case s ~ f => Do(f, s) }

  private def scala = "scala" ~> ("[^\\n]*" r) ^^ { s => val t = s.trim; Scala(if (t == "") None else Some(t)) }

  private def mbt = "mbt" ~> file ^^ { f => MBT(f) }

  private def setbase = "base" ~> path ^^ { p => SetBase(p) }

  private def envvar = "envvar" ~> str ~ quotedStr ^^ { case name ~ value => SetEnvVar(name, value) }

  private def read = "read" ~> file ^^ { f => Read(f, false) }
  private def interpret = "interpret" ~> file ^^ { f => Read(f, true) }

  private def graph = "graph" ~> file ^^ { f => Graph(f) }

  private def check = "check" ~> path ~ (str ?) ^^ { case p ~ idOpt => Check(p, idOpt.getOrElse("mmt")) }

  private def navigate = "navigate" ~> path ^^ { p => Navigate(p) }

  private def printall = "printAll" ^^ { case _ => PrintAll }

  private def printallxml = "printXML" ^^ { case _ => PrintAllXML }
  
  private def printConfig = "printConfig" ^^ {case _ => PrintConfig}

  private def clear = "clear" ^^ { case _ => Clear }

  private def exit = "exit" ^^ { case _ => Exit }

  private def diff = path ~ ("diff" ~> int) ^^ { case p ~ i => Compare(p, i) }

  private def getaction = tofile | towindow | respond | print

  // print is default
  private def tofile = presentation ~ ("write" ~> file) ^^ { case p ~ f => GetAction(ToFile(p, f)) }

  private def towindow = presentation ~ ("window" ~> str) ^^ { case p ~ w => GetAction(ToWindow(p, w)) }

  private def respond = (presentation <~ "respond") ^^ { case p => GetAction(Respond(p)) }

  private def print = presentation ^^ { p => GetAction(Print(p)) }

  private def presentation = present | deps | defaultPresent

  private def present = content ~ ("present" ~> str) ^^ { case c ~ p => Present(c, p) }

  private def deps = path <~ "deps" ^^ { case p => Deps(p) }

  private def defaultPresent = content ^^ { c => Present(c, "text") }

  private def content = "get" ~> (closure | elaboration | component | get)

  private def closure = path <~ "closure" ^^ { p => Closure(p) }

  private def elaboration = path <~ "elaboration" ^^ { p => Elaboration(p) }

  private def component = (path <~ "component") ~ str ^^ { case p ~ s => Component(p, ComponentKey.parse(s)) }
  
  private def get = path ^^ { p => Get(p) }

  private def windowaction = windowclose | windowpos | gui

  private def windowclose = "window" ~> str <~ "close" ^^ { s => WindowClose(s) }

  private def windowpos = ("window" ~> str <~ "position") ~ int ~ int ^^ { case s ~ x ~ y => WindowPosition(s, x, y) }

  private def gui = "gui" ~> ("on" | "off") ^^ { s => BrowserAction(s) }

  /* common non-terminals */
  private def path = str ^^ { s => Path.parse(s, nsMap) }

  private def mpath = str ^^ { s => Path.parseM(s, nsMap) }

  // [str_1,...,str_n] or str
  private def stringList = ("\\[.*\\]" r) ^^ { s => stringToList(s.substring(1, s.length - 1), ",")} |
    str ^^ { s => List(s) }

  private def file = str ^^ { s => File(home.resolve(s)) }

  private def uri = str ^^ { s => URI(s) }

  private def int = str ^^ { s => s.toInt }

  private def strMaybeQuoted = quotedStr | str

  /** regular expression for non-empty word without whitespace */
  private def str = "\\S+" r

  /** regular expression for quoted string (that may contain whitespace) */
  private def quotedStr = ("\".*\"" r) ^^ { s => s.substring(1, s.length - 1) }

  /** build modifiers */
  private def keyMod = str ^^ { case km =>
    if (km.startsWith("-"))
      (km.tail, Clean)
    else if ("*!&".contains(km.last))
      (km.init, km.last match {
        case '!' => UpdateOnError(Level.Error)
        case '&' => BuildDepsFirst(UpdateOnError(Level.Error))
        case _ => UpdateOnError(Level.Ignore)
      })
    else (km, Build)
  }

  /** build modifiers */
  // the old keyMod must be kept for compatibility reasons of .msl files
  private def buildModifier =
    ("--force" | "--onChange" | "--onError\\??(=\\d)?".r | "--clean" | "--depsFirst\\??(=\\d)?".r) ^^ {
      case "--force" => Build
      case "--clean" => Clean
      case "--onError" => UpdateOnError(Level.Error)
      case "--depsFirst" => BuildDepsFirst(UpdateOnError(Level.Error))
      case "--onChange" => UpdateOnError(Level.Ignore)
      case s =>
        val c = s.last
        val up = if (c == '?') UpdateOnError(Level.Error, true)
        else UpdateOnError(c.asDigit - 1, s.contains('?')) // 0 => force
        if (s.startsWith("--depsFirst")) BuildDepsFirst(up)
        else up
    }

  /** parses an action from a string, relative to a base path */
  def parseAct(s: String, b: Path, h: File): Action = {
    nsMap = NamespaceMap(b)
    home = h
    val p =
      try {
        parseAll(commented, s)
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
}

/** Objects of type Action represent commands that can be executed by a Controller.
  *
  * In particular, these are the commands available in the MMT shell.
  * The semantics of Actions is implemented in Controller.handle, which must contain one case for every Action.
  *
  * Each subclass documents its concrete syntax and its semantics (relative to an instance "controller" of Controller).
  * The syntax uses the following basic data types:
  * FILE a /-separated file name, relative to controller.home
  * URI a URI, relative to controller.base
  * INT an integer
  * STRING a non-empty string without whitespace
  * CLASS a string representing a Java-style URI of a class, e.g., info.kwarc.mmt.api.frontend.Action
  * [TYPE] for optional arguments
  * TYPE* for whitespace-separated lists of arguments
  * If such a type is listed as id:TYPE, then id is the name of the constructor argument of the Action, into which the concrete argument is parsed.
  */
sealed abstract class Action

case class Compare(p: Path, r: Int) extends Action {
  override def toString = "diff " + p.toPath + ":" + r.toString
}

/** add a log handler
  *
  * concrete syntax: log file FILE
  * or: log console
  *
  * @param h log handler
  */
case class AddReportHandler(h: ReportHandler) extends Action {
  override def toString = "log " + h.toString
}

/** switch on logging for a certain group
  *
  * concrete syntax: log+ group:STRING
  * Some of the available groups are documented in Report. Generally, plugins may use their own groups.
  */
case class LoggingOn(group: String) extends Action {
  override def toString = "log+ " + group
}

/** switch off logging for a certain group
  *
  * concrete syntax: log- group:STRING
  * Some of the available groups are documented in Report. Generally, plugins may use their own groups.
  */
case class LoggingOff(group: String) extends Action {
  override def toString = "log- " + group
}

/** set the current base path
  *
  * concrete syntax: base base:URI
  */
case class SetBase(base: Path) extends Action {
  override def toString = "base " + base
}

/** set an environment variable
  *
  * concrete syntax: envvar name "value"
  */
case class SetEnvVar(name: String, value: String) extends Action {
  override def toString = s"""envvar $name "$value""""
}

/** load a file containing commands and execute them, fails on first error if any
  *
  * concrete syntax: file file:FILE
  */
case class ExecFile(file: File, name: Option[String]) extends Action {
  override def toString = "file " + file + name.map(" " +).getOrElse("")
}

/** bind all following commands to a name without executing them
  *
  * the folder of the containing msl file provides the namespace of this binding
  * concrete syntax: define name:STRING
  */
case class Define(name: String) extends Action {
  override def toString = "define " + name
}

/** ends a [[Define]]
  *
  * concrete syntax: end
  */
case object EndDefine extends Action {
  override def toString = "end"
}

/** run a previously named list of commands
  *
  * concrete syntax: do [folder:FILE] name:STRING
  * if folder is omitted, this refers to the most recently defined action with the right name
  */
case class Do(file: Option[File], name: String) extends Action {
  override def toString = "do " + file.getOrElse("") + " " + name
}

/** stores a command binding done with [[Define]] */
case class Defined(file: File, name: String, body: List[Action])

case class Graph(target: File) extends Action {
  override def toString = "graph " + target
}

/** read a file containing MMT in OMDoc syntax
  *
  * concrete syntax: read file:FILE
  */
case class Read(file: File, interpret: Boolean) extends Action {
  override def toString = (if (interpret) "interpret " else "read ") + file
}

/** check a knowledge item with respect to a certain checker */
case class Check(p: Path, id: String) extends Action {
  override def toString = s"check $p $id"
}

/** navigate to knowledge item */
case class Navigate(p: Path) extends Action {
  override def toString = "navigate " + p
}

/** add a catalog entry for the local file system
  *
  * All URIs of the form file:///SUFFIX are mapped to SUFFIX
  *
  * concrete syntax: mathpath local
  */
case object Local extends Action {
  override def toString = "mathpath local"
}

/** add catalog entry for a local directory
  *
  * All URIs of the form uri/SUFFIX are mapped to file/SUFFIX
  *
  * concrete syntax: mathpath fs uri:URI file:FILE
  *
  * @param uri the logical identifier of the directory
  * @param file the physical identifeir of the directory
  */
case class AddMathPathFS(uri: URI, file: File) extends Action {
  override def toString = "mathpath fs " + uri + " " + file
}

/** add catalog entry for realizations in Java
  *
  * @param javapath the Java path entry, will be passed to [[java.net.URLClassLoader]]
  */
case class AddMathPathJava(javapath: File) extends Action {
  override def toString = "mathpath java " + javapath
}

/** clone an archive from a remote OAF
  *
  * concrete syntax: oaf close path:STRING
  */
case class OAFInit(path: String) extends Action {
  override def toString = "oaf init " + path
}

/** clone an archive from a remote OAF
  *
  * concrete syntax: oaf close path:STRING
  */
case class OAFClone(path: String) extends Action {
  override def toString = "oaf clone " + path
}

/** pulls all repositories from remote OAF
  *
  * concrete syntax: oaf pull
  */
case object OAFPull extends Action {
  override def toString = "oaf pull"
}

/** pushes all repositories to remote OAF
  *
  * concrete syntax: oaf push
  */
case object OAFPush extends Action {
  override def toString = "oaf push"
}

/** registers a compiler
  *
  * concrete syntax: importer cls:CLASS args:STRING*
  *
  * @param cls the name of a class implementing Compiler, e.g., "info.kwarc.mmt.api.lf.Twelf"
  * @param args a list of arguments that will be passed to the compiler's init method
  */
case class AddExtension(cls: String, args: List[String]) extends Action {
  override def toString = "extension " + cls + args.map(" " +).mkString
}

/** add catalog entries for a set of local copies, based on a file in Locutor registry syntax */
case class AddArchive(folder: java.io.File) extends Action {
  override def toString = "mathpath archive " + folder
}

/** builds a dimension in a previously opened archive */
case class ArchiveBuild(ids: List[String], dim: String, modifier: BuildTargetModifier, in: FilePath = EmptyPath) extends Action {
  override def toString = "build " + MyList(ids).mkString("[", ",", "]") + " " + modifier.toString(dim) +
    (if (in.segments.isEmpty) "" else " " + in)
}

/** handle the make command line */
case class MakeAction(key: String, args: List[String]) extends Action {
  override def toString = "make" + args.map(" " + _).mkString
}

/** builds a dimension in a previously opened archive */
case class ArchiveMar(id: String, file: File) extends Action {
  override def toString = s"archive $id mar $file"
}

/** add MathWebSearch as a web service */
case class AddMWS(uri: URI) extends Action {
  override def toString = "mws " + uri
}

/** print all loaded knowledge items to STDOUT in text syntax */
case object PrintAll extends Action

/** print all loaded knowledge items to STDOUT in XML syntax */
case object PrintAllXML extends Action

/** print all configuration entries to STDOUT*/
case object PrintConfig extends Action {
   override def toString = "printConfig"
}

/** run a Scala interpreter or evaluate a Scala expression */
case class Scala(init: Option[String]) extends Action {
  override def toString = "scala"
}

/** run an .mbt file */
case class MBT(file: File) extends Action {
  override def toString = "mbt " + file
}

/** start up the HTTP server
  *
  * concrete syntax: server on port:INT
  *
  * See info.kwarc.mmt.api.web.Server for the supported HTTP requests.
  * tiscaf.jar must be on the Java classpath before executing this Action.
  *
  * @param port the port to listen to
  */
case class ServerOn(port: Int) extends Action {
  override def toString = "server on " + port
}

/** shut down the web server
  *
  * concrete syntax: server off
  */
case object ServerOff extends Action {
  override def toString: String = "server off"
}

/** clear the state
  *
  * concrete syntax: clear
  */
case object Clear extends Action {
  override def toString: String = "clear"
}

/** release all resources and exit
  *
  * concrete syntax: exit
  */
case object Exit extends Action {
  override def toString: String = "exit"
}

/** do nothing */
case object NoAction extends Action {
  override def toString: String = ""
}

/** close a window with a given ID
  *
  * See ToWindow on how to open windows
  * concrete syntax: window window:STRING close
  */
case class WindowClose(window: String) extends Action {
  override def toString: String = "window " + window + " close"
}

/** position a window with a given ID
  *
  * See ToWindow on how to open windows
  * concrete syntax: window window:STRING position x:INT y:INT
  */
case class WindowPosition(window: String, x: Int, y: Int) extends Action {
  override def toString: String = "window " + window + " position " + x + " " + y
}

/** send a browser command
  *
  * @param command on or off
  */
case class BrowserAction(command: String) extends Action {
  override def toString: String = "gui " + command
}

/** Objects of type GetAction represent commands
  *
  * that
  * - retrieve knowledge items in abstract/internal syntax
  * - post-process them to obtain concrete/external syntax
  * - then output the concrete form in various ways.
  *
  * The concrete syntax is described by the following grammar:
  * get ABSTRACT [CONCRETE] [OUTPUT]
  * where
  * ABSTRACT ::= URI | URI component STRING | URI closure | URI elaboration
  * CONCRETE ::= present param | deps
  * OUTPUT   ::= write FILE | window | respond | CONCRETE
  * The productions for ABSTRACT, CONCRETE, OUTPUT correspond to the instances of MakeAbstract, MakeConcrete, and Output.
  *
  * @param o the instance of Output that executes all three steps.
  */
case class GetAction(o: Output) extends Action {
  /** implement the Action using the provided Controller */
  def make(controller: Controller): Unit = o.make(controller)

  override def toString: String = o.toString
}

/** represent retrieval operations that return content elements
  *
  * These objects become Action commands by wrapping them in post-processing steps.
  * see the children of MakePresentation
  */
abstract class MakeAbstract {
  def make(controller: Controller): Content
}

/** retrieves a knowledge item */
case class Get(p: Path) extends MakeAbstract {
  def make(controller: Controller): StructuralElement = {
    controller.get(p)
  }

  override def toString: String = p.toString
}

/** retrieves a component of a knowledge item */
case class Component(p: Path, comp: ComponentKey) extends MakeAbstract {
  def make(controller: Controller): Content = {
    val o = controller.get(p)
    val tOpt = o.getComponent(comp) flatMap {
      case c: AbstractTermContainer => c.get
      case _ => throw ParseError("component name " + comp + " not a term component in " + o)
    }
    tOpt.getOrElse {
      throw ParseError("component name " + comp + " illegal for element " + o)
    }
  }

  override def toString: String = p + " component " + comp
}

/** retrieves the closure of a knowledge item */
case class Closure(p: Path) extends MakeAbstract {
  def make(controller: Controller): Document = p match {
    case doc ? name =>
      controller.get(doc ? name) // retrieve once to make sure it's in memory
      val cl = controller.depstore.theoryClosure(doc ? name)
      val clp = cl.map{p => MRef(doc, p)}
      new Document(doc, root = true, inititems = clp)
  }

  override def toString: String = p + " closure"
}

/** retrieves the elaboration of an instance */
case class Elaboration(p: Path) extends MakeAbstract {
  def make(controller: Controller) = {
    controller.get(p) match {
      //case i : Instance => new TGroup(i.parent,p,Instance.elaborate(i)) //TODO
      case _ => throw ImplementationError("Non-instance element at " + p)
    }
  }

  override def toString: String = p + " elaboration"
}

/** represents the first post-processing phase
  *
  * These produce concrete syntax from the abstract syntax.
  */
abstract class MakeConcrete {
  /** takes a Controller, executes the rendering and passes it to a RenderingHandler */
  def make(controller: Controller, rb: RenderingHandler)
}

/** takes a content element and renders it using notations */
case class Present(c: MakeAbstract, param: String) extends MakeConcrete {
  def make(controller: Controller, rb: RenderingHandler) {
    val presenter = controller.extman.get(classOf[presentation.Presenter], param).getOrElse {
      throw PresentationError("no presenter found: " + param)
    }
    c.make(controller) match {
      case s: StructuralElement => presenter(s)(rb)
      case o: Obj => presenter(o, None)(rb)
    }
  }

  override def toString: String = c + " present " + param
}

/** retrieves all relational elements about a certain path and renders them as XML */
case class Deps(path: Path) extends MakeConcrete {
  def make(controller: Controller, rb: RenderingHandler) {
    rb.beginTag("", "mmtabox")
    rb.writeAttribute("", "xmlns", "http://omdoc.org/abox")
    rb.finishTag()
    (controller.depstore.getInds ++ controller.depstore.getDeps).foreach(
      (d: ontology.RelationalElement) => if (path <= d.path) rb(d.toNode)
    )
    rb.writeEndTag("", "mmtabox")
  }

  override def toString: String = path.toString + " deps"
}

/** represents the final post-processing phase, which outputs concrete syntax */
abstract class Output {
  def make(controller: Controller)
}

/** prints to STDOUT */
case class Print(pres: MakeConcrete) extends Output {
  def make(controller: Controller) {
    pres.make(controller, ConsoleWriter)
    ConsoleWriter("\n")
  }

  override def toString: String = pres.toString
}

/** writes to a file */
case class ToFile(pres: MakeConcrete, file: java.io.File) extends Output {
  def make(controller: Controller) {
    val rb = new presentation.FileWriter(file)
    pres.make(controller, rb)
    rb.done
  }

  override def toString: String = pres + " write " + file
}

/** displays content in a window */
case class ToWindow(pres: MakeConcrete, window: String) extends Output {
  def make(controller: Controller) {
    val rb = new StringBuilder
    pres.make(controller, rb)
    val res = rb.get
    controller.winman.getWindow(window).set(res)
  }

  override def toString: String = pres + " window " + window
}

/** produces the result and throws it away
  *
  * call get to keep it in memory and retrieve it
  */
case class Respond(pres: MakeConcrete) extends Output {
  def get(controller: Controller): String = {
    val rb = new StringBuilder
    pres.make(controller, rb)
    rb.get
  }

  def make(controller: Controller) {
    get(controller)
  }

  override def toString: String = pres.toString + " respond"
}
