package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import modules.DeclaredTheory
import objects._
import parser._
import presentation._
import documents._
import patterns._
import symbols._
import utils._
import utils.FileConversion._

import scala.util.parsing.combinator._

/** helper object for Actions
 * This object provides a combinator parser for Actions that is used when commands are sent to a controller as strings.
 * It is straightforward to understand the concrete syntax from the source code.
 */
object Action extends RegexParsers {
   private var base : Path = null
   private var home : File = null
   
   private def commented = (comment ^^ {c => NoAction}) | (action ~ opt(comment) ^^ {case a ~ _ => a}) | empty ^^ {_ => NoAction}
   private def empty = "\\s*"r
   private def comment = "//.*"r
   private def action = controller | shell | getaction

   private def controller = log | mathpath | archive | extension | mws | server | windowaction | execfile | scala
   private def log = logfile | logconsole | logon | logoff
     private def logfile = "log file" ~> file ^^ {f => AddReportHandler(new FileHandler(f))}
     private def logconsole = "log console" ^^ {case _ => AddReportHandler(ConsoleHandler)}
     private def logon = "log+" ~> str ^^ {s => LoggingOn(s)}
     private def logoff = "log-" ~> str ^^ {s => LoggingOff(s)}
   private def mathpath = "mathpath" ~> (mathpathLocal | mathpathFS | mathpathSVN | mathpathTNT)
     private def mathpathLocal = "local" ^^ {case _ => Local}
     private def mathpathFS = "fs" ~> uri ~ file ^^ {case u ~ f => AddMathPathFS(u,f)}
     private def mathpathSVN = "svn" ~> uri ~ int ~ (str ?) ~ (str ?) ^^ {case uri ~ rev ~ user ~ pass => AddMathPathSVN(uri, rev, user, pass)}
     private def mathpathTNT = "tntbase" ~> file ^^ {f => AddTNTBase(f)}
   private def archive = archopen | archdim | archmar | archpres | svnarchopen | archbuild
     private def archopen = "archive" ~> "add" ~> file ^^ {f => AddArchive(f)}
     private def svnarchopen = "SVNArchive" ~> "add" ~> str ~ int ^^ {case url ~ rev => AddSVNArchive(url,rev)}
     private def archbuild = "build" ~> str ~ str ~ (str ?) ~ (str *) ^^ {
       case id ~ keymod ~ in ~ args =>
            val segs = MyList.fromString(in.getOrElse(""), "/")
            val (key,mod) = if (keymod.startsWith("-"))
               (keymod.substring(1), archives.Clean)
            else if (keymod.endsWith("*"))
               (keymod.substring(0,keymod.length-1), archives.Update)
            else
               (keymod, archives.Build)
            ArchiveBuild(id, key, mod, segs, args)
     }
     private def archdim = "archive" ~> str ~ dimension ~ (str ?) ^^ {
       case id ~ dim ~ s =>
            val segs = MyList.fromString(s.getOrElse(""), "/")
            ArchiveBuild(id, dim, archives.Build, segs)
     }
     private def archpres = "archive" ~> str ~ ("present" ~> str) ~ (str ?) ^^ { 
        case id ~ p ~ s =>
           val segs = MyList.fromString(s.getOrElse(""), "/")
           ArchiveBuild(id, "present", archives.Build, segs, List(p))
        }
     private def dimension = "check" | "validate" | "mws-flat" | "mws-enriched" | "mws" | "flat" | "enrich" |
           "relational" | "notation" | "source-terms" | "source-structure" | "delete" | "clean" | "extract" | "integrate" | "register" | "test" | "close"
     private def archmar = "archive" ~> str ~ ("mar" ~> file) ^^ {case id ~ trg => ArchiveMar(id, trg)}
   private def extension = "extension" ~> str ~ (str *) ^^ {case c ~ args => AddExtension(c, args)}
   private def mws = "mws" ~> uri ^^ {u => AddMWS(u)}
   private def server = serveron | serveroff
     private def serveron = "server" ~> "on" ~> int ^^ {i => ServerOn(i)}
     private def serveroff = "server" ~> "off" ^^ {_ => ServerOff}
   private def execfile = "file " ~> file ^^ {f => ExecFile(f)}
   private def scala = "scala" ^^ {_ => Scala}

   private def shell = setbase | read | graph | check | printall | printallxml | clear | exit
   private def setbase = "base" ~> path ^^ {p => SetBase(p)}
   private def read = "read" ~> file ^^ {f => Read(f)}
   private def graph = "graph" ~> file ^^ {f => Graph(f)}
   private def check = "check" ~> path ^^ {p => Check(p)}
   private def printall = "printAll" ^^ {case _ => PrintAll}
   private def printallxml = "printXML" ^^ {case _ => PrintAllXML}
   private def clear = "clear" ^^ {case _ => Clear}
   private def exit = "exit" ^^ {case _ => Exit}

   private def getaction = diff | tofile | towindow | respond | print | defaultget 
   private def diff = path ~ ("diff" ~> int) ^^ {case p ~ i => Compare(p, i)}
   private def tofile = presentation ~ ("write" ~> file) ^^ {case p ~ f => GetAction(ToFile(p,f))}
   private def towindow = presentation ~ ("window" ~> str) ^^ {case p ~ w => GetAction(ToWindow(p,w))}
   private def print = presentation <~ "print" ^^ {p => GetAction(Print(p))}
   private def defaultget = presentation ^^ {p => DefaultGet(p)}
   private def respond = (presentation <~ "respond") ~ str ^^ {case p ~ s => GetAction(Respond(p,s))}
   private def presentation = present | deps | defaultPresent
   private def present = content ~ ("present" ~> str) ^^ {case c ~ p => Present(c,p)}
   private def deps = path <~ "deps" ^^ {case p => Deps(p)}
   private def defaultPresent = content ^^ {c => Present(c, "text")}
   private def content = closure | elaboration | component | get
   private def closure = path <~ "closure" ^^ {p => Closure(p)}
   private def elaboration = path <~ "elaboration" ^^ {p => Elaboration(p)}   
   private def component = (path <~ "component") ~ str ^^ {case p ~ s => Component(p, s)}
   private def get = path ^^ {p => Get(p)}
   
   private def windowaction = windowclose | windowpos | browser
   private def windowclose = "window" ~> str <~ "close" ^^ {s => WindowClose(s)}
   private def windowpos   = ("window" ~> str <~ "position") ~ int ~ int ^^ {case s ~ x ~ y => WindowPosition(s, x, y)}
   private def browser = "browser" ~> ("on" | "off") ^^ {s => BrowserAction(s)}
   
   private def path = str ^^ {s => Path.parse(s, base)}
   private def mpath = str ^^ {s => Path.parseM(s, base)}
   private def file = str ^^ {s => File(home.resolve(s))}
   private def uri = str ^^ {s => URI(s)}
   private def int = str ^^ {s => s.toInt}
   private def str = "\\S+"r        //regular expression for non-empty word without whitespace
   /** parses an action from a string, relative to a base path */
   def parseAct(s:String, b : Path, h: File) : Action = {
      base = b
      home = h
      val p = parseAll(commented,s)
      p match {
         case Success(tree, _) => tree
         case e: NoSuccess => throw ParseError(s + "\n  error: " + e.msg)
      }
   }
}

/** Objects of type Action represent commands that can be executed by a Controller.
 * In particular, these are the commands available in the MMT shell.
 * The semantics of Actions is implemented in Controller.handle, which must contain one case for every Action.
 * 
 * Each subclass documents its concrete syntax and its semantics (relative to an instance "controller" of Controller).
 * The syntax uses the following basic data types:
 *  FILE a /-separated file name, relative to controller.home
 *  URI a URI, relative to controller.base
 *  INT an integer
 *  STRING a non-empty string without whitespace
 *  CLASS a string representing a Java-style URI of a class, e.g., info.kwarc.mmt.api.frontend.Action
 *  [TYPE] for optional arguments
 *  TYPE* for whitespace-separated lists of arguments
 * If such a type is listed as id:TYPE, then id is the name of the constructor argument of the Action, into which the concrete argument is parsed.
 */
sealed abstract class Action

case class Compare(p : Path, r : Int) extends Action{override def toString = "diff " + p.toPath + ":" + r.toString}

/** add a log handler
 * @param the log handler
 * concrete syntax:
 *  log file FILE
 *  or
 *  log console
 */
case class AddReportHandler(h : ReportHandler) extends Action {
   override def toString = "log " + h.toString
 }

/** switch on logging for a certain group
 * concrete syntax: log+ group:STRING
 * Some of the available groups are documented in Report. Generally, plugins may use their own groups. 
 */
case class LoggingOn(group : String) extends Action {override def toString = "log+ " + group}
/** switch off logging for a certain group
 * concrete syntax: log- group:STRING
 * Some of the available groups are documented in Report. Generally, plugins may use their own groups. 
 */
case class LoggingOff(group : String) extends Action {override def toString = "log- " + group}

/** set the current base path
 * concrete syntax: base base:URI
 */
case class SetBase(base : Path) extends Action {override def toString = "base " + base}

/** load a file containing commands and execute them, fails on first error if any
 * concrete syntax: file file:FILE
 */
case class ExecFile(file : File) extends Action {override def toString = "file " + file}

case class Graph(target : File) extends Action {override def toString = "graph " + target}

/** read a file containing MMT in OMDoc syntax
 * concrete syntax: read file:FILE
 */
case class Read(file : File) extends Action {override def toString = "read " + file}

/** check a knowledge item */
case class Check(p : Path) extends Action {override def toString = "check " + p}

/** add a catalog entry for the local file system
 * All URIs of the form file:///SUFFIX are mapped to SUFFIX
 * 
 * concrete syntax: mathpath local
 */
case object Local extends Action {override def toString = "mathpath local"}

/** add catalog entry for a local directory
 * @param uri the logical identifier of the directory
 * @param file the physical identifeir of the directory
 * All URIs of the form uri/SUFFIX are mapped to file/SUFFIX
 * 
 * concrete syntax: mathpath fs uri:URI file:FILE
 */
case class AddMathPathFS(uri: URI, file : File) extends Action {override def toString = "mathpath fs " + uri + " " + file}

/** add catalog entry for a remote SVN repository
 * @param uri URI the remote URI
 * @param rev the revision to use, -1 for head
 * @param user user name to use, if any
 * @param password password to use if any
 * All URIs of the form uri/SUFFIX are looked in the repository
 * 
 * concrete syntax: mathpath svn uri:URI rev:INT user:[STRING] password:[STRING]
 */
case class AddMathPathSVN(uri: URI, rev: Int, user: Option[String], password: Option[String]) extends Action {
   override def toString = "mathpath svn " + uri +
      (if (rev == -1) "" else " " + rev) +
      (user.map(" " + _).getOrElse("") + password.map(" " + _).getOrElse(""))
}

/** add a catalog entry for an MMT-aware database such as TNTBase
 *  @param file the configuration file for TNTBase access
 *  Mapping as specified by the configuration file.
 *  
 *  concrete syntax: mathpath tntbase file:FILE
 */
case class AddTNTBase(file : File) extends Action {override def toString = "tntbase " + file}

/** registers a compiler
 * @param cls the name of a class implementing Compiler, e.g., "info.kwarc.mmt.api.lf.Twelf"
 * @param args a list of arguments that will be passed to the compiler's init method
 * 
 * concrete syntax: importer cls:CLASS args:STRING*
 */
case class AddExtension(cls: String, args: List[String]) extends Action {override def toString = "extension " + cls + args.mkString(" ", " ", "")}

/** add catalog entries for a set of local copies, based on a file in Locutor registry syntax */
case class AddArchive(folder : java.io.File) extends Action {override def toString = "archive add " + folder}

/** add a SVN Archive */
case class AddSVNArchive(url : String,  rev : Int) extends Action {override def toString = "SVN archive add " + url + "@" + rev}

/** builds a dimension in a previously opened archive */
case class ArchiveBuild(id: String, dim: String, modifier: archives.BuildTargetModifier, in : List[String], params: List[String] = Nil) extends Action {
   override def toString = "archive " + id + " " + modifier.toString(dim) + " " + in.mkString(" ","/","")
}

/** builds a dimension in a previously opened archive */
case class ArchiveMar(id: String, file: File) extends Action {override def toString = s"archive $id mar $file"}

/** add MathWebSearch as a web service */
case class AddMWS(uri: URI) extends Action {override def toString = "mws " + uri}

/** print all loaded knowledge items to STDOUT in text syntax */
case object PrintAll extends Action

/** print all loaded knowledge items to STDOUT in XML syntax */
case object PrintAllXML extends Action

case object Scala extends Action {override def toString = "scala"}

/** start up the HTTP server
 * @param port the port to listen to
 * concrete syntax: server on port:INT
 * 
 * See info.kwarc.mmt.api.web.Server for the supported HTTP requests.
 * tiscaf.jar must be on the Java classpath before executing this Action.
 */
case class ServerOn(port: Int) extends Action {override def toString = "server on " + port}

/** shut down the web server
 * concrete syntax: server off
 */
case object ServerOff extends Action {override def toString = "server off"}

/** clear the state
 * concrete syntax: clear
 */
case object Clear extends Action {override def toString = "clear"}

/** release all resources and exit
 * concrete syntax: exit
 */
case object Exit extends Action {override def toString = "exit"}

/** do nothing */
case object NoAction extends Action {override def toString = ""}

/** close a window with a given ID
 * See ToWindow on how to open windows
 * concrete syntax: window window:STRING close
 */
case class WindowClose(window: String) extends Action {
  override def toString = "window " + window + " close"  
}

/** position a window with a given ID
 * See ToWindow on how to open windows
 * concrete syntax: window window:STRING position x:INT y:INT */
case class WindowPosition(window: String, x:Int, y: Int) extends Action {
  override def toString = "window " + window + " position " + x + " " + y  
}

/** send a browser command
 * @param command on or off
 */
case class BrowserAction(command: String) extends Action {
  override def toString = "browser " + command
}

/** Objects of type GetAction represent commands that
 *  - retrieve knowledge items in abstract/internal syntax
 *  - post-process them to obtain concrete/external syntax
 *  - then output the concrete form in various ways.
 *  @param o the instance of Output that executes all three steps.
 *  
 *  The concrete syntax is described by the following grammar:
 *  ABSTRACT [CONCRETE] [OUTPUT]
 *  where
 *  ABSTRACT ::= URI | URI component STRING | URI closure | URI elaboration
 *  CONCRETE ::= xml | present URI | text | deps
 *  OUTPUT   ::= write FILE | print | window | respond
 *  The productions for ABSTRACT, CONCRETE, OUTPUT correspond to
 *  the instances of MakeAbstract, MakeConcrete, and Output.
 */ 
case class GetAction(o: Output) extends Action {
   /** implement the Action using the provided Controller */
   def make(controller : Controller) = o.make(controller)
}

/** execute the Controller-specific default Action associated with a presentable element
 * the case where OUTPUT is not specifified
 */
case class DefaultGet(pres : MakeConcrete) extends Action {
   override def toString = pres.toString
}

/** represent retrieval operations that return content elements
 *  These objects become Action commands by wrapping them in post-processing steps.
 *  see the children of MakePresentation
 *  
 *  concrete syntax: 
 */
abstract class MakeAbstract {
   def make(controller : Controller) : Content
}
/** retrieves a knowledge item */
case class Get(p : Path) extends MakeAbstract {
   def make(controller : Controller) : StructuralElement = {
     controller.get(p)
   }
   override def toString = p.toString
}
/** retrieves a component of a knowledge item */
case class Component(p : Path, comp : String) extends MakeAbstract {
   def make(controller : Controller) : Content = {
      val o = controller.get(p)
      o.role.componentByName(comp) match {
         case Some(c) => o.components(c)
         case None => throw ParseError("component name " + comp + " illegal for element with role " + o.role)
      }
   }
   override def toString = p + " component " + comp
}
/** retrieves the closure of a knowledge item */
case class Closure(p : Path) extends MakeAbstract {
   def make(controller : Controller) : Document = p match {
      case doc ? name =>
         controller.get(doc ? name) // retrieve once to make sure it's in memory 
         val cl = controller.depstore.theoryClosure(doc ? name).toList
         val clp = cl.map(MRef(doc, _, true))
         new Document(doc, clp)
   }
   override def toString = p + " closure"
}
/** retrieves the elaboration of an instance */
case class Elaboration(p : Path) extends MakeAbstract {
   def make(controller : Controller) : TGroup = {
	   controller.get(p) match {
	  	   //case i : Instance => new TGroup(i.parent,p,Instance.elaborate(i)) //TODO
	  	   case _ => throw ImplementationError("Non-instance element at " + p)  
       }
   }
   override def toString = p + " elaboration"
}

/** represents the first post-processing phase
 *  These produce concrete syntax from the abstract syntax.
 * */
abstract class MakeConcrete {
   /** takes a Controller, executes the rendering and passes it to a RenderingHandler */
   def make(controller : Controller, rb : RenderingHandler)
}
/** takes a content element and renders it as XML */
case class ToNode(c : MakeAbstract) extends MakeConcrete {
   def make(controller : Controller, rb : RenderingHandler) {rb(c.make(controller).toNode)}
   override def toString = c + " xml"
}
/** takes a content element and renders it as text */
case class ToString(c : MakeAbstract) extends MakeConcrete {
   def make(controller : Controller, rb : RenderingHandler) {rb(c.make(controller).toString)}
   override def toString = c.toString
}

/** takes a content element and renders it using notations */
case class Present(c : MakeAbstract, param : String) extends MakeConcrete {
   def make(controller : Controller, rb : RenderingHandler) {
      val presenter = controller.extman.getPresenter(param) getOrElse {
         val nset = Path.parseM(param, controller.getBase)
         new StyleBasedPresenter(controller, nset)
      }
      c.make(controller) match {
         case s: StructuralElement => presenter(s, rb)
         case o: Obj => presenter(o, rb)
      }
   }
   override def toString = c + " present " + param
}


/** retrieves all relational elements about a certain path and renders them as XML */
case class Deps(path : Path) extends MakeConcrete {
   def make(controller : Controller, rb : RenderingHandler) {
     rb.elementStart("","mmtabox")
     rb.attributeStart("", "xmlns")
     rb("http://omdoc.org/abox")
     rb.attributeEnd
     (controller.depstore.getInds ++ controller.depstore.getDeps).foreach(
         (d : RelationalElement) => if (path <= d.path) rb(d.toNode)
     )
     rb.elementEnd
   }
   override def toString = path.toString + " deps"
}

/** represents the final post-processing phase, which outputs concrete syntax */
abstract class Output {
   def make(controller : Controller)
}
/** prints to STDOUT */
case class Print(pres : MakeConcrete) extends Output {
   def make(controller : Controller) {
      pres.make(controller, ConsoleWriter)
   }
   override def toString = pres.toString
}
/** writes to a file */
case class ToFile(pres : MakeConcrete, file : java.io.File) extends Output {
   def make(controller : Controller) {
      val rb = new presentation.FileWriter(file)
      pres.make(controller, rb)
      rb.done
   }
   override def toString = pres + " write " + file
}
/** displays content in a window */
case class ToWindow(pres : MakeConcrete, window: String) extends Output {
   def make(controller : Controller) {
      val rb = new XMLBuilder
      pres.make(controller, rb)
      val res = rb.get
      controller.winman.getWindow(window).set(res.toString)
   }
   override def toString = pres + " window " + window
}
/** produces the result and throws it away
 *  call get to keep it in memory and retrieve it
 */
case class Respond(pres : MakeConcrete, param : String) extends Output {
   def get(controller : Controller) : scala.xml.Node = {
      val rb = new XMLBuilder // TODO try a to-be-written StringBuilder instead of XMLBuilder for speed
      pres.make(controller, rb)
      rb.get
   }
   def make (controller : Controller) {get(controller)}
   override def toString = pres.toString + " respond" + (if (param == "") "" else " " + param)
}
