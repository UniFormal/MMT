package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
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
   private def controller = logon | logoff | local | catalog | archive | tntbase | importer | foundation | mws | server | execfile
   private def shell = setbase | read | printall | printallxml | clear | exit
   private def logon = "log+" ~> str ^^ {s => LoggingOn(s)}
   private def logoff = "log-" ~> str ^^ {s => LoggingOff(s)}
   private def local = "local" ^^ {case _ => Local}
   private def catalog = "catalog" ~> file ^^ {f => AddCatalog(f)}
   private def archive = archopen | archdim | archmar
   private def archopen = "archive" ~> "add" ~> file ^^ {f => AddArchive(f)}
   private def archdim = "archive" ~> str ~ dimension ~ (str ?) ^^ {
      case id ~ dim ~ s =>
         val segs = MyList.fromString(s.getOrElse(""), "/")
         ArchiveBuild(id, dim, segs)
   }
   private def dimension = "compile" | "content" | "check" | "mws-flat" | "mws" | "flat" | "relational" | "notation" | "delete" | "extract" | "integrate"
   private def archmar = "archive" ~> str ~ ("mar" ~> file) ^^ {case id ~ trg => ArchiveMar(id, trg)}
   private def tntbase = "tntbase" ~> file ^^ {f => AddTNTBase(f)}
   private def importer = "importer" ~> str ~ (str *) ^^ {case c ~ args => AddImporter(c, args)}
   private def foundation = "foundation" ~> str ~ (str *) ^^ {case f ~ args => AddFoundation(f, args)}
   private def mws = "mws" ~> uri ^^ {u => AddMWS(u)}
   private def server = serveron | serveroff
   private def serveron = "server" ~> "on" ~> int ^^ {i => ServerOn(i)}
   private def serveroff = "server" ~> "off" ^^ {_ => ServerOff}
   private def execfile = "file " ~> file ^^ {f => ExecFile(f)}
   private def setbase = "base" ~> path ^^ {p => SetBase(p)}
   private def read = "read" ~> file ^^ {f => Read(f)}
   private def printall = "printAll" ^^ {case _ => PrintAll}
   private def printallxml = "printXML" ^^ {case _ => PrintAllXML}
   private def clear = "clear" ^^ {case _ => Clear}
   private def exit = "exit" ^^ {case _ => Exit}
   private def getaction = tofile | respond | print | defaultget
   private def tofile = presentation ~ ("write" ~> file) ^^ {case p ~ f => GetAction(ToFile(p,f))}
   private def print = presentation <~ "print" ^^ {p => GetAction(Print(p))}
   private def defaultget = presentation ^^ {p => DefaultGet(p)}
   private def respond = (presentation <~ "respond") ~ str ^^ {case p ~ s => GetAction(Respond(p,s))}
   private def presentation = present | tonode | deps | tostring
   private def tonode = content <~ "xml" ^^ {c => ToNode(c)}
   private def present = content ~ ("present" ~> mpath) ^^ {case c ~ p => Present(c,p)}
   private def deps = path <~ "deps" ^^ {case p => Deps(p)}
   private def tostring = content ^^ {c => ToString(c)}
   private def content = closure | elaboration | component | get
   private def closure = path <~ "closure" ^^ {p => Closure(p)}
   private def elaboration = path <~ "elaboration" ^^ {p => Elaboration(p)}   
   private def component = (path <~ "component") ~ str ^^ {case p ~ s => Component(p, s)}
   private def get = path ^^ {p => Get(p)}
   
   private def path = str ^^ {s => Path.parse(s, base)}
   private def mpath = str ^^ {s => Path.parseM(s, base)}
   private def file = str ^^ {s => home.resolve(s)}
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

/** Objects of type Action represent commands that can be executed by a Controller. */
sealed abstract class Action

/** switch on logging for a certain group */
case class LoggingOn(s : String) extends Action {override def toString = "log+ " + s}
/** switch off logging for a certain group */
case class LoggingOff(s : String) extends Action {override def toString = "log- " + s}
/** set the current base path */
case class SetBase(base : Path) extends Action {override def toString = "base " + base}
/** read a knowledge item */
case class ExecFile(file : java.io.File) extends Action {override def toString = "file " + file}
/** print all loaded knowledge items to STDOUT in text syntax */
case class Read(f : java.io.File) extends Action {override def toString = "read " + f}
/** add a catalog entry that makes the file system accessible via file: URIs */
case object Local extends Action {override def toString = "local"}
/** add catalog entries for a set of local copies, based on a file in Locutor registry syntax */
case class AddCatalog(file : java.io.File) extends Action {override def toString = "catalog " + file}
/** add a catalog entry for an MMT-aware database such as TNTBase, based on a configuration file */
case class AddTNTBase(file : java.io.File) extends Action {override def toString = "tntbase " + file}
/** registers a compiler
 * @param cls the name of a class implementing Compiler, e.g., "info.kwarc.mmt.api.lf.Twelf"
 * @param args a list of arguments that will be passed to the compiler's init method
 */
case class AddImporter(cls: String, args: List[String]) extends Action {override def toString = "importer " + cls + args.mkString(" ", " ", "")}
case class AddFoundation(cls: String, args: List[String]) extends Action {override def toString = "foundation " + cls + args.mkString(" ", " ", "")}
/** add catalog entries for a set of local copies, based on a file in Locutor registry syntax */
case class AddArchive(folder : java.io.File) extends Action {override def toString = "archive add " + folder}
/** builds a dimension in a previously opened archive */
case class ArchiveBuild(id: String, dim: String, in : List[String]) extends Action {override def toString = "archive " + id + " " + dim + in.mkString(" ","/","")}
/** builds a dimension in a previously opened archive */
case class ArchiveMar(id: String, file: java.io.File) extends Action {override def toString = "archive " + id + " mar " + file}
/** add MathWebSearch as a web service */
case class AddMWS(uri: URI) extends Action {override def toString = "mws " + uri}
/** load a file containing commands and execute them, fails on first error if any */
case object PrintAll extends Action
/** print all loaded knowledge items to STDOUT in XML syntax */
case object PrintAllXML extends Action
/** shut down server */
case object ServerOff extends Action {override def toString = "server off"}
/** start server */
case class ServerOn(port: Int) extends Action {override def toString = "server on " + port}
/** clear the state */
case object Clear extends Action {override def toString = "clear"}
/** exit */
case object Exit extends Action {override def toString = "exit"}
/** do nothing */
case object NoAction extends Action {override def toString = ""}

/** Objects of type GetAction represent commands that
 *  - retrieve knowledge items in abstract/internal syntax (see MakeAbstract)
 *  - post-process them to obtain concrete/external syntax (see MakeConrete)
 *  - then output the concrete form in various ways (see Output).
 */ 
case class GetAction(o: Output) extends Action {
   /** implement the Action using the provided Controller */
   def make(controller : Controller) = o.make(controller)
}

/** execute the Controller-specific default Action associated with a presentable element */
case class DefaultGet(pres : MakeConcrete) extends Action {
   override def toString = pres.toString
}

/** represent retrieval operations that return content elements
 *  These objects become Action commands by wrapping them in post-processing steps.
 *  see the children of MakePresentation
 */
abstract class MakeAbstract {
   def make(controller : Controller) : Content
}
/** retrieves a knowledge item */
case class Get(p : Path) extends MakeAbstract {
   def make(controller : Controller) : StructuralElement = controller.get(p)
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
	  	   case i : Instance => new TGroup(i.parent,p,Instance.elaborate(i,true)(controller.globalLookup,controller.report))
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
case class Present(c : MakeAbstract, nset : MPath) extends MakeConcrete {
   def make(controller : Controller, rb : RenderingHandler) {
      controller.presenter(c.make(controller), presentation.GlobalParams(rb, nset))
   }
   override def toString = c + " present " + nset
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
      println
   }
   override def toString = pres.toString
}
/** writes to a file */
case class ToFile(pres : MakeConcrete, file : java.io.File) extends Output {
   def make(controller : Controller) {
      val rb = new presentation.FileWriter(file)
      pres.make(controller, rb)
      rb.file.close
   }
   override def toString = pres + " write " + file
}
/** produces the result and throws it away
 *  call get to keep it in memory and retrieve it
 */
case class Respond(pres : MakeConcrete, param : String) extends Output {
   def get(controller : Controller) : scala.xml.Node = {
      val rb = new XMLBuilder
      pres.make(controller, rb)
      rb.get
   }
   def make (controller : Controller) {get(controller)}
   override def toString = pres.toString + " respond" + (if (param == "") "" else " " + param)
}
