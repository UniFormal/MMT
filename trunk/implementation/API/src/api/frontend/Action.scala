package jomdoc.frontend
import jomdoc._
import jomdoc.presentation._
import jomdoc.documents._
import scala.util.parsing.combinator._

object Action extends RegexParsers {
   private var base : Path = null
   private def commented = (comment ^^ {c => NoAction}) | (action ~ opt(comment) ^^ {case a ~ _ => a}) | empty ^^ {_ => NoAction}
   private def empty = "\\s*"r
   private def comment = "//.*"r
   private def action = controller | shell | getaction
   private def controller = logon | logoff | local | catalog | tntbase | execfile
   private def shell = setbase | read | printall | printallxml | clear | exit
   private def logon = "log+" ~> str ^^ {s => LoggingOn(s)}
   private def logoff = "log-" ~> str ^^ {s => LoggingOff(s)}
   private def local = "local" ^^ {case _ => Local}
   private def catalog = "catalog" ~> file ^^ {f => AddCatalog(f)}
   private def tntbase = "tntbase" ~> file ^^ {f => AddTNTBase(f)}
   private def execfile = "file " ~> file ^^ {f => ExecFile(f)}
   private def setbase = "base" ~> path ^^ {p => SetBase(p)}
   private def read = "read" ~> file ^^ {f => Read(DPath(new utils.xml.URI(f.toURI)))}
   private def printall = "printAll" ^^ {case _ => PrintAll}
   private def printallxml = "printXML" ^^ {case _ => PrintAllXML}
   private def clear = "clear" ^^ {case _ => Clear}
   private def exit = "exit" ^^ {case _ => Exit}
   private def getaction = tofile | respond | print | defaultget
   private def tofile = presentation ~ ("write" ~> file) ^^ {case p ~ f => ToFile(p,f)}
   private def print = presentation <~ "print" ^^ {p => Print(p)}
   private def defaultget = presentation ^^ {p => DefaultGet(p)}
   private def respond = (presentation <~ "respond") ~ str ^^ {case p ~ s => Respond(p,s)}
   private def presentation = present | tonode | deps | tostring
   private def tonode = content <~ "xml" ^^ {c => ToNode(c)}
   private def present = content ~ ("present" ~> mpath) ^^ {case c ~ p => Present(c,p)}
   private def deps = locdeps | xmldeps
   private def locdeps = path <~ "deps locutor" ^^ {case p => Deps(p, false)}
   private def xmldeps = path <~ "deps xml" ^^ {case p => Deps(p, true)}
   private def tostring = content ^^ {c => ToString(c)}
   private def content = closure | component | get
   private def closure = path <~ "closure" ^^ {p => Closure(p)}
   private def component = (path <~ "component") ~ str ^^ {case p ~ s => Component(p, s)}
   private def get = path ^^ {p => Get(p)}
   
   private def path = str ^^ {s => Path.parse(s, base)}
   private def mpath = str ^^ {s => Path.parseM(s, base)}
   private def file = str ^^ {s => new java.io.File(s)}
   private def str = "\\S+"r        //regular expression for non-empty word without whitespace
   def parseAct(s:String, b : Path) : Action = {
      base = b
      val p = parseAll(commented,s)
      p match {
         case Success(tree, _) => tree
         case e: NoSuccess => throw ParseError(s + "\n  error: " + e.msg)          
      }
   }
}

abstract class Action

case class LoggingOn(s : String) extends Action {override def toString = "log+ " + s}
case class LoggingOff(s : String) extends Action {override def toString = "log- " + s}
case class SetBase(base : Path) extends Action {override def toString = "base " + base}
case class Read(p : Path) extends Action {override def toString = "read " + p}
case object Local extends Action {override def toString = "local"}
case class AddCatalog(file : java.io.File) extends Action {override def toString = "catalog " + file}
case class AddTNTBase(file : java.io.File) extends Action {override def toString = "tntbase " + file}
case class ExecFile(file : java.io.File) extends Action {override def toString = "file " + file}
case object PrintAll extends Action
case object PrintAllXML extends Action
case object Clear extends Action {override def toString = "clear"}
case object Exit extends Action {override def toString = "exit"}
case object NoAction extends Action {override def toString = ""}

case class DefaultGet(pres : MakePresentation) extends Action {
   override def toString = pres.toString
}
abstract class GetAction extends Action {
   def make(controller : Controller)
}
case class Print(pres : MakePresentation) extends GetAction {
   def make(controller : Controller) {
      pres.make(controller, ConsoleWriter)
      println
   }
   override def toString = pres.toString
}
case class ToFile(pres : MakePresentation, file : java.io.File) extends GetAction {
   def make(controller : Controller) {
      val rb = new presentation.FileWriter(file)
      pres.make(controller, rb)
      rb.file.close
   }
   override def toString = pres + " write " + file
}
case class Respond(pres : MakePresentation, param : String) extends GetAction {
   def get(controller : Controller) : scala.xml.Node = {
      val rb = new XMLBuilder
      pres.make(controller, rb)
      rb.get
   }
   def make (controller : Controller) {get(controller)}
   override def toString = pres.toString + " respond" + (if (param == "") "" else " " + param)
}

abstract class MakePresentation {
   def make(controller : Controller, rb : RenderingHandler)
}
case class ToNode(c : MakeContent) extends MakePresentation {
   def make(controller : Controller, rb : RenderingHandler) {rb(c.make(controller).toNode)}
   override def toString = c + " xml"
}
case class ToString(c : MakeContent) extends MakePresentation {
   def make(controller : Controller, rb : RenderingHandler) {rb(c.make(controller).toString)}
   override def toString = c.toString
}
case class Present(c : MakeContent, nset : MPath) extends MakePresentation {
   def make(controller : Controller, rb : RenderingHandler) {
      controller.presenter(c.make(controller), presentation.GlobalParams(rb, nset))
   }
   override def toString = c + " present " + nset
}
case class Deps(path : Path, xml : Boolean) extends MakePresentation {
   def make(controller : Controller, rb : RenderingHandler) {
     if (xml) rb(<mmtabox xmlns="http://omdoc.org/abox" built="10.04.2010"/>)
     (controller.depstore.getInds ++ controller.depstore.getDeps).foreach(
         (d : ontology.ABoxDecl) =>
            if (path <= d.path) {
               if (xml) rb(d.toNode) else rb(d.toLocutor + "\n")
            }
     )
   }
   override def toString = path.toString + " deps"
}

abstract class MakeContent {
   def make(controller : Controller) : Content
}
case class Closure(p : Path) extends MakeContent {
   def make(controller : Controller) : Document = p match {
      case doc ? name =>
         controller.get(doc ? name) // retrieve once to make sure it's in memory 
         val cl = controller.depstore.theoryClosure(doc ? name).toList
         val clp = cl.map(MRef(doc, _, true))
         new Document(doc, clp)
   }
   override def toString = p + " closure"
}
case class Component(p : Path, comp : String) extends MakeContent {
   def make(controller : Controller) : Content = {
      val o = controller.get(p)
      o.role.componentByName(comp) match {
         case Some(c) => o.components(c)
         case None => throw ParseError("component name " + comp + " illegal for element with role " + o.role)
      }
   }
   override def toString = p + " component " + comp
}
case class Get(p : Path) extends MakeContent {
   def make(controller : Controller) : StructuralElement = controller.get(p)
   override def toString = p.toString
}
