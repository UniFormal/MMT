package jomdoc.frontend
import jomdoc._
import jomdoc.backend._
import jomdoc.presentation._
import jomdoc.libraries._
import jomdoc.documents._
import jomdoc.ontology._
import jomdoc.utils._
//import scala.actors.{Actor,Channel}

case class NotFound(path : Path) extends java.lang.Throwable

abstract class ROController {
   val library : Lookup  
   def get(path : Path) : StructuralElement
   def get(nset : MPath, key : NotationKey) : Notation
   def getNotation(path : Path) : Notation =
      getNotation(path, "no element of required type found at " + path)
   def getNotation(path : Path, msg : String) : Notation = get(path) match {
      case e : Notation => e
      case _ => throw GetError(msg)
   }
}


class Controller(checker : Checker, report : Report) extends ROController {
   def log(s : => String) = report("controller", s)
   val depstore = new ABoxStore(report)
   val docstore = new DocStore(depstore, report)
   val library = new Library(checker, depstore, report)
   val notstore = new NotationStore(library, depstore, report)
   val presenter = new presentation.Presenter(this, report)
   val reader = new Reader(this, report)
   val backend = new Backend(reader, report)
   protected def retrieve(path : Path) {
      log("retrieving " + path)
      backend.get(path, false)
      log("retrieved " + path)
   }
   private def iterate[A](a : => A) : A = iterate(a, Nil)
   /** repeatedly tries to evaluate a while missing resources (NotFound(p)) are retrieved
    *  stops if cyclic retrieval of resources 
    */
   private def iterate[A](a : => A, previous : List[Path]) : A = {
      try {a}
      catch {
         case NotFound(p : Path) if ! previous.exists(_ == p) => retrieve(p); iterate(a, p :: previous)
         case NotFound(p : Path) => throw GetError("retrieval failed for " + p) 
         case e => throw e
      }
   }
   def add(e : StructuralElement) {
      iterate (e match {
         case c : ContentElement => library.add(c)
         case p : PresentationElement => notstore.add(p)
         case d : DocumentElement => docstore.add(d) 
      })
   }
   def clear {
      docstore.clear
      library.clear
      notstore.clear
      depstore.clear
   }
   def get(path : Path) : StructuralElement = {
      path match {
         case p : DPath => iterate (docstore.get(p))
         case p : MPath => iterate (try {library.get(p)} catch {case _ => notstore.get(p)})
         case p : SPath => iterate (library.get(p))
      }
   }
   def get(nset : MPath, key : NotationKey) : Notation = {
      iterate (notstore.get(nset,key))
   }
   protected var base : Path = DPath(mmt.baseURI)
   protected def handleExc[A](a: => A) {
       try {a}
       catch {
    	   case e : jomdoc.Error => report(e)
           case e : java.io.FileNotFoundException => report("error", e.getMessage)
       }
   }
   protected def handleLine(l : String) {
        val act = Action.parseAct(l, base)
        handle(act)
   }
   def handle(act : Action) {
	  if (act != NoAction) report("user", act.toString)
	  handleExc (act match {
	      case AddCatalog(f) =>
	         backend.addStore(Storage.fromLocutorRegistry(f) : _*)
	      case AddTNTBase(f) =>
	         backend.addStore(Storage.fromOMBaseCatalog(f) : _*)
	      case Local =>
	          val currentDir = (new java.io.File(".")).getCanonicalFile
	          val b = new xml.URI(currentDir.toURI)
	          backend.addStore(LocalSystem(b)) 
	      case SetBase(b) =>
	         base = b
	         report("controller", "base: " + base)
	      case Clear => clear
	      case ExecFile(f) =>
	         val file = new java.io.BufferedReader(new java.io.FileReader(f))
	         var line : String = null
	         while ({line = file.readLine(); line != null})
	            try {handleLine(line)}
	            catch {
	            	case e : Error => report(e); file.close; throw e
	            	case e => file.close; throw e
	            }
	         file.close
	      case LoggingOn(g) => report.groups += g
	      case LoggingOff(g) => report.groups -= g
	      case NoAction => ()
	      case Read(p) => backend.get(p, true)
	      case DefaultGet(p) => handle(Print(p))
	      case a : GetAction => a.make(this)
	      case PrintAllXML => report("library", library.toNode.toString)
	      case PrintAll => report("library", library.toString)
	      case Exit => exit
      })
   }
}
