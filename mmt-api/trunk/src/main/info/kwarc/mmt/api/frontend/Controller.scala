package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils._

/** An exception that is throw when a needed knowledge item is not available.
 * A Controller catches it and retrieves the item dynamically.  
 */
case class NotFound(path : Path) extends java.lang.Throwable

/** An interface to a controller containing read-only methods. */
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
   def getDocument(path: DPath, msg : Path => String = p => "no document found at " + p) : Document = get(path) match {
      case d: Document => d
      case _ => throw GetError(msg(path))
   }
}

/** A Controller is the central class maintaining all MMT knowledge items.
  * It stores all stateful entities and executes Action commands.
  */  
class Controller(checker : Checker, report : Report) extends ROController {
   protected def log(s : => String) = report("controller", s)
   /** maintains all relational elements */
   val depstore = new RelStore(report)
   /** maintains all content elements */
   val library = new Library(checker, depstore, report)
   /** maintains all presentation elements */
   val notstore = new NotationStore(library, depstore, report)
   /** maintains all narrative elements */
   val docstore = new DocStore(depstore, report)
   /** the MMT rendering engine */
   val presenter = new presentation.Presenter(this, report)
   /** the MMT parser (XML syntax) */
   val reader = new Reader(this, report)
   /** the catalog maintaining all registered physical storage units */
   val backend = new Backend(reader, report)
   protected def retrieve(path : Path) {
      log("retrieving " + path)
      report.indent
      backend.get(path, false)
      report.unindent
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
   /** retrieves a knowledge item */
   def get(path : Path) : StructuralElement = {
      path match {
         case p : DPath => iterate (docstore.get(p))
         case p : MPath => iterate (try {library.get(p)} catch {case _ => notstore.get(p)})
         case p : GlobalName => iterate (library.get(p))
      }
   }
   /** selects a notation
    *  @param nset the style from which to select
    *  @param the notation key identifying the knowledge item to be presented
    */
   def get(nset : MPath, key : NotationKey) : Notation = {
      iterate (notstore.get(nset,key))
   }
   /** adds a knowledge item */
   def add(e : StructuralElement) {
      iterate (e match {
         case c : ContentElement => library.add(c)
         case p : PresentationElement => notstore.add(p)
         case d : NarrativeElement => docstore.add(d) 
      })
   }
/*   def delete(p: Path) = p match {
      case d: DPath =>
         docstore.delete(m)
      case m: MPath =>
         library.delete(m)
         notstore.delete(m)
      case s: GlobalName => throw DeleteError("deleting symbols not possible")
   }*/
   /** clears the state */
   def clear {
      docstore.clear
      library.clear
      notstore.clear
      depstore.clear
   }
   def read(f: java.io.File) : DPath = {
      val N = utils.xml.readFile(f)
      reader.readDocument(DPath(xml.URI(f.toURI)), N)
   }
   protected var base : Path = DPath(mmt.baseURI)
   def getBase = base
   protected def handleExc[A](a: => A) {
       try {a}
       catch {
    	   case e : info.kwarc.mmt.api.Error => report(e)
         case e : java.io.FileNotFoundException => report("error", e.getMessage)
       }
   }
   protected def handleLine(l : String) {
        val act = Action.parseAct(l, base)
        handle(act)
   }
   /** exectutes an Action */
   def handle(act : Action) : Unit = {
	  if (act != NoAction) report("user", act.toString)
	   (act match {
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
	      case Read(f) => read(f)
	      case DefaultGet(p) => handle(GetAction(Print(p)))
	      case a : GetAction => a.make(this)
	      case PrintAllXML => report("library", library.toNode.toString)
	      case PrintAll => report("library", library.toString)
	      case Exit => exit
      })
   }
}
