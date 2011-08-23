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
class Controller(val checker : Checker, val report : Report) extends ROController {
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
      backend.get(path)
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
   /**
    * deletes a document or module
    * no change management except that the deletion of a document also deletes its subdocuments and modules 
    */
   def delete(p: Path) {p match {
      case d: DPath =>
         val orphaned = docstore.delete(d)
         orphaned foreach delete
      case m: MPath =>
         library.delete(m)
         notstore.delete(m)
      case s: GlobalName => throw DeleteError("deleting symbols not possible")
   }}
   /** clears the state */
   def clear {
      docstore.clear
      library.clear
      notstore.clear
      depstore.clear
   }
   /** releases all resources that are not handled by the garbage collection (currently: only the Twelf catalog) */
   def cleanup {
      backend.cleanup
   }
   /** reads a file and returns the Path of the document found in it */
   def read(f: java.io.File) : DPath = {
      val N = utils.xml.readFile(f)
      reader.readDocument(DPath(URI.fromJava(f.toURI)), N)
   }
   protected var base : Path = DPath(mmt.baseURI)
   def getBase = base
   protected var home = File(".")
   def getHome = home
   def setHome(h: File) {home = h}
   protected def handleExc[A](a: => A) {
       try {a}
       catch {
    	   case e : info.kwarc.mmt.api.Error => report(e)
         case e : java.io.FileNotFoundException => report("error", e.getMessage)
       }
   }
   protected def handleLine(l : String) {
        val act = try {
           Action.parseAct(l, base, home)
        } catch {
           case ParseError(msg) =>
              log(msg)
              return
        }
        handle(act)
   }
   /** executes an Action */
   def handle(act : Action) : Unit = {
	  if (act != NoAction) report("user", act.toString)
	   (act match {
	      case AddCatalog(f) =>
	         backend.addStore(Storage.fromLocutorRegistry(f) : _*)
	      case AddTwelf(f) =>
            val c = new lf.Twelf(File(f))
            backend.addCompiler(c)
	      case AddMizar =>
            //val c = new Mizar
            //backend.addCompiler(c)
	      case AddTNTBase(f) =>
	         backend.addStore(Storage.fromOMBaseCatalog(f) : _*)
	      case Local =>
	          val currentDir = (new java.io.File(".")).getCanonicalFile
	          val b = URI.fromJava(currentDir.toURI)
	          backend.addStore(LocalSystem(b)) 
         case AddArchive(f) => backend.openArchive(f)
         case ArchiveBuild(id, dim, in) =>
            val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found"))
            dim match {
               case "compile" => arch.compile(in)
               case "content" => arch.produceNarrCont(in)
               case "flat" => arch.produceFlat(in, this)
               case "relational" => arch.produceRelational(in, this)
               case "mws" => arch.produceMWS(in, "content")
               case "mws-flat" => arch.produceMWS(in, "flat")
            }
         case ArchiveMar(id, file) =>
            val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found")) 
            arch.toMar(file)
	      case SetBase(b) =>
	         base = b
	         report("response", "base: " + base)
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
	      case PrintAllXML => report("response", "\n" + library.toNode.toString)
	      case PrintAll => report("response", "\n" + library.toString)
	      case Exit =>
	         cleanup 
	         exit
      })
   }
}
