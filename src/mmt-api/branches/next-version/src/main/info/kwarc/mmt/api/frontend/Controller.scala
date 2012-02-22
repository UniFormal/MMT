package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import backend._
import presentation._
import libraries._
import objects._
import documents._
import ontology._
import web._
import utils._
import utils.FileConversion._
import info.kwarc.mmt.uom._
import io.BufferedSource
import java.io.FileInputStream

/** An exception that is throw when a needed knowledge item is not available.
 * A Controller catches it and retrieves the item dynamically.  
 */
case class NotFound(path : Path) extends java.lang.Throwable

/** An interface to a controller containing read-only methods. */
abstract class ROController {
   val localLookup : Lookup
   val globalLookup : Lookup
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
   /** maintains all customizations for specific languages */
   val extman = new ExtensionManager(report)

   /** maintains all knowledge */
   val memory = new Memory(report)
   val depstore = memory.ontology
   val library = memory.content
   val notstore = memory.presentation
   val docstore = memory.narration

   /** the MMT rendering engine */
   val presenter = new presentation.Presenter(this, report)
   /** the MMT parser (XML syntax) */
   val xmlReader = new XMLReader(this, report)
   /** the MMT parser (text/Twelf syntax) */
   val textReader = new TextReader(this, report)
   /** the catalog maintaining all registered physical storage units */
   val backend = new Backend(xmlReader, extman, report)
   /** the query engine */
   val evaluator = new ontology.Evaluator(this)
   /** the universal machine, a computation engine */
   val uom = {val u = new UOMServer(report); u.init; u}
   /** the http server */
   var server : Option[Server] = None

   /** a lookup that uses only the current memory data structures */
   val localLookup = library
   /** a lookup that load missing modules dynamically */
   val globalLookup = new Lookup(report) {
      def get(path : Path) = iterate {library.get(path)}
      def imports(from: Term, to: Term) = library.imports(from, to)
      def importsTo(to: Term) = library.importsTo(to)
      def preImage(p : GlobalName) = library.preImage(p)
   }
   
   protected def retrieve(path : Path) {
      log("retrieving " + path)
      report.indent
      backend.get(path)
      report.unindent
      log("retrieved " + path)
   }
   /**
    * wrapping an expression in this method, evaluates the expression dynamically loading missing content
    * dependency cycles are detected
    * @param a this is evaluated until evaluation does not throw NotFound
    * @return the evaluation
    * be aware that the argument may be evaluated repeatedly
    */
   def iterate[A](a : => A) : A = iterate(a, Nil)
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
         case c : ContentElement => addContent(c)
         case p : PresentationElement => notstore.add(p)
         case d : NarrativeElement => docstore.add(d) 
      })
   }
  /**
  * Validates and, if successful, adds a ContentElement to the theory graph.
  * Note that the element already points to the intended parent element
  * so that no target path is needed as an argument.
  * @param e the element to be added
  */
  def addContent(e : ContentElement) {
    log("adding: " + e.toString)
    try {checker.check(e)(memory) match {
       case Fail(msg) => throw AddError(msg)
       case Success(deps) =>
          memory.content.add(e)
          deps.map(memory.ontology += _)
       case Reconstructed(rs, deps) =>
          rs.foreach(memory.content.add)
          deps.map(memory.ontology += _)
    }} catch {
       case e @ NotFound(_) => throw e
    }
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
      extman.cleanup
   }
   /** reads a file and returns the Path of the document found in it */
   def read(f: java.io.File, docBase : Option[DPath] = None) : DPath = {
      val N = utils.xml.readFile(f)
      val dpath = docBase.getOrElse(DPath(URI.fromJava(f.toURI)))
      xmlReader.readDocument(dpath, N)
   }
   /** reads a text/Twelf file and returns its Path */
   def readText(f: java.io.File) : DPath = {
      val dpath = DPath(URI.fromJava(f.toURI))
      val source = scala.io.Source.fromFile(f, "UTF-8")
      val (doc, errorList) = textReader.crawlDocument(source, dpath)
      if (!errorList.isEmpty)
        log(errorList.size + " errors in " + dpath.toString + ": " + errorList.mkString("\n  ", "\n  ", ""))
      source.close
      dpath
   }
   protected var base : Path = DPath(mmt.baseURI)
   def getBase = base
   protected var home = File(".")
   def getHome = home
   def setHome(h: File) {home = h}

   /** starts the HTTP server on the given port */
   def startServer(port : Int) {
    server match {
      case Some(serv) => log("server already started on port " + serv.port)
      case None if (Util.isTaken(port)) => log("port " + port + " is taken. Server not started.")
      case _ =>
        val serv = new Server(port, this)
        serv.start
        log("Server started at http://localhost:" + port)
        server = Some(serv)
    }
  }
  
   /** stops the HTTP server */
   def stopServer {
    server match {
      case Some(serv) => 
        serv.stop
        log("Server stopped")
        server = None
      case None => log("server is not running, so it can't be stopped")
    }
  }

   protected def handleExc[A](a: => A) {
       try {a}
       catch {
    	   case e : info.kwarc.mmt.api.Error => report(e)
         case e : java.io.FileNotFoundException => report("error", e.getMessage)
       }
   }
   def handleLine(l : String) {
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
	      case AddImporter(c, args) => extman.addImporter(c, args)
	      case AddFoundation(c, args) => extman.addFoundation(c, args)
	      case AddTNTBase(f) =>
	         backend.addStore(Storage.fromOMBaseCatalog(f) : _*)
	      case Local =>
	          val currentDir = (new java.io.File(".")).getCanonicalFile
	          val b = URI.fromJava(currentDir.toURI)
	          backend.addStore(LocalSystem(b)) 
         case AddArchive(f) =>
	         backend.openArchive(f)
         case ArchiveBuild(id, dim, in) =>
            val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found"))
            dim match {
               case "compile" => arch.compile(in)
               case "content" => arch.produceNarrCont(in)
               case "check" => arch.check(in, this)
               case "delete" => arch.deleteNarrCont(in)
               case "clean" => List("narration", "content", "relational", "notation") foreach {arch.clean(in, _)}
               case "flat" => arch.produceFlat(in, this)
               case "relational" =>
                  arch.readRelational(in, this)
                  log(" done reading relational index")
               case "notation" => 
                  arch.readNotation(in, this)
                  log("done reading notation index")
               case "mws" => arch.produceMWS(in, "content")
               case "mws-flat" => arch.produceMWS(in, "mws-flat")
               case "extract" => arch.extractScala(in, "source")
               case "integrate" => arch.integrateScala(in, "source")
            }
         case ArchiveMar(id, file) =>
            val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found")) 
            arch.toMar(file)
         case AddMWS(uri) => extman.setMWS(uri)
	      case SetBase(b) =>
	         base = b
	         report("response", "base: " + base)
	      case ServerOn(p) => startServer(p)
	      case ServerOff => stopServer
	      case Clear => clear
	      case ExecFile(f) =>
	         var line : String = null
	         val oldHome = home
	         home = f.getParentFile
            File.ReadLineWise(f)(handleLine)
	         home = oldHome
	      case LoggingOn(g) => report.groups += g
	      case LoggingOff(g) => report.groups -= g
	      case NoAction => ()
	      case Read(f) => read(f)
        case ReadText(f) => readText(f)
	      case DefaultGet(p) => handle(GetAction(Print(p)))
	      case a : GetAction => a.make(this)
	      case PrintAllXML => report("response", "\n" + library.toNode.toString)
	      case PrintAll => report("response", "\n" + library.toString)
	      case Exit =>
	         cleanup 
	         sys.exit
      })
   }
}
