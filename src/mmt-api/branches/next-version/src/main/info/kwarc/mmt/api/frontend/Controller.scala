package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import backend._
import presentation._
import libraries._
import objects._
import documents._
import ontology._
import symbols.Constant
import web._
import utils._
import utils.FileConversion._
import uom._

import io.BufferedSource
import java.io.FileInputStream

/** An exception that is throw when a needed knowledge item is not available.
 * A Controller catches it and retrieves the item dynamically.  
 */
case class NotFound(path : Path) extends java.lang.Throwable

/** An interface to a controller containing read-only methods. */
abstract class ROController {
   val memory : ROMemory
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
class Controller extends ROController {
   def this(r: Report) {
      this()
      report_ = r
   }
   /** handles all output and log messages */
   private var report_ : Report = new Report 
   def report = report_
   /** maintains all knowledge */
   val memory = new Memory(report)
   val depstore = memory.ontology
   val library = memory.content
   val notstore = memory.presentation
   val docstore = memory.narration

   /** maintains all customizations for specific languages */
   val extman = new ExtensionManager(report)
   /** the http server */
   var server : Option[Server] = None
   /** the MMT parser (XML syntax) */
   val xmlReader = new XMLReader(this, report)
   /** the MMT parser (text/Twelf syntax) */
   val textReader = new TextReader(this, report)
   /** the catalog maintaining all registered physical storage units */
   val backend = new Backend(extman, report)
   /** the checker for the validation of ContentElement's and objects */
   val checker = new Checker(this)
   /** the MMT rendering engine */
   val presenter = new presentation.Presenter(this, report)
   /** the query engine */
   val evaluator = new ontology.Evaluator(this)
   /** the universal machine, a computation engine */
   val uom = new UOM(report)

   protected def log(s : => String) = report("controller", s)

   /** a lookup that uses only the current memory data structures */
   val localLookup = new Lookup(report) {
      def get(path : Path) =  try {library.get(path)} catch {case NotFound(p) => throw GetError(p.toPath + "not known")}
      def imports(from: Term, to: Term) = library.imports(from, to)
      def importsTo(to: Term) = library.importsTo(to)
      def getImplicit(from: Term, to: Term) = library.getImplicit(from,to)
      def preImage(p : GlobalName) = library.preImage(p)
      def getDeclarationsInScope(th : MPath) = library.getDeclarationsInScope(th)
   }
   /** a lookup that load missing modules dynamically */
   val globalLookup = new Lookup(report) {
      def get(path : Path) = iterate {library.get(path)}
      def imports(from: Term, to: Term) = iterate {library.imports(from, to)}
      def importsTo(to: Term) = iterate {library.importsTo(to)}
      def getImplicit(from: Term, to: Term) = library.getImplicit(from,to)
      def preImage(p : GlobalName) = iterate {library.preImage(p)}
      def getDeclarationsInScope(th : MPath) = iterate {library.getDeclarationsInScope(th)}
   }
   /** loads a path via the backend and reports it */
   protected def retrieve(path : Path) {
      log("retrieving " + path)
      report.indent
      // get, ...
      backend.get(path) {
        // read, ...
        case (u,n) => xmlReader.readDocuments(DPath(u), n) {
           // and add the content with URI path
           e => add(e)
        }
      }
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
         case _ : CPath => throw ImplementationError("cannot retrieve component paths")
      }
   }
   /** selects a notation
    *  @param nset the style from which to select
    *  @param key the notation key identifying the knowledge item to be presented
    */
   def get(nset : MPath, key : NotationKey) : Notation = {
      iterate (notstore.get(nset,key))
   }
   /** adds a knowledge item */
   def add(e : StructuralElement) {
      iterate (e match {
         case c : ContentElement => memory.content.add(c)
         case p : PresentationElement => notstore.add(p)
         case d : NarrativeElement => docstore.add(d) 
      })
   }

   /**
    * deletes a document or module
    * no change management except that the deletions of scoped declarations are recursive
    *   in particular, the deletion of a document also deletes its subdocuments and modules 
    */
   def delete(p: Path) {p match {
      case d: DPath =>
         val orphaned = docstore.delete(d)
         orphaned foreach delete
      case m: MPath =>
         library.delete(m)
         notstore.delete(m)
      case s: GlobalName => library.delete(s)
      case _ : CPath => throw ImplementationError("cannot delete component paths") //TODO delete component
   }}
   /** clears the state */
   def clear {
      memory.clear
   }
   /** releases all resources that are not handled by the garbage collection (currently: only the Twelf catalog) */
   def cleanup {
      extman.cleanup
      //closes all open svn sessions from storages in backend
      backend.cleanup
      if (server.isDefined) stopServer
   }
   /** reads a file containing a document and returns the Path of the document found in it */
   def read(f: java.io.File, docBase : Option[DPath] = None) : DPath = {
      val N = utils.xml.readFile(f)
      val dpath = docBase.getOrElse(DPath(URI.fromJava(f.toURI)))
      var p: DPath = null
      xmlReader.readDocument(dpath, N) {
         case d: Document =>
            add(d)
            p = d.path
         case e => add(e)
      }
      p
   }
   /** reads a text/Twelf file and returns its Path */
   def readText(f: java.io.File, docBase : Option[DPath] = None) : DPath = {
      val dpath = docBase getOrElse DPath(URI.fromJava(f.toURI))
      val source = scala.io.Source.fromFile(f, "UTF-8")
      val (doc, errorList) = textReader.readDocument(source, dpath)
      source.close
      if (!errorList.isEmpty)
        log(errorList.size + " errors in " + dpath.toString + ": " + errorList.mkString("\n  ", "\n  ", ""))
      dpath
   }
   /** MMT base URI */
   protected var base : Path = DPath(mmt.baseURI)
   def getBase = base
   /** base URL In the local system */
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

   def reportException[A](a: => A) {
       try {a}
       catch {
    	   case e : info.kwarc.mmt.api.Error => report(e)
         case e : java.lang.Throwable => report("error", e.getMessage)
       }
   }
   /** executes a string command */
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
	  act match {
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
         case AddSVNArchive(url, rev) =>
           backend.openArchive(url, rev)
          case ArchiveBuild(id, dim, in, params) =>
            val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found"))
            dim match {
               case "compile" => arch.produceCompiled(in)
               case "compile*" => arch.updateCompiled(in)
               case "content" => arch.produceNarrCont(in)
               case "content*" => arch.updateNarrCont(in)
               case "check" => arch.check(in, this)
               case "delete" => arch.deleteNarrCont(in)
               case "clean" => List("narration", "content", "relational", "notation") foreach {arch.clean(in, _)}
               case "flat" => arch.produceFlat(in, this)
               case "source" =>
                  arch.readSource(in, this)
                  log("done reading source")
               case "relational" =>
                  arch.readRelational(in, this)
                  log("done reading relational index")
               case "notation" => 
                  arch.readNotation(in, this)
                  log("done reading notation index")
               case "mws" => arch.produceMWS(in, "content")
               case "mws-flat" => arch.produceMWS(in, "mws-flat")
               case "extract" => arch.extractScala(in, "source")
               case "integrate" => arch.integrateScala(in, "source")
               case "present" => params.foreach(p => arch.producePres(Nil,p, this))
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
	      case AddReportHandler(h) => report.addHandler(h)
	      case LoggingOn(g) => report.groups += g
	      case LoggingOff(g) => report.groups -= g
	      case NoAction => ()
	      case Read(f) => read(f)
	      case ReadText(f) => readText(f)
	      case Check(p) =>
	         try {
	            checker.check(p)(_ => (), _ => ())
	            log("check succeeded")
	         } catch {
	          case e: Error =>
	            log("check failed\n" + e.getMessage)
	         }
	      case DefaultGet(p) => handle(GetAction(Print(p)))
	      case a : GetAction => a.make(this)
	      case PrintAllXML => report("response", "\n" + library.toNode.toString)
	      case PrintAll => report("response", "\n" + library.toString)
         case Compare(p,r) => //TODO
         case Exit =>
            //val tg = new TheoryGraph(depstore)
            //tg.export(utils.File("graph.gexf"))
	         cleanup 
	         sys.exit
     }
     if (act != NoAction) report("user", act.toString + " finished")
   }
}
