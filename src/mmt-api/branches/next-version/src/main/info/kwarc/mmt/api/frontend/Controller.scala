package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import backend._
import presentation._
import libraries._
import objects._
import symbols._
import modules._
import documents._
import ontology._
import pragmatics._
import symbols.Constant
import web._
import utils._
import utils.FileConversion._
import uom._
import moc._
import gui._

import io.BufferedSource
import java.io.FileInputStream

import org.tmatesoft.svn.core._
import io._
import auth._
import wc.SVNWCUtil

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
   val extman = new ExtensionManager(this)
   /** the http server */
   var server : Option[Server] = None
   /** the MMT parser (XML syntax) */
   val xmlReader = new XMLReader(this)
   /** the MMT term parser */
   val termParser = new parser.NotationParser(parser.LFGrammar.grammar, this)
   /** the MMT parser (text/Twelf syntax) */
   val textReader = new TextReader(this)
   /** the catalog maintaining all registered physical storage units */
   val backend = new Backend(extman, report)
   /** the checker for the validation of ContentElement's and objects */
   val checker = new Checker(this)
   /** pragmatic MMT features besides Patterns */
   val pragmatic = new Pragmatics(this)
   /** the MMT rendering engine */
   val presenter = new presentation.Presenter(this, report)
   /** the query engine */
   val evaluator = new ontology.Evaluator(this)
   /** the universal machine, a computation engine */
   val uom = new UOM(report)
   /** the window manager */
   val winman = new WindowManager(this)
   /** moc.refiner - handling pragmatic changes in scope */ 
   val refiner = new moc.PragmaticRefiner(Set(moc.pragmaticRename, pragmaticAlphaRename))
   /** moc.propagator - handling change propagation */
   val propagator = new moc.OccursInImpactPropagator(memory)
         
   
   def detectChanges(elems : List[ContentElement]) : StrictDiff = {
     val changes = elems flatMap {elem =>
       try {
         val old = globalLookup.get(elem.path)
         moc.Differ.diff(old, elem).changes
       } catch {
         case e => elem match {
           case m : Module => List(AddModule(m))
           case d : Declaration => List(AddDeclaration(d))
           case _ => throw ImplementationError("Updating element not supported for " + elem.toString)
         }   
       }
     }
     new StrictDiff(changes)
   }
   
   def detectRefinements(diff : StrictDiff) : List[String] = {
     refiner.detectPossibleRefinements(diff).map(_._1.description).toList
   }
   
   def update(diff : StrictDiff, withChanges : List[String] = Nil) : Set[CPath] = {     
     val changes = diff.changes     
     println(changes)
     val pChanges = refiner(new StrictDiff(changes), Some(withChanges))
     println("pchanges : " + pChanges.changes)
     val propDiff = pChanges ++ propagator(pChanges)
     println("pdiff : " + propDiff.changes)
     moc.Patcher.patch(propDiff, memory)
     propagator.boxedPaths
   }

   protected def log(s : => String) = report("controller", s)

   /** a lookup that uses only the current memory data structures */
   val localLookup = new Lookup(report) {
      def get(path : Path) =  try {library.get(path)} catch {case NotFound(p) => throw GetError(p.toPath + "not known")}
      //def imports(from: Term, to: Term) = library.imports(from, to)
      def visible(to: Term) = library.visible(to)
      def getImplicit(from: Term, to: Term) = library.getImplicit(from,to)
      def preImage(p : GlobalName) = library.preImage(p)
      def getDeclarationsInScope(mod : Term) = library.getDeclarationsInScope(mod)
   }
   /** a lookup that loads missing modules dynamically */
   val globalLookup = new Lookup(report) {
      def get(path : Path) = iterate {library.get(path)}
      //def imports(from: Term, to: Term) = iterate {library.imports(from, to)}
      def visible(to: Term) = iterate {library.visible(to)}
      def getImplicit(from: Term, to: Term) = library.getImplicit(from,to)
      def preImage(p : GlobalName) = iterate {library.preImage(p)}
      def getDeclarationsInScope(mod : Term) = iterate {library.getDeclarationsInScope(mod)}
      def getAllPaths() = library.getAllPaths //temporary TODO remove
   }
   /** loads a path via the backend and reports it */
   protected def retrieve(path : Path) {
      log("retrieving " + path)
      report.indent
      // get, ...
      backend.get(path) {
        // read, ...
        case (u,n) =>
          xmlReader.readDocuments(DPath(u), n) {
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
   def read(f: File, docBase : Option[DPath] = None) : DPath = {
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
   def readText(f: File, docBase : Option[DPath] = None) : DPath = {
      val dpath = docBase getOrElse DPath(URI.fromJava(f.toURI))
      val source = scala.io.Source.fromFile(f, "UTF-8")
      val (doc, errorList) = textReader.readDocument(source, dpath)(termParser.apply)
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
      case None => log("server is not running, so it cant be stopped")
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
	      case AddMathPathFS(uri,file) =>
	         val lc = LocalCopy(uri.schemeNull, uri.authorityNull, uri.pathAsString, file)
	         backend.addStore(lc)
	      case AddMathPathSVN(uri, rev, user, pass) =>
	         val repos = SVNRepositoryFactory.create(SVNURL.parseURIEncoded(uri.toString))
            user foreach {u =>
              val authManager = SVNWCUtil.createDefaultAuthenticationManager(u, pass.getOrElse(""))
              repos.setAuthenticationManager(authManager)
            }
            val s = SVNRepo(uri.schemeNull, uri.authorityNull, uri.pathAsString, repos, rev)
            backend.addStore(s)
	      case AddImporter(c, args) => extman.addImporter(c, args)
	      case AddPlugin(c) => extman.addPlugin(c, Nil)
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
               case "validate" => arch.validate(in, this)
               case "delete" => arch.deleteNarrCont(in)
               case "clean" => List("narration", "content", "relational", "notation") foreach {arch.clean(in, _)}
               case "flat" => arch.produceFlat(in, this)
               case "enrich" =>
                 val me = new ModuleElaborator(this)
                 arch.produceEnriched(in,me, this)
               case "source-terms" | "source-structure" => arch match {
                  case arch: archives.MMTArchive =>
                     arch.readSource(in, this, dim.endsWith("-terms"))
                     log("done reading source")
                  case _ => log("archive is not an MMT archive")
               }
               case "relational" =>
                  arch.readRelational(in, this, "rel")
                  arch.readRelational(in, this, "occ")
                  log("done reading relational index")
               case "notation" => 
                  arch.readNotation(in, this)
                  log("done reading notation index")
               case "mws" => arch.produceMWS(in, "content")
               case "mws-flat" => arch.produceMWS(in, "mws-flat")
               case "mws-enriched" => arch.produceMWS(in, "mws-enriched")
               case "extract" => arch.extractScala(in, "source")
               case "integrate" => arch.integrateScala(in, "source")
               case "present" => params.foreach(p => arch.producePres(Nil,p, this))
               case "close" => backend.closeArchive(id)
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
	      case Graph(f) =>
	         val tg = new TheoryGraph(depstore)
            tg.exportDot(f)
	      case Check(p) =>
	         try {
	            checker.check(p)( _ => (), _ => ())
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
         case WindowClose(w) => winman.deleteWindow(w)
         case WindowPosition(w,x,y) => winman.getWindow(w).move(x,y)
         case BrowserAction(c) => c match {
            case "on" => winman.openBrowser
            case "off" => winman.closeBrowser
         }
             
         case Exit =>
	         cleanup 
	         sys.exit
     }
     if (act != NoAction) report("user", act.toString + " finished")
   }
}
