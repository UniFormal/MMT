package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import backend._
import presentation._
import libraries._
import objects._
import symbols._
import modules._
import documents._
import parser._
import ontology._
import pragmatics._
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
class Controller extends ROController with Logger {
   def this(r: Report) {
      this()
      report_ = r
   }
   /** handles all output and log messages */
   private var report_ : Report = new Report 
   val report = report_
   /** maintains all knowledge */
   val memory = new Memory(report)
   val depstore = memory.ontology
   val library = memory.content
   val notstore = memory.presentation
   val docstore = memory.narration

   /** text-based presenter for error messages, logging, etc. */
   val presenter = new StructureAndObjectPresenter(this)
   
   /** maintains all customizations for specific languages */
   val extman = new ExtensionManager(this)
   /** pragmatic MMT features besides Patterns */
   val pragmatic = new Pragmatics(this)
   /** the http server */
   var server : Option[Server] = None
   /** the MMT parser (XML syntax) */
   val xmlReader = new XMLReader(this)
   /** the MMT term parser */
   val termParser = new ObjectParser(this)
   /** the MMT parser (native MMT text syntax) */
   val textParser = new StructureAndObjectParser(this)
   /** a basic structure checker */
   val checker = new StructureAndObjectChecker(this)
   /** the MMT parser (Twelf text syntax) */
   val textReader = new TextReader(this)
   /** the catalog maintaining all registered physical storage units */
   val backend = new Backend(extman, report)
   /** the query engine */
   val evaluator = new ontology.Evaluator(this)
   /** the universal machine, a computation engine */
   val uom = new UOM(this)
   /** the window manager */
   val winman = new WindowManager(this)
   /** moc.refiner - handling pragmatic changes in scope */ 
   val refiner = new moc.PragmaticRefiner(Set(moc.pragmaticRename, pragmaticAlphaRename))
   /** moc.propagator - handling change propagation */
   val propagator = new moc.OccursInImpactPropagator(memory)
   
   private def init {
      extman.addDefaultExtensions
   }
   init
   
   
   //not sure if this really belong here, map from jobname to some state info
   val states = new collection.mutable.HashMap[String, ParserState]
         
   
   def detectChanges(elems : List[ContentElement]) : StrictDiff = {
     val changes = elems flatMap {elem =>
       try {
         val old = globalLookup.get(elem.path)
         moc.Differ.diff(old, elem).changes
       } catch {
         case e : Throwable => elem match {
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
     val pChanges = refiner(new StrictDiff(changes), Some(withChanges))
     val propDiff = pChanges ++ propagator(pChanges)
     moc.Patcher.patch(propDiff, this)
     propagator.boxedPaths
   }

   val logPrefix = "controller"

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
   }
   /** loads a path via the backend and reports it */
   protected def retrieve(path : Path) {
      log("retrieving " + path)
      logGroup {
         // get, ...
         try {
           backend.get(path) {
              // read, ...
              case (u,n) =>
                 xmlReader.readDocuments(DPath(u), n) {
                 // and add the content with URI path
                 e => add(e)
              }
           }
         } catch {
            case BackendError(p) =>
               log("retrieval failed for " + p)
               throw GetError("retrieval failed for " + p) 
         }
      }
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
      }
   }
   /** retrieves a knowledge item */
   def get(path : Path) : StructuralElement = {
      path match {
         case p : DPath => iterate (docstore.get(p))
         case p : MPath => iterate (try {library.get(p)} catch {case _: java.lang.Exception => notstore.get(p)})
         case p : GlobalName => iterate (library.get(p))
         case _ : CPath => throw ImplementationError("cannot retrieve component paths")
      }
   }
   /** selects a notation
    *  @param nset the style from which to select
    *  @param key the notation key identifying the knowledge item to be presented
    */
   def get(nset : MPath, key : NotationKey) : StyleNotation = {
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
   /** releases all resources that are not handled by the garbage collection */
   def cleanup {
      extman.cleanup
      //closes all open svn sessions from storages in backend
      backend.cleanup
      server foreach {_.stop}
   }
   /** reads a file containing a document and returns the Path of the document found in it
    * the reader is chosen according to the file ending: omdoc, elf, or mmt
    * @param f the input file
    * @param docBase the base path to use for relative references
    * @return the read Document and a list of errors that were encountered
    */
   def read(f: File, docBase : Option[DPath] = None) : (Document,List[SourceError]) = {
      // use docBase, fall back to logical document id given by an archive, fall back to file:f
      val dpath = docBase orElse
                  backend.resolvePhysical(f).map {
                     case (arch, p) => DPath(arch.narrationBase / p)
                  } getOrElse
                  DPath(utils.FileURI(f))
      delete(dpath)
      f.getExtension match {
         case Some("omdoc") =>
            val N = utils.xml.readFile(f)
            var doc: Document = null
            xmlReader.readDocument(dpath, N) {
               case d: Document =>
                  add(d)
                  doc = d
               case e => add(e)
            }
            (doc, Nil)
         case Some("elf") =>
            val source = scala.io.Source.fromFile(f, "UTF-8")
            val (doc, errorList) = textReader.readDocument(source, dpath)(termParser.apply)
            source.close
            if (!errorList.isEmpty)
              log(errorList.size + " errors in " + dpath.toString + ": " + errorList.mkString("\n  ", "\n  ", ""))
            (doc, errorList.toList)
         case Some("mmt") =>
            val r = Reader(f)
            try {
               val (doc, state) = textParser(r, dpath)
               (doc, state.getErrors)
            } finally {
               r.close
            }
         case Some(e) => throw ParseError("unknown file extension: " + f)
         case None => throw ParseError("unknown document format: " + f)
      }
   }
   /** MMT base URI */
   protected var base : Path = DPath(mmt.baseURI)
   def getBase = base
   /** base URL In the local system */
   protected var home = File(System.getProperty("user.dir"))
   def getHome = home
   def setHome(h: File) {home = h}

   def reportException[A](a: => A) {
       try {a}
       catch {
    	   case e : info.kwarc.mmt.api.Error => report(e)
         case e : java.lang.Exception => report("error", e.getMessage)
       }
   }
   /** executes a string command */
   def handleLine(l : String) {
        val act = try {
           Action.parseAct(l, base, home)
        } catch {
           case ParseError(msg) =>
              logError(msg)
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
	      case AddExtension(c, args) => extman.addExtension(c, args)
	      case AddTNTBase(f) =>
	         backend.addStore(Storage.fromOMBaseCatalog(f) : _*)
	      case Local =>
	          val currentDir = (new java.io.File(".")).getCanonicalFile
	          val b = URI.fromJava(currentDir.toURI)
	          backend.addStore(LocalSystem(b)) 
         case AddArchive(f) =>
	         val arch = backend.openArchive(f)
	         extman.targets.foreach {t => t.register(arch)}
         case AddSVNArchive(url, rev) =>
           backend.openArchive(url, rev)
          case ArchiveBuild(id, key, mod, in, args) =>
            val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found"))
            key match {
               case "check" => arch.check(in, this)
               case "validate" => arch.validate(in, this)
               case "clean" => List("compiled", "narration", "content", "relational", "notation") foreach {arch.clean(in, _)}
               case "flat" => arch.produceFlat(in, this)
               case "enrich" =>
                 val me = new ModuleElaborator(this)
                 arch.produceEnriched(in,me, this)
               case "source-terms" | "source-structure" => arch match {
                  case arch: archives.MMTArchive =>
                     arch.readSource(in, this, key.endsWith("-terms"))
                     log("done reading source")
                  case _ => log("archive is not an MMT archive")
               }
               case "relational" =>
                  arch.readRelational(in, this, "rel")
                  arch.readRelational(in, this, "occ")
                  log("done reading relational index")
               /*
               case "notation" => 
                  arch.readNotation(in, this)
                  log("done reading notation index")
                */
               case "mws"          => arch.produceMWS(in, "content")
               case "mws-flat"     => arch.produceMWS(in, "mws-flat")
               case "mws-enriched" => arch.produceMWS(in, "mws-enriched")
               case "extract"      => arch.extractScala(this, in)
               case "integrate"    => arch.integrateScala(this, in)
               case "register"     =>
                  if (in.length != 1)
                     logError("exactly 1 parameter required for register command, found " + in.mkString(""))
                  else
                     arch.loadJava(this, in(0), true, false)
               case "test"         =>
                  if (in.length != 1)
                     logError("exactly 1 parameter required for test command, found " + in.mkString(""))
                  else
                     arch.loadJava(this, in(0), false, true)
               case "present"      => args.foreach(p => arch.producePres(Nil,p, this))
               case "close"        =>
                  val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found"))
                  extman.targets.foreach {t => t.register(arch)}
                  backend.closeArchive(id)
               case d =>
                  extman.getTarget(d) match {
                     case Some(buildTarget) =>
                        buildTarget(mod, arch, in, args)
                     case None =>
                        logError("unknown dimension " + d + ", ignored")
                  }
            }
         case ArchiveMar(id, file) =>
            val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found")) 
            arch.toMar(file)
         case AddMWS(uri) => extman.setMWS(uri)
	      case SetBase(b) =>
	         base = b
	         report("response", "base: " + base)
	      case ServerOn(port) => server match {
            case Some(serv) => logError("server already started on port " + serv.port)
            case None if (Util.isTaken(port)) => logError("port " + port + " is taken, server not started.")
            case _ =>
              val serv = new Server(port, this)
              serv.start
              log("Server started at http://localhost:" + port)
              server = Some(serv)
          }
	      case ServerOff => server match {
            case Some(serv) => 
              serv.stop
              log("Server stopped")
              server = None
            case None => log("server not running")
          }
	      case Scala =>
	         val interp = new MMTILoop(this)
            interp.run
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
	      case Graph(f) =>
	         val tg = new TheoryGraph(depstore)
            tg.exportDot(f)
	      case Check(p) =>
	         try {
	            checker(p)
	            log("check succeeded")
	         } catch {
	          case e: Invalid =>
	            log("check failed\n" + e.getMessage)
	         }
	      case DefaultGet(p) => handle(GetAction(Print(p)))
	      case a : GetAction => a.make(this)
	      case PrintAllXML => report("response", "\n" + library.toNode.toString)
	      case PrintAll => report("response", "\n" + library.toString)
         case Compare(p,r) => //TODO
         case WindowClose(w) => winman.deleteWindow(w)
         case WindowPosition(w,x,y) => winman.getWindow(w).setLocation(x,y)
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
