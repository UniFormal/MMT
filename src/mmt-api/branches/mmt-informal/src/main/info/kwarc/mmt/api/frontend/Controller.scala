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
   val presenter = new StructureAndObjectPresenter
   
   /** maintains all customizations for specific languages */
   val extman = new ExtensionManager(this)
   /** pragmatic MMT features besides Patterns */
   val pragmatic = new Pragmatics(this)
   /** the http server */
   var server : Option[Server] = None
   /** the MMT parser (XML syntax) */
   val xmlReader = new XMLReader(report)
   /** the MMT term parser */
   val termParser = new ObjectParser(this)
   /** the MMT parser (native MMT text syntax) */
   val textParser = new StructureAndObjectParser(this)
   /** a basic structure checker */
   val checker = new StructureAndObjectChecker(this)
   /** the MMT parser (Twelf text syntax) */
   val textReader = new TextReader(this, this.add)
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
      def get(path : Path) =  try {library.get(path)} catch {case NotFound(p) => throw GetError(p.toPath + " not known")}
      //def imports(from: Term, to: Term) = library.imports(from, to)
      def visible(to: Term) = library.visible(to)
      def getImplicit(from: Term, to: Term) = library.getImplicit(from,to)
      def preImage(p : GlobalName) = library.preImage(p)
      def getDeclarationsInScope(mod : Term) = library.getDeclarationsInScope(mod)
   }
   private val self = this
   /** a lookup that loads missing modules dynamically */
   val globalLookup = new Lookup(report) {
      def get(path : Path) = {
         val se = iterate {self.get(path)}
         se match {
            case ce: ContentElement => ce
            case _ => throw GetError(path + " exists but is not a content element")
         }
      }
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
              // read and add the content
              case (u,n) => xmlReader.readDocuments(DPath(u), n) {e => add(e)}
           }
         } catch {
            case b : BackendError =>
               throw GetError("backend: " + b.getMessage) 
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
         case NotFound(p : Path) =>
            if (previous.exists(_ == p))
               throw GetError("retrieval failed for " + p)
            else {
               retrieve(p)
               iterate(a, p :: previous)
            }
          
      }
   }
   
   def get(fpath : flexiformal.FragPath) : Content = {
     def getFrag(c : Content, indices : List[Int]) : Content = indices match {
       case Nil => c
       case hd :: tl => getFrag(c.children(hd), tl)
     }
     val s= get(fpath.path)
     getFrag(s, fpath.fragment.indices)
   }
   
   /** retrieves a knowledge item */
   def get(path : Path) : StructuralElement = {
      path match {
         case p : DPath => iterate (docstore.get(p))
         case p : MPath =>
             try {
               iterate{ 
                 library.get(p)
               }
               }
             catch {
                case _: GetError =>
                   try {iterate(notstore.get(p))}
                   catch {
                      // check if the MPath refers to a Realization given as a Scala class; if so, add it as a module
                      case e: GetError =>
                         log("trying to find realization for " + p + " on classpath")
                         Realization.fromScala(p) match {
                            case Some(r) => add(r); r
                            case None => throw e
                         }
                   }
             }
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
      iterate {e match {
         case nw : ContentElement =>
            localLookup.getO(e.path) match {
               case Some(old) if old.inactive =>
                  /* optimization for change management
                   * if e.path is already loaded but inactive, and the new e is compatible with it,
                   * we reactivate the existing declaration
                   * otherwise, we remove the deactivated declaration and proceed normally
                   */
                  //TODO we currently reactivate declarations even if they occur in different orders
                  //     if no reactivatable element exists, we append at the end
                  if (old.compatible(nw)) {
                     log("activating " + old.path)
                     // deactivate all children
                     old.getDeclarations.foreach {_.inactive = true}
                     // update metadata and components
                     old.metadata = nw.metadata
                     nw.getComponents.foreach {case (comp, cont) =>
                        old.getComponent(comp).foreach {_.update(cont)}
                     }
                     // activate the old one
                     old.inactive = false
                     extman.changeListeners foreach {l =>
                        l.onUpdate(nw)
                     }
                  } else {
                     // delete the deactivated old one, and add the new one
                     log("deleting deactivated " + old.path)
                     memory.content.update(nw)
                     extman.changeListeners foreach {l =>
                        l.onDelete(nw.path)
                        l.onAdd(nw)
                     }
                  }
               case _ =>
                  // the normal case
                  memory.content.add(nw)
                  extman.changeListeners foreach {l =>
                     l.onAdd(nw)
                  }
            }
         case p : PresentationElement => notstore.add(p)
         case d : NarrativeElement => docstore.add(d) 
      }}
   }

   /**
    * deletes a document or module from Memory
    * no change management except that the deletions of scoped declarations are recursive
    *   in particular, the deletion of a document also deletes its subdocuments and modules 
    */
   def delete(p: Path) {
      p match {
         case d: DPath =>
            docstore.delete(d)
         case m: MPath =>
            library.delete(m)
            notstore.delete(m)
         case s: GlobalName =>
            library.delete(s)
         case cp : CPath =>
            library.delete(cp)
      }
      extman.changeListeners foreach {l =>
         l.onDelete(p)
      }
      //depstore.deleteSubject(p)
   }

   /** clears the state */
   def clear {
      memory.clear
      extman.changeListeners foreach {l => l.onClear}
   }
   /** releases all resources that are not handled by the garbage collection */
   def cleanup {
      // notify all extensions
      extman.cleanup
      //close all open svn sessions from storages in backend
      backend.cleanup
      // flush logging buffers
      report.flush
      // stop server
      server foreach {_.stop}
   }

   /** deletes a document, deactivates and returns its modules */
   private def deactivateDocument(d: DPath): List[Module] = {
      docstore.delete(d).toList.flatMap {doc =>
         log("deactivating document " + d)
         logGroup {
            doc.getLocalItems flatMap {
               case r: DRef => deactivateDocument(r.target)
               case r: MRef => get(r.target) match {
                  case m: Module =>
                     log("deactivating " + m.path)
                     m.inactive = true
                     List(m)
                  case _ => Nil // impossible
               }
            }
         }
      }
   }
   /** deletes everything in ce that is marked for deletion (i.e., inactive) */
   private def deleteInactive(ce: ContentElement) {
      ce.foreachDeclaration {d => if (d.inactive) {
         log("deleting deactivated " + d.path)
         delete(d.path)
      }}
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
      val modules = deactivateDocument(dpath)
      log("reading " + dpath)
      val result = f.getExtension match {
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
            val (doc, errorList) = textReader.readDocument(source, dpath)(pu => termParser.apply(pu, throw _))
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
      log("deleting the remaining deactivated elements")
      logGroup {
         modules foreach {m => deleteInactive(m)}
      }
      result
   }
   
   /** like read(f: File) but taking a string in mmt format  */
   def read(s: String, dpath: DPath) : (Document,List[SourceError]) = {
      val modules = deactivateDocument(dpath)
      log("reading " + dpath)
      val r = Reader(s)
      val result = try {
         val (doc, state) = textParser(r, dpath)
         (doc, state.getErrors)
      } finally {
         r.close
      }
      log("deleting the remaining deactivated elements")
      logGroup {
         modules foreach {m => deleteInactive(m)}
      }
      result
   }
   /** MMT base URI */
   protected var base : Path = DPath(mmt.baseURI)
   def getBase = base
   /** base URL In the local system */
   protected var home = File(System.getProperty("user.dir"))
   /** @return the current home directory */
   def getHome = home
   /** @param h sets the current home directory relative to which path names in commands are executed
    *  
    *  initially the current directory
    */
   def setHome(h: File) {home = h}

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
        report.flush
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
	      case Local =>
	          val currentDir = (new java.io.File(".")).getCanonicalFile
	          val b = URI.fromJava(currentDir.toURI)
	          backend.addStore(LocalSystem(b)) 
         case AddArchive(f) =>
	         val archs = backend.openArchive(f)
	         archs foreach {a => extman.targets.foreach {t => t.register(a)}}
         case AddSVNArchive(url, rev) =>
           backend.openArchive(url, rev)
         case ArchiveBuild(id, key, mod, in, args) =>
            val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found"))
            key match {
               case "check" => arch.check(in, this)
               case "validate" => arch.validate(in, this)
               case "flat" => arch.produceFlat(in, this)
               case "enrich" =>
                 val me = new ModuleElaborator(this)
                 arch.produceEnriched(in,me, this)
               case "relational" =>
                  arch.readRelational(in, this, "rel")
                  arch.readRelational(in, this, "occ")
                  log("done reading relational index")
               /*
               case "notation" => 
                  arch.readNotation(in, this)
                  log("done reading notation index")
                */
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
            case None if Util.isTaken(port) => logError("port " + port + " is taken, server not started.")
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
            File.read(f).split("\\n").foreach(handleLine)
	         home = oldHome
	      case AddReportHandler(h) => report.addHandler(h)
	      case LoggingOn(g) => report.groups += g
	      case LoggingOff(g) => report.groups -= g
	      case NoAction => ()
	      case Read(f) => read(f)
	      case Graph(f) =>
	         val tg = new TheoryGraph(depstore)
	         val gv = new GraphExporter(tg.nodes.toIterable, Nil, tg)
            gv.exportDot(f)
	      case Check(p) =>
	         val errors = checker(p)
	         if (errors == Nil)
	            log("check succeeded")
	         else
	            log("check failed (see log for error messages)")
	      case Navigate(p) =>
	         extman.changeListeners foreach {l =>
	            l.onNavigate(p)
	         }
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
