package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import backend._
import presentation._
import libraries._
import objects._
import symbols._
import modules._
import documents._
import checking._
import parser._
import ontology._
import notations._
import web._
import utils._
import utils.FileConversion._
import uom._
import moc._
import gui._

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
   val docstore = memory.narration
   
   /** text-based presenter for error messages, logging, etc. */
   val presenter: Presenter = new MMTStructurePresenter(new NotationBasedPresenter {override def twoDimensional = false})
   /** the MMT parser (native MMT text syntax) */
   val textParser: Parser = new KeywordBasedParser(new NotationBasedParser)
   /** default structure checker and type reconstructor */
   val checker: Checker = new MMTStructureChecker(new RuleBasedChecker)
   /** elaborator and universal machine for simplification */
   val simplifier: Simplifier = new StepBasedElaborator(new UOM)

   /** the MMT parser (XML syntax) */
   val xmlReader = new XMLReader(report)
   val xmlStreamer = new XMLStreamer(this)
   /** Twelf-specific parser */
   val twelfParser = new TextReader(this, this.add)
   
   /** converts between strict and pragmatic syntax using [[NotationExtension]]s */
   val pragmatic = new Pragmatics(this)
   /** maintains all customizations for specific languages */
   val extman = new ExtensionManager(this)
   /** the http server */
   var server : Option[Server] = None
   /** the catalog maintaining all registered physical storage units */
   val backend = new Backend(extman, report)
   /** the query engine */
   val evaluator = new ontology.Evaluator(this)
   /** the window manager */
   val winman = new WindowManager(this)
   /** moc.refiner - handling pragmatic changes in scope */ 
   val refiner = new moc.PragmaticRefiner(Set(moc.pragmaticRename, pragmaticAlphaRename))
   /** moc.propagator - handling change propagation */
   val propagator = new moc.OccursInImpactPropagator(memory)
   
   private def init {
      extman.addDefaultExtensions
      List(presenter, textParser, checker, simplifier).foreach {_.init(this)}
   }
   init
   
   /** @return a notifier for all currently registered [[ChangeListener]]s */
   private[api] def notifyListeners = new Notify(extman.changeListeners, report)

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
   val localLookup = new Lookup {
      def get(path : Path) =  try {library.get(path)} catch {case NotFound(p) => throw GetError(p.toPath + " not known")}
      //def imports(from: Term, to: Term) = library.imports(from, to)
      def visible(to: Term) = library.visible(to)
      def getImplicit(from: Term, to: Term) = library.getImplicit(from,to)
      def preImage(p : GlobalName) = library.preImage(p)
      def getDeclarationsInScope(mod : Term) = library.getDeclarationsInScope(mod)
   }
   private val self = this
   /** a lookup that loads missing modules dynamically */
   val globalLookup = new Lookup {
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
         try {
            // loading objects into memory changes state, so make sure only one object is loaded at a time
            this.synchronized {
               backend.load(path)(this)
            }
         } catch {
            case NotApplicable(msg) =>
               throw GetError("backend: " + msg) 
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
   /** retrieves a knowledge item */
   def get(path : Path) : StructuralElement = {
      path match {
         case p : DPath => iterate (docstore.get(p))
         case p : MPath => iterate(library.get(p))
         case p : GlobalName => iterate (library.get(p))
         case _ : CPath => throw ImplementationError("cannot retrieve component paths")
      }
   }
   /** adds a knowledge item */
   def add(e : StructuralElement) {
      iterate {e match {
         case nw : ContentElement =>
            localLookup.getO(e.path) match {
               case Some(old) =>
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
                     old.getDeclarations.foreach {_.status = Inactive}
                     // update metadata and components
                     old.metadata = nw.metadata
                     nw.getComponents.foreach {case (comp, cont) =>
                        old.getComponent(comp).foreach {_.update(cont)}
                     }
                     // activate the old one
                     old.status = Active
                     notifyListeners.onUpdate(nw)
                  } else {
                     // delete the deactivated old one, and add the new one
                     log("deleting deactivated " + old.path)
                     memory.content.update(nw)
                     if (old.getOrigin != Some(DefaultAssignment)) // TODO hacky, but need to handle this case somehow
                         notifyListeners.onDelete(old)
                     notifyListeners.onAdd(nw)
                  }
               case _ => 
                  // the normal case
                  memory.content.add(nw)
                  notifyListeners.onAdd(nw)
            }
         case d : NarrativeElement => docstore.add(d) 
      }}
   }

   /**
    * deletes a document or module from Memory
    * no change management except that the deletions of scoped declarations are recursive
    *   in particular, the deletion of a document also deletes its subdocuments and modules 
    */
   def delete(p: Path) {
      val seOpt = localLookup.getO(p)
      p match {
         case d: DPath => docstore.delete(d)
         case m: MPath => library.delete(m)
         case s: GlobalName => library.delete(s)
         case cp : CPath => library.delete(cp)
      }
      seOpt foreach {se =>
         notifyListeners.onDelete(se)
      }
      //depstore.deleteSubject(p)
   }

   /** clears the state */
   def clear {
      memory.clear
      notifyListeners.onClear
   }
   /** releases all resources that are not handled by the garbage collection */
   def cleanup {
      // notify all extensions
      extman.cleanup
      //close all open svn sessions from storages in backend
      backend.cleanup
      // close logging buffers
      report.cleanup
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
               case r: MRef => localLookup.getO(r.target) match {
                  case Some(m: Module) =>
                     log("deactivating " + m.path)
                     m.status = Inactive
                     List(m)
                  case _ => Nil
               }
            }
         }
      }
   }
   /** deletes everything in ce that is marked for deletion (i.e., inactive) */
   private def deleteInactive(ce: ContentElement) {
      ce.foreachDeclaration {d => if (d.status == Inactive) {
         log("deleting deactivated " + d.path)
         delete(d.path)
      }}
   }
   /** convenience function for reading from a string  */
   def read(s: String, dpath: DPath, format: String)(implicit errorCont: ErrorHandler) : Document = {
      read(scala.io.Source.fromString(s), dpath, format)
   }
   /**
    * convenience function for reading from a file
    * file extension is used as format, file URI as default namespace
    * @param f the input file
    * @param docBase the base path to use for relative references
    * @return the read Document
    */
   def read(f: File, docBase : Option[DPath] = None)(implicit errorCont: ErrorHandler) : Document = {
      // use docBase, fall back to logical document id given by an archive, fall back to file:f
      val dpath = docBase orElse
                  backend.resolvePhysical(f).map {
                     case (arch, p) => DPath(arch.narrationBase / p)
                  } getOrElse
                  DPath(FileURI(f))
      val src = scala.io.Source.fromFile(f, "UTF-8")
      read(src, dpath, f.getExtension.getOrElse(""))
   }

   /**
    * reads a document
    * 
    * @param src the input source
    * @param srcURI the URI of the input source (needed for source references)
    * @param dpath the base path to use for relative references
    * @param format key to choose the reader: e.g., omdoc, elf, or mmt 
    * @param errorCont continuation to be called on all encountered errors
    * @return the read Document
    */
   def read(src: scala.io.Source, dpath: DPath, format: String)(implicit errorCont: ErrorHandler) : Document = {
      val modules = deactivateDocument(dpath)
      log("reading " + dpath + " with format " + format)
      val result = format match {
         case "omdoc" =>
            xmlStreamer.readDocument(dpath, src)(add)
         case "elf" =>
            val (doc, errorList) = twelfParser.readDocument(src, dpath)(pu => textParser(pu)(ErrorThrower))
            errorList.foreach {e => errorCont(e)}
            doc
         case "mmt" =>
            val srcURI = dpath.uri.setExtension(format)
            textParser.readString(srcURI, dpath, src.mkString)
         case f =>
            throw ParseError("unknown format: " + f)
      }
      src.close
      if (! modules.isEmpty) {
         log("deleting the remaining deactivated elements")
         logGroup {
            modules foreach {m => deleteInactive(m)}
         }
      }
      result
   }

   /** MMT base URI */
   protected var base : Path = DPath(mmt.baseURI)
   /** @return the current base URI */
   def getBase = base
   /** base URL In the local system */
   protected var home = File(System.getProperty("user.dir"))
   /** @return the current home directory */
   def getHome = home
   /**
    * @param h sets the current home directory relative to which path names in commands are executed
    * 
    * initially the current working directory
    */
   def setHome(h: File) {home = h}
   
   protected var actionDefinitions: List[Defined] = Nil
   protected var currentActionDefinition: Option[Defined] = None
   
   /** interface to a remote OAF */
   protected var oaf: Option[OAF] = None
   protected def getOAF = oaf.getOrElse {throw GeneralError("no oaf defined, use 'oaf root'")}
   
   /** executes a string command */
   def handleLine(l : String) {
        try {
           val act = Action.parseAct(l, base, home)
           handle(act)
        } catch {
           case e: Error =>
              log(e)
              return
        }
        report.flush
   }
   /** executes an Action */
   def handle(act : Action) : Unit = {
	  currentActionDefinition foreach {case Defined(file, name, acts) =>
	      if (act != EndDefine) {
	         currentActionDefinition = Some(Defined(file, name, acts ::: List(act)))
   	      report("user", "  " + name + ":  " + act.toString)
   	      return
	      }
	  }
     if (act != NoAction) report("user", act.toString)
	  act match {
	      case AddMathPathFS(uri,file) =>
	         val lc = LocalCopy(uri.schemeNull, uri.authorityNull, uri.pathAsString, file)
	         backend.addStore(lc)
         case AddMathPathJava(file) =>
            backend.openRealizationArchive(file)
	      case Local =>
	          val currentDir = (new java.io.File(".")).getCanonicalFile
	          val b = URI.fromJava(currentDir.toURI)
	          backend.addStore(LocalSystem(b)) 
         case AddArchive(f) =>
	         val archs = backend.openArchive(f)
	         archs.foreach {a =>
	            a.properties.get("classpath").foreach {cp =>
	               handle(AddMathPathJava(a.root/cp))
	            }
	            notifyListeners.onArchiveOpen(a)
	         }
         case ArchiveBuild(ids, key, mod, in, args) => ids.foreach {id =>
            val arch = backend.getArchive(id) getOrElse(throw GetError("archive not found: " + id))
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
               case "integrate"    => arch.integrateScala(this, in)
               case "test"         =>
                  if (in.length != 1)
                     logError("exactly 1 parameter required, found " + in.mkString(""))
                  else
                     arch.loadJava(this, in(0))
               case "close"        =>
                  val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found"))
                  backend.closeArchive(id)
                  notifyListeners.onArchiveClose(arch)
               case d =>
                  extman.getTarget(d) match {
                     case Some(buildTarget) =>
                        buildTarget(mod, arch, in, args)
                     case None =>
                        logError("unknown dimension " + d + ", ignored")
                  }
            }
         }
         case ArchiveMar(id, file) =>
            val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found")) 
            arch.toMar(file)
         case AddExtension(c, args) =>
            extman.addExtension(c, args)
         case AddMWS(uri) =>
            extman.mws = Some(new MathWebSearch(uri.toURL))
         case OAFRoot(dir, uriOpt) =>
            if (!dir.isDirectory)
               throw GeneralError(dir + " is not a directory")
            oaf = Some(new OAF(uriOpt.getOrElse(OAF.defaultURL), dir, report))
         case OAFClone(path) =>
            def cloneRecursively(p: String) {
               val lcOpt = getOAF.clone(p)
               lcOpt foreach {lc =>
                  val archs = backend.openArchive(lc)
                  archs foreach {a =>
                     val deps = MyList.fromString(a.properties.getOrElse("dependencies", ""))
                     deps foreach {d => cloneRecursively(URI(d).pathAsString)}
                  }
               }
            }
            cloneRecursively(path)
         case OAFPull => getOAF.pull            
         case OAFPush => getOAF.push
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
	      case ExecFile(f, nameOpt) =>
	         val folder = f.getParentFile
	         // store old state, and initialize fresh state
	         val oldHome = home
	         val oldCAD = currentActionDefinition
	         home = folder
	         currentActionDefinition = None
	         // excecute the file
            File.read(f).split("\\n").foreach(handleLine)
            if (currentActionDefinition.isDefined)
               throw ParseError("end of definition expected")
            // restore old state
	         home = oldHome
	         currentActionDefinition = oldCAD
	         // run the actionDefinition, if given
	         nameOpt foreach {name =>
	            handle(Do(Some(folder), name))
	         }
	      case Define(name) =>
	         currentActionDefinition match {
	            case None =>
	               currentActionDefinition = Some(Defined(home, name, Nil))
	            case Some(_) =>
	               throw ParseError("end of definition expected")
	         }
	      case EndDefine =>
	         currentActionDefinition match {
	            case Some(a) =>
	               actionDefinitions ::= a
	               currentActionDefinition = None
	            case None =>
	               throw ParseError("no definition to end")
	         }
	      case Do(file, name) =>
	         actionDefinitions.find {a => (file.isEmpty || a.file == file.get) && a.name == name} match {
	            case Some(Defined(_, _, actions)) =>
	               actions foreach handle
	            case None =>
	               logError("not defined")
	         }
	      case AddReportHandler(h) => report.addHandler(h)
	      case LoggingOn(g) => report.groups += g
	      case LoggingOff(g) => report.groups -= g
	      case NoAction => ()
	      case Read(f) =>
	         read(f)(new ErrorLogger(report))
	      case Graph(f) =>
	         val tg = new TheoryGraph(depstore)
	         val gv = new GraphExporter(tg.nodes.toIterable, Nil, tg)
            gv.exportDot(f)
	      case Check(p) =>
	         checker(p)(new ErrorLogger(report), RelationHandler.ignore)
	      case Navigate(p) =>
	         notifyListeners.onNavigate(p)
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
