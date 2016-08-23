package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import archives._
import backend._
import checking._
import documents._
import gui._
import libraries._
import moc._
import modules._
import notations._
import objects._
import ontology._
import parser._
import presentation._
import symbols._
import uom._
import utils._
import web._

/** An exception that is thrown when a needed knowledge item is not available
  *
  * A Controller catches it and retrieves the item dynamically.
  */
case class NotFound(path: Path, found: Option[Path] = None) extends java.lang.Throwable {
  override def toString = "NotFound(" + path + ")\n" + Stacktrace.asString(this)
}

/** minor variables kept by the controller, usually modifiable via actions */
class ControllerState {
  /** namespace prefixes that are in scope */
  var nsMap = NamespaceMap.empty
  /** base URL In the local system
   *  initially the current working directory
   */
  var home = File(System.getProperty("user.dir"))

  var actionDefinitions: List[Defined] = Nil
  var currentActionDefinition: Option[Defined] = None

  /** the configuration */
  val config = new MMTConfig
}

/** An interface to a controller containing read-only methods. */
abstract class ROController {
  val memory: ROMemory
  val localLookup: Lookup
  val globalLookup: Lookup

  def get(path: Path): StructuralElement

  def getDocument(path: DPath, msg: Path => String = p => "no document found at " + p): Document = get(path) match {
    case d: Document => d
    case _ => throw GetError(msg(path))
  }
}

/** A Controller is the central class maintaining all MMT knowledge items
  *
  * It stores all stateful entities and executes Action commands.
  */
class Controller extends ROController with ActionHandling with Logger {
  def this(r: Report) {
    this()
    report_ = r
  }

  // **************************** logging

  /** handles all output and log messages */
  private var report_ : Report = new Report
  val report = report_
  val logPrefix = "controller"

  // **************************** data state and components

  /** maintains all customizations for specific languages */
  val extman = new ExtensionManager(this)

  /** maintains all knowledge */
  val memory = new Memory(extman, report)
  val depstore = memory.ontology
  val library = memory.content

  /** convenience for getting the default text-based presenter (for error messages, logging, etc.) */
  def presenter: Presenter = extman.get(classOf[Presenter], "present-text-notations").get

  /** convenience for getting the default simplifier */
  def simplifier: Simplifier = extman.get(classOf[Simplifier]).head

  /** convenience for getting the default object parser */
  def objectParser: ObjectParser = extman.get(classOf[ObjectParser], "mmt").get

  /** runs build tasks */
  def buildManager = extman.get(classOf[BuildManager]).head

  /** converts between strict and pragmatic syntax using [[notations.NotationExtension]]s */
  def pragmatic = extman.get(classOf[Pragmatics]).head
  /** the http server */
  var server: Option[Server] = None
  /** the catalog maintaining all registered physical storage units */
  val backend = new Backend(extman, report)
  /** the query engine */
  val evaluator = new ontology.Evaluator(this)
  /** the window manager */
  lazy val winman = new WindowManager(this) // lazy so that GUI dependencies are optional
  /** moc.refiner - handling pragmatic changes in scope */
  val refiner = new moc.PragmaticRefiner(Set(moc.pragmaticRename, pragmaticAlphaRename))
  /** moc.propagator - handling change propagation */
  val propagator = new moc.OccursInImpactPropagator(memory)
  /** relational manager, handles extracting and parsing relational elements */
  val relman = new RelationalManager(this)

  /** communication with components
    *
    *  @return a notifier for all currently registered [[ChangeListener]]s
   */
  private[api] def notifyListeners = new Notify(extman.get(classOf[ChangeListener]), report)

  // **************************** control state and configuration

  /** all control state */
  protected val state = new ControllerState

  /** @return the current namespace map */
  def getNamespaceMap: NamespaceMap = state.nsMap

  /** @return the current base URI */
  def getBase: Path = state.nsMap.base

  /** @return the current home directory */
  def getHome: File = state.home
  /** sets the current home directory (relative to which path names in commands are executed) */
  def setHome(h: File) {
    state.home = h
  }

  /** @return the current configuration */
  def getConfig = state.config

  /** @return the value of an environment variable */
  def getEnvVar(name: String): Option[String] = {
    state.config.getEntry(classOf[EnvVarConf], name).map(_.value) orElse Option(System.getenv.get(name))
  }

  /** @return the current OAF root */
  def getOAF: Option[MathHub] = {
    val ocO = state.config.getEntries(classOf[OAFConf]).headOption
    ocO map {oc =>
      if (!oc.local.isDirectory)
        throw GeneralError(oc.local + " is not a directory")
      new MathHub(oc.remote.getOrElse(MathHub.defaultURL), oc.local, report)
    }
  }

  /** integrate a configuration into the current state */
  def loadConfig(conf: MMTConfig, loadEverything: Boolean) {
       state.config.add(conf)
       conf.getEntries(classOf[NamespaceConf]).foreach {case NamespaceConf(id,uri) =>
          state.nsMap = state.nsMap.add(id, uri)
       }
       conf.getEntries(classOf[MathPathConf]).foreach {c =>
         addArchive(c.local)
       }
       if (loadEverything) {
         loadAllArchives(conf)
         loadAllNeededTargets(conf)
       }
   }
  def loadConfigFile(f: File, loadEverything: Boolean) {
     val cfg = MMTConfig.parse(f)
     loadConfig(cfg, loadEverything)
  }

   private def loadAllArchives(conf: MMTConfig) {
       conf.getArchives foreach { arch =>
        addArchive(File(conf.getBase + arch.id))
      }
   }

   private def loadAllNeededTargets(conf: MMTConfig) {
      val archives = conf.getArchives
      val activeFormats = archives.flatMap(_.formats).distinct.map {id =>
        conf.getEntries(classOf[FormatConf]).find(_.id == id).getOrElse(throw ConfigurationError("Unknown format id: " + id))
      }
      val preloadedBuildTargets = extman.get(classOf[BuildTarget])
      val neededTargets = activeFormats.flatMap(f => f.importers ::: f.exporters).distinct.flatMap {key =>
        if (!preloadedBuildTargets.exists(_.key == key)) {
          conf.getEntry(classOf[ExtensionConf], key)
        } else None
      }
      neededTargets foreach {comp =>
        log("loading " + comp.cls)
        extman.addExtension(comp.cls, comp.args)
      }
   }

  // *************************** initialization and termination

  private def init {
    extman.addDefaultExtensions
    // load default configuration
    val mmtrc = MMTConfig.parse(MMTSystem.getResourceAsString("/mmtrc"), None)
    loadConfig(mmtrc, false)
  }

  init

  /** releases all resources that are not handled by the garbage collection */
  def cleanup {
    // notify all extensions
    extman.cleanup
    //close all open storages in backend
    backend.cleanup
    // close logging
    report.cleanup
    // stop server
    server foreach {
      _.stop
    }
    server = None
  }

  // **************************** reading source files

  /** parses a ParsingStream with an appropriate parser and optionally checks it
    *
    * @param ps the input
    * @param interpret if true, try to use an interpreter, not a parser
    * @param mayImport if true, use an importer as a fallback
    * @param errorCont continuation to be called on all encountered errors
    * @return the read Document
    */
  def read(ps: ParsingStream, interpret: Boolean, mayImport: Boolean = false)(implicit errorCont: ErrorHandler): Document = {
     val dpath = ps.parentInfo match {
        case IsRootDoc(dp) => dp
        case _ =>
           throw GeneralError("can only read root documents")
     }
     // (M) denotes whether the document is already in memory
     // If (M), we reuse the old structure and merge updates into it
     // The merging happens in the 'add' method.
     val oldDocOpt = localLookup.getO(dpath) map {
        case d: Document => d
        case _ => throw AddError("a non-document with this URI already exists: " + dpath)
     }
     oldDocOpt.foreach {doc =>
        // (M): deactivate the old structure
        log("deactivating " + doc.path)
        deactivate(doc)
     }
     // find an appropriate interpreter
     log((if (interpret) "interpreting " else "parsing ") + ps.source + " with format " + ps.format +
      (if (mayImport) "; using importer if necessary" else ""))
     lazy val fromImporter = if (mayImport) {
        extman.get(classOf[Importer]).find {imp =>
            imp.inExts contains ps.format
        }.map {imp => imp.asInterpreter}
     } else None
     val interpreterOpt = if (interpret) {
        extman.get(classOf[Interpreter], ps.format) orElse
           fromImporter orElse
              extman.get(classOf[Parser], ps.format).map(_.asInterpreter)
     } else {
        val parserOpt = extman.get(classOf[Parser], ps.format) orElse {
           extman.get(classOf[TwoStepInterpreter], ps.format).map(_.parser)
        }
        parserOpt.map(_.asInterpreter) orElse fromImporter
     }
     val interpreter = interpreterOpt.getOrElse {
       throw GeneralError(s"no ${if (interpret) "interpreter" else "parser"} for format ${ps.format} found")
     }
     // interpret the document
     interpreter(ps)
     // (M): delete all now-inactive parts of the old structure
     // TODO for two-step interpreters, deleteInactive should be called after parsing (currently: after checking)
     oldDocOpt.foreach {doc => deleteInactive(doc)}
     // return the read document
     // (M) return the reused document
     oldDocOpt getOrElse {
        globalLookup.getAs(classOf[Document], dpath)
     }
  }

  // ******************************* lookup of MMT URIs

  /** a lookup that uses only the current memory data structures */
  val localLookup = new LookupWithNotFoundHandler(library) {
    protected def handler[A](code: => A): A = try {
      code
    } catch {
      case NotFound(p, _) =>
        throw GetError(p.toPath + " not known")
    }

    def getDeclarationsInScope(mod: Term) = library.getDeclarationsInScope(mod)
  }
  /** a lookup that loads missing modules dynamically */
  val globalLookup = new LookupWithNotFoundHandler(library) {
    protected def handler[A](code: => A): A = iterate {
      code
    }

    def getDeclarationsInScope(mod: Term) = iterate {
      library.getDeclarationsInScope(mod)
    }
  }

  /** convenience for global lookup */
  def get(path: Path): StructuralElement = globalLookup.get(path)

  /** like get */
  def getO(path: Path) = try {
    Some(get(path))
  } catch {
    case _: GetError => None
  }

  // ******************************* transparent loading during global lookup

  /** wrapping an expression in this method, evaluates the expression dynamically loading missing content
    *
    * dependency cycles are detected
    * be aware that the argument may be evaluated repeatedly
    *
    * @param a this is evaluated until evaluation does not throw NotFound
    * @return the evaluation
    */
  def iterate[A](a: => A): A = iterate(a, Nil)

  /** repeatedly tries to evaluate its argument while missing resources (NotFound(p)) are retrieved
    *
    * stops if cyclic retrieval of resources
    */
  private def iterate[A](a: => A, previous: List[NotFound]): A = {
    try {
      a
    }
    catch {
      case e: NotFound =>
        val p = e.path
        if (previous.exists(_.path == p)) {
          throw GetError("retrieval failed (due to non-existence or cyclic dependency) for " + p)
        } else {
          iterate({retrieve(e); a}, e :: previous)
        }
    }
  }

  /** loads a path via the backend and reports it */
  protected def retrieve(nf: NotFound) {
    log("asking backend for URI " + nf.path)
    logGroup {
      try {
        // loading objects into memory changes state, so make sure only one object is loaded at a time
        this.synchronized {
          backend.load(nf)(this)
        }
      } catch {
        case NotApplicable(msg) =>
          throw GetError("backend: " + msg)
      }
    }
    log("retrieved " + nf.path)
  }

  // ******************************* adding elements and in-memory change management

  /** adds a knowledge item */
  def add(nw: StructuralElement) {
    iterate {
          localLookup.getO(nw.path) match {
            case Some(old) if InactiveElement.is(old) =>
              /* optimization for change management
               * If an inactive element with the same path already exists, we try to reuse it.
               * That way, already-performed computations and checks do not have to be redone;
               *  this applies in particular to object-level parsing and checking.
               *  (Incidentally, Java pointers to the elements stay valid.)
               * Reuse is only possible if the elements are compatible;
               *  intuitively, that means they have to agree in all fields except possibly for components and children.
               * In that case, they new components replace the old ones (if different);
               *   and the new child declarations are recursively added or merged into the old ones later on.
               * Otherwise, the new element is added as usual.
               * When adding elements to Body, the order is irrelevant because the children are stored as a set;
               *  but when adding to a Document (including Body.asDocument), the order matters.
               *  Therefore, we call reorder after every add/update.
               */
              if (old.compatible(nw)) {
                log("reusing deactivated " + old.path)
                // update everything but components and children
                old.merge(nw)
                var hasChanged = false // will be true if a component changed
                var deletedKeys = old.getComponents.map(_.key).toSet // will contain the list of no-longer-present components
                // update/add components
                nw.getComponents.foreach {case DeclarationComponent(comp, cont) =>
                  deletedKeys -= comp
                  old.getComponent(comp).foreach {oldCont =>
                    val ch = oldCont.update(cont)
                    if (ch) {
                       log(comp.toString + " changed or added")
                       hasChanged = true
                    }
                  }
                }
                // delete components
                if (deletedKeys.nonEmpty)
                  hasChanged = true
                deletedKeys foreach {comp =>
                  old.getComponent(comp).foreach {
                    log(comp.toString + " deleted")
                    _.delete
                  }
                }
                // activate the old one
                InactiveElement.erase(old)
                // notify listeners if a component changed
                if (hasChanged)
                   notifyListeners.onUpdate(old, nw)
              } else {
                // delete the deactivated old one, and add the new one
                log("overwriting deactivated " + old.path)
                memory.content.update(nw)
                //if (old.getOrigin != DefaultAssignment) notifyListeners.onDelete(old) // an old hack that should not be necessary anymore
                notifyListeners.onUpdate(old, nw)
              }
              memory.content.reorder(nw.path)
            //case Some(_) => // in this case, we could already report an error; but the None case reports it anyway
            case _ =>
              // the normal case
              memory.content.add(nw)
              // load extension providing semantics for a Module
              nw match {
                case m: Module =>
                  if (!extman.get(classOf[Plugin]).exists(_.theory == m.path)) {
                    getConfig.getEntries(classOf[SemanticsConf]).find(_.theory == m.path).foreach {sc =>
                      log("loading semantic extension for " + m.path)
                      extman.addExtension(sc.cls, sc.args)
                    }
                  }
                case _ =>
              }
              notifyListeners.onAdd(nw)
          }
    }
  }

  /**
   * if set, the element is deactivated
   */
  private val InactiveElement = new BooleanClientProperty[StructuralElement](utils.mmt.baseURI / "clientProperties" / "controller" / "status")

  /** marks this and its descendants as inactive */
  private def deactivate(se: StructuralElement) {
     if (!se.isGenerated)
       // generated constants and refs to them (see (*)) should be updated/removed by change listeners
       InactiveElement.is(se)
     //log("deactivating " + se.path)
     se match {
        case b: modules.Body =>
           b.asDocument.getDeclarations foreach deactivate
        case r: NRef =>
           localLookup.getO(r.target) foreach {d =>
              if (d.isGenerated)
                 InactiveElement.erase(se) //(*)
              deactivate(d)
           }
        case _ =>
           se.getDeclarations foreach deactivate
     }
  }
  /** deletes all inactive descendants */
  private def deleteInactive(se: StructuralElement) {
     if (InactiveElement.is(se)) {
        log("deleting deactivated " + se.path)
        delete(se.path)
        notifyListeners.onDelete(se)
     } else {
        se match {
           case b: modules.Body =>
              deleteInactive(b.asDocument)
           case r: NRef =>
              localLookup.getO(r.target) foreach deleteInactive
           case _ =>
              se.getDeclarations foreach deleteInactive
        }
     }
  }

  // ******************************* deleting elements

  /** deletes a document or module from memory
    * no change management, deletions are non-recursive, listeners are notified
    */
  def delete(p: Path) {
    p match {
      case cp: CPath =>
         throw DeleteError("deletion of component paths not implemented")
      case p =>
        library.delete(p)
        localLookup.getO(p) foreach {se =>
          notifyListeners.onDelete(se)
        }
    }
  }

  /** clears the state */
  def clear {
    memory.clear
    notifyListeners.onClear
  }

  // ******************************* change management as used in the moc package

  def detectChanges(elems: List[ContentElement]): StrictDiff = {
    val changes = elems flatMap { elem =>
      try {
        val old = globalLookup.get(elem.path)
        moc.Differ.diff(old, elem).changes
      } catch {
        case e: Exception => elem match {
          case m: Module => List(AddModule(m))
          case d: Declaration => List(AddDeclaration(d))
          case _ => throw ImplementationError("Updating element not supported for " + elem.toString)
        }
      }
    }
    new StrictDiff(changes)
  }

  def detectRefinements(diff: StrictDiff): List[String] = {
    refiner.detectPossibleRefinements(diff).map(_._1.description).toList
  }

  def update(diff: StrictDiff, withChanges: List[String] = Nil): Set[CPath] = {
    val changes = diff.changes
    val pChanges = refiner(new StrictDiff(changes), Some(withChanges))
    val propDiff = pChanges ++ propagator(pChanges)
    moc.Patcher.patch(propDiff, this)
    propagator.boxedPaths
  }
}
