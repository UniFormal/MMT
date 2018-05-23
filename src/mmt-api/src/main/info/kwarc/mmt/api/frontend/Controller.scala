package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import archives._
import info.kwarc.mmt.api.archives.lmh.MathHub._
import backend._
import checking._
import documents._
import gui._
import actions._
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

import scala.util.Try
import info.kwarc.mmt.api.archives.lmh.MathHub

/** An exception that is thrown when a needed knowledge item is not available
  *
  * A Controller catches it and retrieves the item dynamically.
  * 
  * @param path the URI of the not-found element
  * @param found a parent URI that has been loaded
  */
case class NotFound(path: Path, found: Option[Path] = None) extends java.lang.Throwable {
  override def toString = s"NotFound($path,$found)" // badly implemented in Throwable
  def toStringLong = toString + "\n" + Stacktrace.asString(this)
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

  def getDocument(path: DPath, msg: Path => String = p => "no document found at " + p): Document = Try(get(path)) match {
    case scala.util.Success(d: Document) => d
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
  /** applies only complification rules, used to unsimplify a fully-processed object, e.g., to undo definition expansion */
  def complifier(rules: RuleSet) = new TermTransformer("complify", this, rules, ttr => ttr.isInstanceOf[ComplificationRule])

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
  val evaluator = new ontology.QueryEvaluator(this)
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

  /** @return the current action being defined or None */
  def getCurrentActionDefinition = state.currentActionDefinition.map(_.name)
  /** @return the defined actions */
  def getActionDefinitions = state.actionDefinitions
  
  /** @return the value of an environment variable */
  def getEnvVar(name: String): Option[String] = {
    state.config.getEntry(classOf[EnvVarConf], name).map(_.value) orElse Option(System.getenv.get(name))
  }

  /** integrate a configuration into the current state */
  def loadConfig(conf: MMTConfig, loadEverything: Boolean) {
       state.config.add(conf)
       // add entries to the namespace
       conf.getEntries(classOf[NamespaceConf]).foreach {case NamespaceConf(id,uri) =>
          state.nsMap = state.nsMap.add(id, uri)
       }
       // add archives to the MathPath
       conf.getEntries(classOf[MathPathConf]).foreach {c =>
         addArchive(c.local)
       }
       // update the lmh cache
       conf.getEntries(classOf[LMHConf]).foreach { c =>
         lmh = Some(new MathHub(this, c.local, c.remote.getOrElse(MathHub.defaultURL), c.https))
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

  /** parses a ParsingStream with an appropriate parser or interpreter and optionally checks it
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
     val _ = interpreter(ps)
     // (M): delete all now-inactive parts of the old structure
     // TODO for two-step interpreters, deleteInactive should be called after parsing (currently: after checking)
     oldDocOpt.foreach {doc => deleteInactive(doc)}
     // return the read document
     // (M) return the reused document
     oldDocOpt getOrElse {
        globalLookup.getAs(classOf[Document], dpath)
     }
  }

  /** builds a file/folder in an archive using an appropriate importer */
  def build(f: File)(implicit errorCont: ErrorHandler) {
    backend.resolvePhysical(f) orElse backend.resolveAnyPhysicalAndLoad(f) match {
      case Some((a, p)) =>
        val format = f.getExtension.getOrElse {
          throw GeneralError("no file extension in " + f)
        }
        val importer = extman.get(classOf[Importer]).find(_.inExts contains format).getOrElse {
          throw GeneralError("no importer found for " + f)
        }
        log("building " + f)
        importer.build(a, Build.update, FilePath(p), Some(errorCont))
      case None =>
        throw GeneralError("not in a known archive: " + f)
    }
  }

  // ******************************* lookup of MMT URIs

  /** a lookup that uses only the current memory data structures */
  val localLookup = new LookupWithNotFoundHandler(library) with FailingNotFoundHandler {
    def forDeclarationsInScope(mod: Term)(f: (MPath,Term,Declaration) => Unit) = library.forDeclarationsInScope(mod)(f)
  }

  /** a lookup that uses the previous in-memory version (ignoring the current one) */
  val previousLocalLookup = new LookupWithNotFoundHandler(memory.previousContent) with FailingNotFoundHandler {
    def forDeclarationsInScope(mod: Term)(f: (MPath,Term,Declaration) => Unit) = memory.previousContent.forDeclarationsInScope(mod)(f)
  }

  /** a lookup that loads missing modules dynamically */
  val globalLookup = new LookupWithNotFoundHandler(library) {
    protected def handler[A](code: => A): A = iterate {
      code
    }

    def forDeclarationsInScope(mod: Term)(f: (MPath,Term,Declaration) => Unit) = iterate {
      library.forDeclarationsInScope(mod)(f)
    }
  }

  /** convenience for global lookup */
  def get(path: Path): StructuralElement = globalLookup.get(path)

  /** like get */
  def getO(path: Path) = try {
    Some(get(path))
  } catch {
    case _: GetError => None
    case _: BackendError => None
  }

  def getAs[E <: StructuralElement](cls : Class[E], path: Path): E = getO(path) match {
    case Some(e : E@unchecked) if cls.isInstance(e) => e
    case Some(r) => throw GetError("Element exists but is not a " + cls + ": " + path + " is " + r.getClass)
    case None => throw GetError("Element doesn't exist: " + path)
  }

  def getConstant(path : GlobalName) = getAs(classOf[Constant],path)
  def getTheory(path : MPath) = getAs(classOf[DeclaredTheory],path)


  // ******************* determine context of elements

  /** computes the context of an element, e.g., as needed for checking
   *  this methods allows processing individual elements without doing a top-down traversal to carry the context
   */
  def getContext(e: StructuralElement): Context = {
    lazy val parent = get(e.parent)
    e match {
      case d: NarrativeElement => d.parentOpt match {
        case None => d match {
          case d: Document => d.contentAncestor match {
            case None => Context.empty
            case Some(m) => getContextWithInner(m)
          }
          case _ => Context.empty
        }
        case Some(p) => getContext(parent)
      }
      case m: Module => m.superModule match {
        case None => Context.empty
        case Some(smP) => get(smP) match {
          case sm: DeclaredModule => getContextWithInner(sm)
          case sm: DefinedModule => getContext(m) // should never occur
        }
      }
      case d: Declaration =>
        parent match {
          case ce: ContainerElement[_] => getContextWithInner(ce)
          case nm: NestedModule => nm.module match {
            case ce: ContainerElement[_] => getContextWithInner(ce)
          }
        }
    }
  }

  /** auxiliary method of getContext, returns the context in which the body of ContainerElement is checked */
  private def getContextWithInner(e: ContainerElement[_]) = getContext(e) ++ getExtraInnerContext(e)

  /** additional context for checking the body of ContainerElement */
  def getExtraInnerContext(e: ContainerElement[_]) = e match {
    case d: Document => Context.empty
    case m: DeclaredModule => m.getInnerContext
    case s: DeclaredStructure => Context.empty
    case dd: DerivedDeclaration =>
      val sfOpt = extman.get(classOf[StructuralFeature], dd.feature)
      sfOpt match {
        case Some(sf) => sf.getInnerContext(dd)
        case None => Context.empty
      }
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
  def iterate[A](a: => A): A = iterate(a, Nil, true)

  /** repeatedly tries to evaluate its argument while missing resources (NotFound(p)) are retrieved
    *
    * @param a the expression to evaluate
    * @param previous all previous not-found failures during this recursion (needed for cycle detection)
    * @param org true if this is the original request, not a recursive call
    */
  private def iterate[A](a: => A, previous: List[NotFound], org: Boolean): A = {
    try {
      a
    } catch {
      case e: NotFound =>
        val p = e.path
        val eprev = e :: previous
        if (previous.exists(_.path == p)) {
          val msg = if (org)
            "retrieval finished, but element was not in memory afterwards (this usually means the backend loaded an element whose URI is not the one that was requested)"
          else
            "cyclic dependency while trying to retrieve"
          throw GetError("cannot retrieve " + eprev.last.path + ": " + msg)
        } else {
          iterate({retrieve(e)}, eprev, false)
          iterate(a, eprev, true)
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
          throw GetError(msg)
      }
    }
    log("retrieved " + nf.path)
  }

  // ******************************* adding elements and in-memory change management

  /** adds a knowledge item
   *  @param afterOpt the name of the declaration after which it should be added (only inside modules, documents)
   *  None adds at beginning, null (default) at end
   */
  def add(nw: StructuralElement, at: AddPosition = AtEnd) {
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
                notifyListeners.onUpdate(old, nw)
              }
              memory.content.reorder(nw.path)
            case Some(old) if getO(old.parent).map(_.getDeclarations).getOrElse(Nil) contains old =>
              // This condition is necessary in case lookup succeeds even though the element has not been added to the body yet.
              // This happens, e.g., during elaboration or in links where elements are generated dynamically by the library.
              memory.content.update(nw)
              notifyListeners.onUpdate(old, nw)
            case _ =>
              // the normal case
              memory.content.add(nw, at)
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
        val seOpt = localLookup.getO(p)
        seOpt foreach {se =>
          library.delete(p) // would throw NotFound if seOpt.isEmpty
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
