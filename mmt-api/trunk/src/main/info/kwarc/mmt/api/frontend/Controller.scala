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
case class NotFound(path: Path) extends java.lang.Throwable

/** minor variables kept by the controller, usually modifiable via actions */
class ControllerState {
  /** MMT base URI */
  var nsMap = NamespaceMap.empty
  /** base URL In the local system
   *  initially the current working directory
   */
  var home = File(System.getProperty("user.dir"))

  var actionDefinitions: List[Defined] = Nil
  var currentActionDefinition: Option[Defined] = None

  var environmentVariables = new scala.collection.mutable.ListMap[String, String]

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

  /** maintains all knowledge */
  val memory = new Memory(report)
  val depstore = memory.ontology
  val library = memory.content

  /** maintains all customizations for specific languages */
  val extman = new ExtensionManager(this)

  /** convenience for getting the default text-based presenter (for error messages, logging, etc.) */
  def presenter: Presenter = extman.get(classOf[Presenter], "present-text-notations").get

  /** convenience for getting the default simplifier */
  def simplifier: Simplifier = extman.get(classOf[Simplifier]).head

  /** convenience for getting the default object parser */
  def objectParser: ObjectParser = extman.get(classOf[ObjectParser], "mmt").get

  /** runs build tasks */
  def buildManager = extman.get(classOf[BuildManager]).head

  /** converts between strict and pragmatic syntax using [[notations.NotationExtension]]s */
  val pragmatic = new Pragmatics(this)
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
    state.environmentVariables.get(name) orElse Option(System.getenv.get(name))
  }
  
  /** @return the current OAF root */
  def getOAF: Option[OAF] = {
    val ocO = state.config.getEntries(classOf[OAFConf]).headOption
    ocO map { oc =>
      if (oc.local.isDirectory)
        throw GeneralError(oc.local + " is not a directory")
      new OAF(oc.remote.getOrElse(OAF.defaultURL), oc.local, report)
    }
  }

  // *************************** initialization and termination 
  
  private def init {
    extman.addDefaultExtensions
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
    val modules = deactivateDocument(ps.dpath)
    log((if (interpret) "interpreting " else "parsing ") + ps.source + " with format " + ps.format +
      (if (mayImport) "; using importer if necessary" else ""))
    var interpreterOpt = if (interpret) {
      extman.get(classOf[Interpreter], ps.format) orElse
        extman.get(classOf[Parser], ps.format).map(p => new OneStepInterpreter(p))
    } else {
      val parserOpt = extman.get(classOf[Parser], ps.format) orElse {
        extman.get(classOf[TwoStepInterpreter], ps.format).map(_.parser)
      }
      parserOpt.map { p => new OneStepInterpreter(p) }
    }
    interpreterOpt = interpreterOpt orElse {
      if (mayImport) {
        extman.get(classOf[Importer]).find { imp =>
          imp.inExts contains ps.format
        }.map { imp => imp.asInterpreter }
      } else None
    }
    val interpreter = interpreterOpt.getOrElse {
      throw GeneralError(s"no ${if (interpret) "interpreter" else "parser"} for format ${ps.format} found")
    }
    val doc = interpreter(ps)
    if (modules.nonEmpty) {
      log("deleting the remaining deactivated elements")
      logGroup {
        modules foreach { m => deleteInactive(m) }
      }
    }
    doc
  }

  // ******************************* lookup of MMT URIs
  
  /** a lookup that uses only the current memory data structures */
  val localLookup = new LookupWithNotFoundHandler(library) {
    protected def handler[A](code: => A): A = try {
      code
    } catch {
      case NotFound(p) =>
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
  private def iterate[A](a: => A, previous: List[Path]): A = {
    try {
      a
    }
    catch {
      case NotFound(p: Path) =>
        if (previous.contains(p))
          throw GetError("retrieval failed for " + p)
        else {
          retrieve(p)
          iterate(a, p :: previous)
        }

    }
  }

  /** loads a path via the backend and reports it */
  protected def retrieve(path: Path) {
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

  // ******************************* adding elements and in-memory change management

  /** adds a knowledge item */
  def add(e: StructuralElement) {
    iterate {
      e match {
        case nw: ContentElement =>
          localLookup.getO(nw.path) match {
            //TODO localLookup yields a generated Constant when retrieving assignments in a view,
            // which old.compatible(nw) false due to having different origin
            //probably introduced when changing the representation of paths in views
            //might also be because old is read from .omdoc due to dependency from checking earlier item
            // and nw is read from .mmt when checking nw
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
                old.getDeclarations.foreach {
                  _.status = Inactive
                }
                // update metadata and components
                old.metadata = nw.metadata
                nw.getComponents.foreach { case DeclarationComponent(comp, cont) =>
                  old.getComponent(comp).foreach {
                    _.update(cont)
                  }
                }
                // activate the old one
                old.status = Active
                notifyListeners.onUpdate(nw)
              } else {
                // delete the deactivated old one, and add the new one
                log("deleting deactivated " + old.path)
                memory.content.update(nw)
                if (old.getOrigin != DefaultAssignment) // TODO hacky, but need to handle this case somehow
                  notifyListeners.onDelete(old)
                notifyListeners.onAdd(nw)
              }
            case _ =>
              // the normal case
              memory.content.add(nw)
              nw match {
                case m: Module =>
                  // load extension providing semantics for a theory
                  if (!extman.get(classOf[Plugin]).exists(_.theory == m.path)) {
                    getConfig.getEntries(classOf[SemanticsConf]).find(_.theory == m.path).foreach { sc =>
                      log("loading semantic extension for " + m.path)
                      extman.addExtension(sc.cls, sc.args)
                    }
                  }
                case _ =>
              }
              notifyListeners.onAdd(nw)
          }
        case d: NarrativeElement => library.add(d)
      }
    }
  }

  /** deletes a document, deactivates and returns its modules */
  private def deactivateDocument(d: DPath): List[Module] = {
     val doc = localLookup.getO(d)
     if (doc.isDefined)
        library.delete(d)
     doc.toList.flatMap {
       case doc: Document =>
         log("deactivating document " + d)
         logGroup {
           doc.getDeclarations flatMap {
             case inner: Document => deactivateDocument(inner.path)
             case r: DRef => deactivateDocument(r.target)
             case r: MRef => localLookup.getO(r.target) match {
               case Some(m: Module) =>
                 log("deactivating " + m.path)
                 m.status = Inactive
                 List(m)
               case _ => Nil
             }
             case ne => Nil
           }
         }
       case _ => Nil
    }
  }

  /** deletes everything in ce that is marked for deletion (i.e., inactive) */
  private def deleteInactive(ce: ContentElement) {
    ce.foreachDeclaration { d => if (d.status == Inactive) {
      log("deleting deactivated " + d.path)
      delete(d.path)
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
