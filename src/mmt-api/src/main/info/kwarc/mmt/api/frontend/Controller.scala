package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import archives._
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
  var home = File.currentDir

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

  def getDocument(path: DPath, msg: String = "no document found"): Document = Try(get(path)) match {
    case scala.util.Success(d: Document) => d
    case _ => throw GetError(path, msg)
  }
}

/** A Controller is the central class maintaining all MMT content and extensions.
  *
  * Every application (e.g., [[Shell]]) typically creates one controller.
  * The controller creates and owns one instance of many MMT clasess (see the documentation of the respective fields below).
  * 
  * It also maintains the [[MMTConfig]] and exectures [[Action]]s.
  */
class Controller(report_ : Report = new Report) extends ROController with ActionHandling with Logger {
  // note that fields must be declared here in dependency order; otherwise, some val's are used when they are still null (i.e., uninitialized)
  
  // **************************** logging

  /** handles all output and log messages */
  val report = report_
  
  val logPrefix = "controller"

  // **************************** data state and components

  /** maintains all extensions */
  val extman = new ExtensionManager(this)
  /** the interface to physical storgae */
  val backend = new Backend(extman, report)

  /** maintains all knowledge: structural elements, relational data, etc. */
  val memory = new Memory(extman, report)
  /** shortcut for the relational manager */
  val depstore = memory.ontology
  /** shortcut for the library */
  val library = memory.content

  /** convenience for getting the default text-based presenter (for error messages, logging, etc.) */
  def presenter: Presenter = extman.get(classOf[Presenter], "present-text-notations").getOrElse(extman.get(classOf[Presenter], "present-text-notations-flat").get)

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

  /** return the MMT version (from the jar) */
  def getVersion = MMTSystem.getResourceAsString("/versioning/system.txt")

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
        case e => throw AddError(e, "a non-document with this URI already exists")
     }
     oldDocOpt.foreach {doc =>
        // (M): deactivate the old structure
        log("deactivating " + doc.path)
        //deactivate(doc)
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
        importer.build(a, BuildChanged(), FilePath(p), Some(errorCont))
      case None =>
        throw GeneralError("not in a known archive: " + f)
    }
  }

  // ******************************* lookup of MMT URIs

  /** a lookup that uses only the current memory data structures */
  val localLookup = library.asLocalLookup

  /** a lookup that uses the previous in-memory version (ignoring the current one) */
  val previousLocalLookup = memory.previousContent.asLocalLookup

  /** a lookup that loads missing modules dynamically */
  val globalLookup = new LookupWithNotFoundHandler(library) {
    protected def handler[A](code: => A): A = iterate {
      code
    }

    // TODO naively wrapping this in 'iterate' may duplicate side effects
    def forDeclarationsInScope(mod: Term)(f: (MPath,Term,Declaration) => Unit) = iterate {
      library.forDeclarationsInScope(mod)(f)
    }
  }

  /** convenience for global lookup */
  def get(path: Path): StructuralElement = globalLookup.get(path)

  /**
    * Tries to retrieve an element.
    * Like [[get]] but returns None in case no element could be found for `path`.
    **/
  def getO(path: Path): Option[StructuralElement] = try {
    Some(get(path))
  } catch {
    case _: GetError => None
    case _: BackendError => None
  }

  /**
    * @see [[getAsO]]
    */
  def getAs[E <: StructuralElement](cls : Class[E], path: Path): E = getO(path) match {
    case Some(e : E@unchecked) if cls.isInstance(e) => e
    case Some(r) => throw GetError(path, "element exists but is not a " + cls + " - it is a " + r.getClass)
    case None => throw GetError(path, "element does not exist")
  }

  /**
    * Tries to retrieve an element of a specific class.
    *
    * @return Some element in case an element is found for `path` (and that is an instance of `E`), or None if
    *         no element could be found for `path`.
    * @throws GetError in case the element exists, but is not an instance of `E`.
    * @see [[getAs]]
    */
  def getAsO[E <: StructuralElement](cls : Class[E], path: Path): Option[E] = getO(path).map {
    case e : E@unchecked if cls.isInstance(e) => e
    case r => throw GetError(path, "element exists but is not a " + cls + " - it is a " + r.getClass)
  }

  def getConstant(path: GlobalName): Constant = getAs(classOf[Constant], path)
  def getTheory(path: MPath): Theory = getAs(classOf[Theory], path)
  def getModule(path: MPath): Module = getAs(classOf[Module], path)


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
          case sm: AbstractTheory => getContextWithInner(sm)
          case sm: Module => getContextWithInner(sm)
          case _ => throw InvalidElement(m, "super module of module must be module")
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
    case m: Module => m.getInnerContext
    case s: Structure => Context.empty
    case dd: DerivedDeclaration =>
      val sfOpt = extman.get(classOf[StructuralFeature], dd.feature)
      sfOpt match {
        case Some(sf) => sf.getInnerContext(dd)
        case None => Context.empty
      }
  }
  
  // ******************************* semantics objects and literals

  /** the semantic type of all semantic objects
   *  This cannot be a stand-alone rule because it needs access to the backend to load semantic objects via Java reflection 
   */
  private object SemanticObjectType extends uom.Atomic[SemanticObject] {
     def asString = "semantic-object"
     val cls = classOf[SemanticObject]
     override def atomicToString(so: SemanticObject) = so.mpath.toPath
     def fromString(s: String) = {
       val mp = Path.parseM(s, NamespaceMap.empty)
       backend.loadObjectO(mp) match {
         case Some(so: SemanticObject) => so
         case Some(_) => throw ParseError("object exists but is not a rule")
         case None => throw ParseError("object does not exist")
       }
     }
     /** scala"QUALIFIED-CLASS-NAME" */
     override def lex = quotedLiteral("scala")
  }
  
  /** a built-in rule for using semantic objects as literals
   *  This cannot be a stand-alone rule because it needs access to the backend to load semantic objects via Java reflections. 
   */
  private val SemanticObjectRealizedType = new RepresentedRealizedType(OMS(utils.mmt.mmtcd ? "scala"), SemanticObjectType)

  /** convert an UnknownOMLIT to an OMLIT by choosing an applicable rule */
  def recognizeLiteral(rules: RuleSet, ul: UnknownOMLIT) = {
     val rts = Iterable(SemanticObjectRealizedType) ++ rules.get(classOf[uom.RealizedType])
     rts.find(_.synType == ul.synType).map {rule =>
       rule.parse(ul.valueString).from(ul)
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
          throw GetError(eprev.last.path, msg)
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
          throw GetError(nf.path, msg)
      }
    }
    log("retrieved " + nf.path)
  }

  // ******************************* adding elements and in-memory change management

  /**
    * Adds a knowledge item (a [[StructuralElement]]).
    *
    * If the element is a [[ContainerElement]] (incl. [[Structure]]s and [[PlainInclude]]s!),
    * you *must* to call [[endAdd()]] some time after calling this [[add()]] method.
    * Otherwise, you risk an inconsistent state of MMT.
    *
    * If the element is a [[Module]] (e.g. a [[Theory]] or [[View]]) whose name indicates
    * that it is nested within another module, then you *must* have called [[add()]]
    * *before* with a [[NestedModule]] declaration. Otherwise, an exception is thrown.
    *
    * @param at the position where it should be added (only inside modules, documents)
   */
  def add(nw: StructuralElement, at: AddPosition = AtEnd) {
    iterate {
          localLookup.getO(nw.path) match {
            case Some(old) if InactiveElement.is(old) =>
              /* this code was unintentionally never called because elements were never deactivated
               * this has now been fixed, but the library will never return deactivated objects so that the code still isn't called
               */
              /* optimization for change management
               * If an inactive element with the same path already exists, we try to reuse it.
               * That way, already-performed computations and checks do not have to be redone;
               *  this applies in particular to object-level parsing and checking.
               *  (Incidentally, Java pointers to the elements stay valid.)
               * Reuse is only possible if the elements are compatible;
               *  intuitively, that means they have to agree in all fields except possibly for components and children.
               * Additionally, we require that both elements are from the same source container -
               *  otherwise, modules of the same name in different files would be replaced by each other.
               * In that case, they new components replace the old ones (if different);
               *   and the new child declarations are recursively added or merged into the old ones later on.
               * Otherwise, the new element is added as usual.
               * When adding elements to Body, the order is irrelevant because the children are stored as a set;
               *  but when adding to a Document (including Body.asDocument), the order matters.
               *  Therefore, we call reorder after every add/update.
               */
              val compatible = old.compatible(nw)
              val replace = compatible && {
                val List(oldcont,nwcont) = List(old,nw).map{e => SourceRef.get(old).map(_.container)}
                oldcont == nwcont && oldcont.isDefined
              }
              if (replace) {
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
                memory.content.reactivate(old)
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
            case Some(old) if {
              val p = getO(old.parent)
              val ds = p.map(_.getDeclarations).getOrElse(Nil)
              p contains old
            } =>
              // This condition is necessary in case lookup succeeds for an element that is not physically present
              // This happens, e.g., during elaboration or in links where elements are generated dynamically by the library.
              // TODO there should be two kinds of lookup - physcial and logical
              memory.content.update(nw)
              notifyListeners.onUpdate(old, nw)
            case _ =>
              // the normal case
              memory.content.add(nw, at)
              // load extension providing semantics for a Module
              nw match {
                case t: Theory =>
                  TheoryExp.metas(OMMOD(t.path), all=true)(globalLookup) foreach {m =>
                    if (!extman.get(classOf[Plugin]).exists(_.theory == m)) {
                      getConfig.getEntries(classOf[SemanticsConf]).find(_.theory == m).foreach {sc =>
                        log("loading semantic extension for " + m)
                        extman.addExtension(sc.cls, sc.args)
                      }
                    }
                  }
                case _ =>
              }
              notifyListeners.onAdd(nw)
          }
    }
  }
  
  /** called after adding all elements in the body of a container element */
  def endAdd(c: ContainerElement[_]) {
    memory.content.endAdd(c)
  }
  
  /** marks this and its descendants as inactive */
  private def deactivate(se: StructuralElement) {
     if (!se.isGenerated) {
       // generated constants and refs to them (see (*)) should be updated/removed by change listeners
       memory.content.deactivate(se)
     }
     //log("deactivating " + se.path)
     se match {
        case b: modules.ModuleOrLink =>
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
           case b: modules.ModuleOrLink =>
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
      case _: CPath =>
         throw DeleteError(p, "deletion of component paths not implemented")
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
    backend.clear
    extman.clear
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

object Controller {
  /** convenience method for making a controller, e.g., for interactive shells or testing */ 
  def make(logging: Boolean, loadDefaultConfig: Boolean, loadArchives: List[String]) = {
    val c = new Controller
    if (logging) {
      c.handleLine("log console")
    }
    if (loadDefaultConfig) {
      val deploy = MMTSystem.runStyle match {
        case rs: MMTSystem.DeployRunStyle =>
          c.loadConfigFile(rs.deploy/"mmtrc", false)
        case _ =>
      }
    }
    val contentRoot = c.getMathHub.map(_.local).getOrElse(File(""))
    loadArchives.foreach {a =>
      c.handleLine(s"mathpath archive $contentRoot/$a")
    }
    c
  }
}