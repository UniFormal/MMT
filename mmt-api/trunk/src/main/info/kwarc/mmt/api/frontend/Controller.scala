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
  /** base URL In the local system */
  var home = File(System.getProperty("user.dir"))

  var actionDefinitions: List[Defined] = Nil
  var currentActionDefinition: Option[Defined] = None

  var environmentVariables = new scala.collection.mutable.ListMap[String, String]

  /** interface to a remote OAF */
  var oaf: Option[OAF] = None

  def getOAF: OAF = oaf.getOrElse {
    throw GeneralError("no oaf defined, use 'oaf root'")
  }

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
class Controller extends ROController with Logger {
  def this(r: Report) {
    this()
    report_ = r
  }

  /** runs build tasks */
  def buildManager = extman.get(classOf[BuildManager]).head

  /** handles all output and log messages */
  private var report_ : Report = new Report
  val report = report_
  /** maintains all knowledge */
  val memory = new Memory(report)
  val depstore = memory.ontology
  val library = memory.content
  val docstore = memory.narration

  /** maintains all customizations for specific languages */
  val extman = new ExtensionManager(this)

  /** convenience for getting the default text-based presenter (for error messages, logging, etc.) */
  def presenter: Presenter = extman.get(classOf[Presenter], "present-text-notations").get

  /** convenience for getting the default simplifier */
  def simplifier: Simplifier = extman.get(classOf[Simplifier]).head

  /** convenience for getting the default object parser */
  def objectParser: ObjectParser = extman.get(classOf[ObjectParser], "mmt").get

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
  /** the profile configuration */
  val config = new MMTConfig


  /** all other mutable fields */
  protected val state = new ControllerState

  /** @return the current namespace map */
  def getNamespaceMap: NamespaceMap = state.nsMap

  /** @return the current base URI */
  def getBase: Path = state.nsMap.base

  /** @return the current home directory */
  def getHome: File = state.home

  /** initially the current working directory
    *
    * @param h sets the current home directory relative to which path names in commands are executed
    */
  def setHome(h: File) {
    state.home = h
  }

  /** @return the value of an environment variable */
  def getEnvVar(name: String): Option[String] =
    state.environmentVariables.get(name) orElse Option(System.getenv.get(name))

  /** @return the current OAF root */
  def getOAF: Option[OAF] = state.oaf

  /** @return the current configuration */
  def getConfig = state.config

  private def init() {
    extman.addDefaultExtensions
  }

  init()

  /** @return a notifier for all currently registered [[ChangeListener]]s */
  private[api] def notifyListeners = new Notify(extman.get(classOf[ChangeListener]), report)

  //not sure if this really belong here, map from jobname to some state info
  val states = new collection.mutable.HashMap[String, ParserState]

  def detectChanges(elems: List[ContentElement]): StrictDiff = {
    val changes = elems flatMap { elem =>
      try {
        val old = globalLookup.get(elem.path)
        moc.Differ.diff(old, elem).changes
      } catch {
        case e: Throwable => elem match {
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

  val logPrefix = "controller"

  /** a lookup that uses only the current memory data structures */
  val localLookup = new Lookup {
    def get(path: Path) = try {
      library.get(path)
    } catch {
      case NotFound(p) => throw GetError(p.toPath + " not known")
    }

    //def imports(from: Term, to: Term) = library.imports(from, to)
    def visible(to: Term) = library.visible(to)

    def getImplicit(from: Term, to: Term) = library.getImplicit(from, to)

    def preImage(p: GlobalName) = library.preImage(p)

    def getDeclarationsInScope(mod: Term) = library.getDeclarationsInScope(mod)
  }
  private val self = this
  /** a lookup that loads missing modules dynamically */
  val globalLookup = new Lookup {
    def get(path: Path) = {
      val se = iterate {
        self.get(path)
      }
      se match {
        case ce: ContentElement => ce
        case _ => throw GetError(path + " exists but is not a content element")
      }
    }

    //def imports(from: Term, to: Term) = iterate {library.imports(from, to)}
    def visible(to: Term) = iterate {
      library.visible(to)
    }

    def getImplicit(from: Term, to: Term) = library.getImplicit(from, to)

    def preImage(p: GlobalName) = iterate {
      library.preImage(p)
    }

    def getDeclarationsInScope(mod: Term) = iterate {
      library.getDeclarationsInScope(mod)
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

  /** wrapping an expression in this method, evaluates the expression dynamically loading missing content
    *
    * dependency cycles are detected
    * be aware that the argument may be evaluated repeatedly
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

  /** retrieves a knowledge item */
  def get(path: Path): StructuralElement = {
    path match {
      case p: DPath => iterate(docstore.get(p))
      case p: MPath => iterate(library.get(p))
      case p: GlobalName => iterate(library.get(p))
      case _: CPath => throw ImplementationError("cannot retrieve component paths")
    }
  }

  /** adds a knowledge item */
  def add(e: StructuralElement) {
    iterate {
      e match {
        case nw: ContentElement =>
          localLookup.getO(e.path) match {
            //TODO localLookup yields a generated Constant when retrieving assignments in a view, which old.compatible(nw) false due to having different origin
            //probably introduced when changing the representation of paths in views
            //might also be because old is read from .omdoc due to dependency from checking earlier item and nw is read from .mmt when checking nw
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
                nw.getComponents.foreach { case (comp, cont) =>
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
        case d: NarrativeElement => docstore.add(d)
      }
    }
  }

  /** deletes a document or module from Memory
    *
    * no change management, deletions are non-recursive
    */
  def delete(p: Path) {
    val seOpt = localLookup.getO(p)
    p match {
      case d: DPath => docstore.delete(d)
      case _ =>
        library.delete(p)
        localLookup.getO(p) foreach { se =>
          notifyListeners.onDelete(se)
        }
    }
  }

  /** clears the state */
  def clear {
    memory.clear
    notifyListeners.onClear
  }

  /** releases all buffers */
  def cleanup {
    // notify all extensions
    extman.cleanup
    //close all open storages in backend
    backend.cleanup
    // close logging
    // report.cleanup
    // stop serversources that are not handled by the garbage collection
    server foreach {
      _.stop
    }
    server = None
  }

  /** deletes a document, deactivates and returns its modules */
  private def deactivateDocument(d: DPath): List[Module] = {
    docstore.delete(d).toList.flatMap { doc =>
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
    ce.foreachDeclaration { d => if (d.status == Inactive) {
      log("deleting deactivated " + d.path)
      delete(d.path)
    }
    }
  }

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

  /** builds a file in an archive choosing an appropriate importer */
  def build(f: File)(implicit errorCont: ErrorHandler) {
    backend.resolvePhysical(f) match {
      case Some((a, p)) =>
        val format = f.getExtension.getOrElse {
          throw GeneralError("no file extension")
        }
        val importer = extman.get(classOf[Importer]).find(_.inExts contains format).getOrElse {
          throw GeneralError("no importer found")
        }
        log("building " + f)
        importer.build(a, FilePath(p), Some(errorCont))
      case None =>
        throw GeneralError(f + " is not in a known archive")
    }
  }

  /** executes a string command */
  def handleLine(l: String, showLog: Boolean = true) {
    try {
      val act = Action.parseAct(l, getBase, getHome)
      handle(act, showLog)
    } catch {
      case e: Error =>
        log(e)
    }
    report.flush
  }

  private def buildFilesAction(keys: List[String], mod: BuildTargetModifier, args: List[String], files: List[File]) {
    report.addHandler(ConsoleHandler)
    val realFiles = if (files.isEmpty)
      List(File(System.getProperty("user.dir")))
    else {
      files.filter { f =>
        val ex = f.exists
        if (!ex)
          logError("file \"" + f + "\" does not exist")
        ex
      }
    }
    /* guess which files/folders the users wants to build
     *  @return archive root and relative path in it
     */
    def collectInputs(f: File): List[(File, FilePath)] = {
      backend.resolveAnyPhysical(f) match {
        case Some(ff) =>
          // f is a file in an archive
          List(ff)
        case None =>
          // not in archive, treat f as directory containing archives
          f.subdirs.flatMap(collectInputs)
      }
    }
    val inputs = realFiles flatMap collectInputs

    val buildTargets = keys map { key => extman.getOrAddExtension(classOf[BuildTarget], key, args) }

    inputs foreach { case (root, fp) =>
      handle(AddArchive(root), showLog = false) // make sure the archive is open
    val archive = backend.getArchive(root).get // non-empty by invariant of resolveAnyPhysical
      buildTargets foreach { bt =>
        report.groups += bt.key + "-result" // ensure logging
      val inPath = fp.segments match {
          case dim :: path =>
            bt match {
              case bt: TraversingBuildTarget if dim != bt.inDim.toString =>
                logError("wrong in-dimension \"" + dim + "\"")
              case _ =>
            }
            FilePath(path)
          case Nil => EmptyPath
        }
        bt(mod, archive, inPath)
      }
    }
  }

  private def archiveBuildAction(ids: List[String], key: String, mod: BuildTargetModifier, in: FilePath) {
    ids.foreach { id =>
      val arch = backend.getArchive(id) getOrElse (throw GetError("archive not found: " + id))
      key match {
        case "check" => arch.check(in, this)
        case "validate" => arch.validate(in, this)
        case "relational" =>
          arch.readRelational(in, this, "rel")
          arch.readRelational(in, this, "occ")
          log("done reading relational index")
        case "integrate" => arch.integrateScala(this, in)
        case "test" => // misuse of filepath parameter
          if (in.segments.length != 1)
            logError("exactly 1 parameter required, found " + in)
          else
            arch.loadJava(this, in.segments.head)
        case "close" =>
          val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found"))
          backend.closeArchive(id)
          notifyListeners.onArchiveClose(arch)
        case _ =>
          val bt = extman.getOrAddExtension(classOf[BuildTarget], key)
          bt(mod, arch, in)
      }
    }
  }

  private def execFileAction(f: File, nameOpt: Option[String]) {
    val folder = f.getParentFile
    // store old state, and initialize fresh state
    val oldHome = state.home
    val oldCAD = state.currentActionDefinition
    state.home = folder
    state.currentActionDefinition = None
    // excecute the file
    File.read(f).split("\\n").foreach(f => handleLine(f))
    if (state.currentActionDefinition.isDefined)
      throw ParseError("end of definition expected")
    // restore old state
    state.home = oldHome
    state.currentActionDefinition = oldCAD
    // run the actionDefinition, if given
    nameOpt foreach { name =>
      handle(Do(Some(folder), name))
    }
  }

  private def cloneRecursively(p: String) {
    val lcOpt = state.getOAF.clone(p)
    lcOpt foreach { lc =>
      val archs = backend.openArchive(lc)
      archs foreach { a =>
        val deps = stringToList(a.properties.getOrElse("dependencies", ""))
        deps foreach { d => cloneRecursively(URI(d).pathAsString) }
      }
    }
  }

  /** executes an Action */
  def handle(act: Action, showLog: Boolean = true) {
    state.currentActionDefinition match {
      case Some(Defined(file, name, acts)) if act != EndDefine =>
        state.currentActionDefinition = Some(Defined(file, name, acts ::: List(act)))
        if (showLog) report("user", "  " + name + ":  " + act.toString)
      case _ =>
        if (act != NoAction && showLog) report("user", act.toString)
        act match {
          case AddMathPathFS(uri, file) =>
            val lc = LocalCopy(uri.schemeNull, uri.authorityNull, uri.pathAsString, file)
            backend.addStore(lc)
          case AddMathPathJava(file) =>
            backend.openRealizationArchive(file)
          case Local =>
            val currentDir = new java.io.File(".").getCanonicalFile
            val b = URI.fromJava(currentDir.toURI)
            backend.addStore(LocalSystem(b))
          case AddArchive(f) =>
            val archs = backend.openArchive(f)
            archs.foreach { a =>
              a.properties.get("classpath").foreach { cp =>
                handle(AddMathPathJava(a.root / cp))
              }
              notifyListeners.onArchiveOpen(a)
            }
          case BuildFiles(keys, mod, args, files) =>
            buildFilesAction(keys, mod, args, files)
          case ArchiveBuild(ids, key, mod, in) =>
            archiveBuildAction(ids, key, mod, in)
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
            state.oaf = Some(new OAF(uriOpt.getOrElse(OAF.defaultURL), dir, report))
          case OAFInit(path) =>
            state.getOAF.init(path)
          case OAFClone(path) =>
            cloneRecursively(path)
          case OAFPull => state.getOAF.pull
          case OAFPush => state.getOAF.push
          case SetBase(b) =>
            state.nsMap = state.nsMap(b)
            report("response", "base: " + getBase)
          case SetEnvVar(n, v) =>
            state.environmentVariables(n) = v
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
          case Scala(fOpt) =>
            val interp = new MMTILoop(this)
            interp.run(fOpt)
          case MBT(file) =>
            new MMTScriptEngine(this).apply(file)
          case Clear => clear
          case ExecFile(f, nameOpt) => execFileAction(f, nameOpt)
          case Define(name) =>
            state.currentActionDefinition match {
              case None =>
                state.currentActionDefinition = Some(Defined(state.home, name, Nil))
              case Some(_) =>
                throw ParseError("end of definition expected")
            }
          case EndDefine =>
            state.currentActionDefinition match {
              case Some(a) =>
                state.actionDefinitions ::= a
                state.currentActionDefinition = None
              case None =>
                throw ParseError("no definition to end")
            }
          case Do(file, name) =>
            state.actionDefinitions.find { a => (file.isEmpty || a.file == file.get) && a.name == name } match {
              case Some(Defined(_, _, actions)) =>
                actions foreach (f => handle(f))
              case None =>
                logError("not defined")
            }
          case AddReportHandler(h) => report.addHandler(h)
          case LoggingOn(g) => report.groups += g
          case LoggingOff(g) => report.groups -= g
          case NoAction => ()
          case Read(f) =>
            val ps = backend.resolvePhysical(f) match {
              case Some((arch, p)) => ParsingStream.fromSourceFile(arch, FilePath(p))
              case None => ParsingStream.fromFile(f)
            }
            read(ps, interpret = false, mayImport = true)(new ErrorLogger(report))
          case Check(p, id) =>
            val checker = extman.get(classOf[Checker], id).getOrElse {
              throw GeneralError(s"no checker $id found")
            }
            checker(p)(new CheckingEnvironment(new ErrorLogger(report), RelationHandler.ignore))
          case Graph(f) =>
            val tg = new TheoryGraph(depstore)
            val gv = new GraphExporter(tg.nodes.toIterable, Nil, tg)
            gv.exportDot(f)
          case Navigate(p) =>
            notifyListeners.onNavigate(p)
          case a: GetAction => a.make(this)
          case PrintAllXML => report("response", "\n" + library.toNode.toString)
          case PrintAll => report("response", "\n" + library.toString)
          case Compare(p, r) => //TODO
          case WindowClose(w) => winman.deleteWindow(w)
          case WindowPosition(w, x, y) => winman.getWindow(w).setLocation(x, y)
          case BrowserAction(c) => c match {
            case "on" => winman.openBrowser
            case "off" => winman.closeBrowser
          }
          case Exit =>
            cleanup
            sys.exit()
        }
        if (act != NoAction && showLog) report("user", act.toString + " finished")
    }
  }
}
