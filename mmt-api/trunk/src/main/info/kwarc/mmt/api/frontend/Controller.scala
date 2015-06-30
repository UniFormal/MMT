package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.gui._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.moc._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web._

/** An exception that is throw when a needed knowledge item is not available.
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

  def getOAF = oaf.getOrElse {
    throw GeneralError("no oaf defined, use 'oaf root'")
  }
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

  /* TODO these should be extensions; parser and checker already are */
  /** text-based presenter for error messages, logging, etc. */
  val presenter: Presenter = new MMTStructurePresenter(new NotationBasedPresenter {
    override def twoDimensional = false
  })
  /** elaborator and universal machine for simplification */
  val simplifier: Simplifier = new StepBasedElaborator(new UOM)

  /** convenience for getting the default object parser */
  def objectParser = extman.get(classOf[ObjectParser], "mmt").get

  /** converts between strict and pragmatic syntax using [[NotationExtension]]s */
  val pragmatic = new Pragmatics(this)
  /** maintains all customizations for specific languages */
  val extman = new ExtensionManager(this)
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

  /** all other mutable fields */
  protected val state = new ControllerState

  /** @return the current namespace map */
  def getNamespaceMap = state.nsMap

  /** @return the current base URI */
  def getBase = state.nsMap.base

  /** @return the current home directory */
  def getHome = state.home

  /** @return the value of an environment variable */
  def getEnvVar(name: String) = state.environmentVariables.get(name) orElse Option(System.getenv.get(name))

  /**
   * @param h sets the current home directory relative to which path names in commands are executed
   *
   *          initially the current working directory
   */
  def setHome(h: File) {
    state.home = h
  }

  private def init {
    extman.addDefaultExtensions
    List(presenter, simplifier).foreach {
      _.init(this)
    }
  }

  init

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

  /**
   * wrapping an expression in this method, evaluates the expression dynamically loading missing content
   * dependency cycles are detected
   * @param a this is evaluated until evaluation does not throw NotFound
   * @return the evaluation
   *         be aware that the argument may be evaluated repeatedly
   */
  def iterate[A](a: => A): A = iterate(a, Nil)

  /** repeatedly tries to evaluate a while missing resources (NotFound(p)) are retrieved
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
              notifyListeners.onAdd(nw)
          }
        case d: NarrativeElement => docstore.add(d)
      }
    }
  }

  /**
   * deletes a document or module from Memory
   * no change management except that the deletions of scoped declarations are recursive
   * in particular, the deletion of a document also deletes its subdocuments and modules
   */
  def delete(p: Path) {
    val seOpt = localLookup.getO(p)
    p match {
      case d: DPath => docstore.delete(d)
      case m: MPath => library.delete(m)
      case s: GlobalName => library.delete(s)
      case cp: CPath => library.delete(cp)
    }
    seOpt foreach { se =>
      notifyListeners.onDelete(se)
    }
    //depstore.deleteSubject(p)
  }

  /** clears the state */
  def clear {
    memory.clear
    notifyListeners.onClear
  }

  /** releases all buffers
      report.cleanup
      // stop serversources that are not handled by the garbage collection */
  def cleanup {
    // notify all extensions
    extman.cleanup
    //close all open storages in backend
    backend.cleanup
    // close logging
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

  /**
   * parses a ParsingStream with an appropriate parser and optionally checks it
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
        importer.build(a, p, Some(errorCont))
      case None =>
        throw GeneralError(f + " is not in a known archive")
    }
  }

  /** executes a string command */
  def handleLine(l: String) {
    try {
      val act = Action.parseAct(l, getBase, getHome)
      handle(act)
    } catch {
      case e: Error =>
        log(e)
        return
    }
    report.flush
  }

  /** executes an Action */
  def handle(act: Action): Unit = {
    state.currentActionDefinition foreach { case Defined(file, name, acts) =>
      if (act != EndDefine) {
        state.currentActionDefinition = Some(Defined(file, name, acts ::: List(act)))
        report("user", "  " + name + ":  " + act.toString)
        return
      }
    }
    if (act != NoAction) report("user", act.toString)
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
      case ArchiveBuild(ids, key, mod, in, args) => ids.foreach { id =>
        val arch = backend.getArchive(id) getOrElse (throw GetError("archive not found: " + id))
        key match {
          case "check" => arch.check(in, this)
          case "validate" => arch.validate(in, this)
          case "flat" => arch.produceFlat(in, this)
          case "enrich" =>
            val me = new ModuleElaborator(this)
            arch.produceEnriched(in, me, this)
          case "relational" =>
            arch.readRelational(in, this, "rel")
            arch.readRelational(in, this, "occ")
            log("done reading relational index")
          case "integrate" => arch.integrateScala(this, in)
          case "test" =>
            if (in.length != 1)
              logError("exactly 1 parameter required, found " + in.mkString(""))
            else
              arch.loadJava(this, in.head)
          case "close" =>
            val arch = backend.getArchive(id).getOrElse(throw GetError("archive not found"))
            backend.closeArchive(id)
            notifyListeners.onArchiveClose(arch)
          case d =>
            extman.get(classOf[BuildTarget], d) match {
              case Some(buildTarget) =>
                buildTarget(mod, arch, in)
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
        state.oaf = Some(new OAF(uriOpt.getOrElse(OAF.defaultURL), dir, report))
      case OAFInit(path) =>
        state.getOAF.init(path)
      case OAFClone(path) =>
        def cloneRecursively(p: String) {
          val lcOpt = state.getOAF.clone(p)
          lcOpt foreach { lc =>
            val archs = backend.openArchive(lc)
            archs foreach { a =>
              val deps = stringToList(a.properties.getOrElse("dependencies", ""))
              deps foreach { d => cloneRecursively(URI(d).pathAsString) }
            }
          }
        }
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
      case Clear => clear
      case ExecFile(f, nameOpt) =>
        val folder = f.getParentFile
        // store old state, and initialize fresh state
        val oldHome = state.home
        val oldCAD = state.currentActionDefinition
        state.home = folder
        state.currentActionDefinition = None
        // excecute the file
        File.read(f).split("\\n").foreach(handleLine)
        if (state.currentActionDefinition.isDefined)
          throw ParseError("end of definition expected")
        // restore old state
        state.home = oldHome
        state.currentActionDefinition = oldCAD
        // run the actionDefinition, if given
        nameOpt foreach { name =>
          handle(Do(Some(folder), name))
        }
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
            actions foreach handle
          case None =>
            logError("not defined")
        }
      case AddReportHandler(h) => report.addHandler(h)
      case LoggingOn(g) => report.groups += g
      case LoggingOff(g) => report.groups -= g
      case NoAction => ()
      case Read(f) =>
        val ps = backend.resolvePhysical(f) match {
          case Some((arch, p)) => ParsingStream.fromSourceFile(arch, p)
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
        sys.exit
    }
    if (act != NoAction) report("user", act.toString + " finished")
  }
}
