package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import actions._
import archives._
import backend._
import checking._
import opaque._
import parser._
import presentation._
import symbols._
import proving._
import uom._
import utils._
import web._

trait Extension extends Logger {
  /** the controller that this extension is added to; only valid after creation of the extension, i.e., will return null if used in a non-lazy val-field */
  protected var controller: Controller = null
  protected var report: Report = null

  lazy val defaultPrefix = {
    val s = getClass.toString
    val p = s.lastIndexOf(".")
    if (p == -1) s else s.substring(p + 1)
  }

  /** the prefix used to identify this extension for logging, by default the class name */
  def logPrefix: String = defaultPrefix

  /** a custom error class for this extension */
  case class LocalError(s: String) extends ExtensionError(logPrefix, s)
  /** convenience method for wrapping code in error handler that throws [[LocalError]] */
  protected def catchErrors(msg: String)(code: => Unit): Unit = {
     try {code}
     catch {case e: Error =>
       log(LocalError(msg).setCausedBy(e))
     }
  }
  /** like its partner but with return value */
  protected def catchErrors[A](msg: String, recoverWith: => A)(code: => A): A = {
     try {code}
     catch {case e: Error =>
       log(LocalError(msg).setCausedBy(e))
       recoverWith
     }
  }
  /** an [[ErrorHandler]] that wraps an error in a [[LocalError]] and throws it */
  protected def makeErrorThrower(msg: String) = new ErrorHandler {
    protected def addError(e: Error): Unit = {
      throw LocalError(msg).setCausedBy(e)
    }
  }


  /** MMT initialization (idempotent) */
  private[api] def init(controller: Controller): Unit = {
    this.controller = controller
    report = controller.report
  }

  /** any extension can initialize other extensions if those are not meant to be added to the ExtensionManager */
  protected def initOther(e: Extension): Unit = {
     e.init(controller)
  }

  protected def getFromFirstArgOrEnvvar(args: List[String], name: String, default: String = ""): String = {
    AnaArgs.splitOptions(args)._2 match {
      case Nil => controller.getEnvVar(name).getOrElse({
        log("environment variable " + name + " is not set" +
          (if (default.nonEmpty) " using \"" + default + "\"" else ""))
        default
      })
      case hd :: Nil =>
        log("using argument " + hd + " as value for " + name)
        hd
      case _ => throw LocalError("too many arguments: " + args.mkString(" ")
        + "; expected: 1")
    }
  }

  /** extension-specific initialization (override as needed, empty by default) */
  def start(args: List[String]): Unit = {}

  /** called when the controller is cleared; extensions must still be operational after processing this call */
  def clear: Unit = {}

  /** extension-specific cleanup (override as needed, empty by default)
    *
    * Extensions may create persistent data structures and threads,
    * but they must clean up after themselves in this method
    */
  def destroy: Unit = {}

  /** extensions that process tasks in separate threads should override this and wait until those threads are done */
  def waitUntilRemainingTasksFinished: Unit = {}

  /** convenience for calling waitUntilRemainingTasksFinished and then destroy */
  def destroyWhenRemainingTasksFinished: Unit = {
    waitUntilRemainingTasksFinished
    destroy
  }
}

/** extensions classes that can be tested for applicability based on a format string */
trait FormatBasedExtension extends Extension {
  /**
    * @param format the format/key/other identifier, for which an extension is needed
    * @return true if this extension is applicable
    */
  def isApplicable(format: String): Boolean
}

/**
  * Common super class of all extensions, whose functionality is systematically split between structure and object level
  *
  * Implementing classes should have a constructor that takes an Extension providing the object level functionality
  * and add the structure level functionality.
  */
trait LeveledExtension extends Extension {
  def objectLevel: Extension

  override def init(controller: Controller): Unit = {
    objectLevel.init(controller)
    super.init(controller)
  }

  override def destroy: Unit = {
    objectLevel.destroy
    super.destroy
  }
}

/** An ExtensionManager maintains all language-specific extensions of MMT.
  * This includes Compilers, QueryTransformers, and Foundations.
  * They are provided as plugins and registered via their qualified class name, which is instantiated by reflection.
  */
class ExtensionManager(controller: Controller) extends Logger {

  private[api] var extensions: List[Extension] = Nil
  private val knownExtensionTypes = List(
    classOf[Plugin], classOf[ParserExtension], classOf[QueryTransformer],
    classOf[ontology.QueryFunctionExtension],
    classOf[ChangeListener], classOf[ServerExtension],
    classOf[Parser], classOf[Checker], classOf[Prover], classOf[Interpreter], classOf[Simplifier], classOf[Presenter],
    classOf[BuildTarget],
    classOf[StructuralFeature], classOf[ModuleLevelFeature] 
  )


  /** all extensions of type E */ 
  def get[E <: Extension](cls: Class[E]): List[E] = extensions.collect {
    case e: E@unchecked if cls.isInstance(e) => e
  }

  /** some extension of type E */ 
  def get[E <: FormatBasedExtension](cls: Class[E], format: String): Option[E] = extensions.collectFirst {
    case e: E@unchecked if cls.isInstance(e) && e.isApplicable(format) => e
  }

  /** like get, but if necessary looks up the key in the current [[MMTConfig]]
    *
    * @param args additional arguments for the extension (appended to those in configuration); ignored if extension has already been created
    */
  def getOrAddExtension[E <: FormatBasedExtension](cls: Class[E], format: String, args: List[String] = Nil): Option[E] = {
    get(cls, format) orElse {
      val (className, extraArgs) = controller.getConfig.getEntry(classOf[ExtensionConf], format) match {
        case Some(tc) => (tc.cls, tc.args)
        case None => (format,Nil) // fallback: treat format as class name
      }
      val ext = addExtensionO(className, extraArgs ::: args)
      ext map {
        case e: E@unchecked if cls.isInstance(e) => e
        case _ => throw RegistrationError(s"extension for $format exists but has unexpected type")
      }
    }
  }

  /** like getOrAddExtension, but also checks for a possible exporter key */
  def getOrAddExtensionOrExporter[E <: FormatBasedExtension](cls: Class[E], format: String): Option[E] = {
    if (format.endsWith("_content") || format.endsWith("_narration")) {
      val exporter = format.substring(0, format.lastIndexOf('_'))
      getOrAddExtension(classOf[Exporter], exporter)
    }
    getOrAddExtension(cls, format)
   }

  val report = controller.report
  val logPrefix = "extman"

  /** instantiates an extension, initializes it, and adds it
    *
    * @param cls qualified class name (e.g., org.my.Extension), must be on the class path at run time
    * @param args arguments that will be passed when initializing the extension
    */
  def addExtension(cls: String, args: List[String]): Extension = {
    log("trying to create extension " + cls)
    val clsJ = try {
       try {
         Class.forName(cls)
       } catch {case e: ClassNotFoundException =>
         controller.backend.loadClass(cls).getOrElse(throw e)
       }
    } catch {
      case e: Throwable =>
        // need to catch all Exceptions and Errors here because NoClassDefFoundError is not an Exception
        throw RegistrationError("error while trying to load class " + cls).setCausedBy(e)
    }
    extensions.find(e => e.getClass == clsJ).foreach {e =>
       log("... already loaded, skipping")
       return e
    }
    val ext = try {
      val Ext = clsJ.asInstanceOf[Class[Extension]]
      Ext.getDeclaredConstructor().newInstance()
    } catch {
      case e: Exception =>
        throw RegistrationError("error while trying to instantiate class " + cls).setCausedBy(e)
    }
    addExtension(ext, args)
    ext
  }

  /** like addExtension but throws no errors */
  def addExtensionO(cls: String, args: List[String]): Option[Extension] = {
    try {
      Some(addExtension(cls, args))
    } catch {
      case r: RegistrationError => None
    }
  }

  /** initializes and adds an extension */
  def addExtension(ext: Extension, args: List[String] = Nil): Unit = {
    log("adding extension " + ext.getClass.toString)
    ext.init(controller)
    extensions ::= ext
    ext match {
      case pl: Plugin =>
        val loadedPlugins = get(classOf[Plugin])
        log("  ... as plugin")
        pl.dependencies foreach { d => if (!loadedPlugins.exists(_.getClass == java.lang.Class.forName(d))) addExtension(d, Nil) }
      case _ =>
    }
    try {
      ext.start(args)
    } catch {
      case e: Error =>
        removeExtension(ext)
        throw RegistrationError("error while starting extension: " + e.getMessage).setCausedBy(e)
      case e: Exception =>
        removeExtension(ext)
        e.printStackTrace()
        throw RegistrationError("unknown error while starting extension: " + e.getClass.toString + ": " + e.getMessage).setCausedBy(e)
    }
    knownExtensionTypes.foreach { cls =>
      if (cls.isInstance(ext))
        log("... as " + cls.getName)
    }
  }

  /** remove an extension (must have been stopped already) */
  def removeExtension(ext: Extension): Unit = {
    extensions = extensions diff List(ext)
  }

  /** retrieves an applicable parser extension */
  def getParserExtension(se: StructuralElement, keyword: String): Option[ParserExtension] =
    get(classOf[ParserExtension]) find {
      _.isApplicable(se, keyword)
    }

  def stringDescription: String = {
    knownExtensionTypes.map {cls =>
      val es = get(cls)
      if (es.isEmpty) "" else cls.getName + "\n" + es.map("  " + _.toString + "\n").mkString("") + "\n\n"
    }.mkString("")
  }

  def addDefaultExtensions: Unit = {
    // MMT's defaults for the main algorithms
    val nbp = new NotationBasedParser
    val kwp = new KeywordBasedParser(nbp)
    val rbc = new RuleBasedChecker
    val msc = new MMTStructureChecker(rbc)
    val nbpr = new NotationBasedPresenter {
      override def twoDimensional = false
    }
    val msp = new MMTSyntaxPresenter(nbpr)
    val rbs = new RuleBasedSimplifier
    val mss = new ElaborationBasedSimplifier(rbs)
    val mmtint = new TwoStepInterpreter(kwp, msc, mss)// with MMTStructureEstimator
    // a fast interpreter that only parses structure and does not check
    /*
    does not work well yet because (co)domains and rules are also parsed by the object-parser and then cause parser errors
    val fastparse = new KeywordBasedParser(DefaultObjectParser)
    val fastint = new OneStepInterpreter(fastparse) {
      override def format = "mmtstructure"
      override def inExts = mmtint.inExts
    } */
    val rbe = new execution.RuleBasedExecutor
    //use this for identifying structure and thus dependencies
    //val mmtStructureOnly = new OneStepInterpreter(new KeywordBasedParser(DefaultObjectParser))
    val mmtextr = ontology.MMTExtractor

    val rbp = new RuleBasedProver

    List(new XMLStreamer, nbp, kwp, rbc, msc, mmtint, nbpr, rbs, mss, msp, mmtextr, rbp, rbe).foreach {e => addExtension(e)}
    // build manager
    addExtension(new TrivialBuildManager)
    // pragmatic-strict converter
    addExtension(new notations.Pragmatics)
    //targets, opaque formats, and presenters
    val mp = new PresentationMathMLPresenter
    val hp = new HTMLPresenter(mp) {
      val key = "html"
    }
    List(mp, hp, new uom.GenericScalaExporter,
      new TextInterpreter, new HTMLInterpreter, TextPresenter, OMDocPresenter,
      new MMTSyntaxPresenter(nbpr), new FlatMMTSyntaxPresenter(nbpr)).foreach(addExtension(_))
    //parser extensions
    List(new symbols.RuleConstantParser, parser.MetadataParser, parser.CommentIgnorer).foreach(addExtension(_))
    //serverPlugins
    List(new web.GetActionServer, new web.SVGServer, new web.QueryServer, new web.SearchServer,
      new web.TreeView, new web.BreadcrumbsServer, new web.ActionServer, new web.ContextMenuAggregator, new web.MessageHandler,
      new web.SubmitCommentServer, new JSONBasedGraphServer, new SyntaxPresenterServer).foreach(addExtension(_))
    //queryExtensions
    import ontology._
    List(new Parse, new Infer, new Analyze, new Simplify, new Present, new PresentDecl).foreach(addExtension(_))
    // graphs: loading these here is practical even though they need the path to dot (which will be retrieved via getEnvvar)
    List(new DeclarationTreeExporter, new DependencyGraphExporter, new TheoryGraphExporter).foreach(addExtension(_))
    // shell extensions
    List(new ShellSendCommand, new execution.ExecuteFromShell, new Make, new RunFile).foreach(addExtension(_))

    addExtension(new AbbreviationRuleGenerator)
    
    // action companions
    List(NoActionCompanion,
        ListReportGroupsCompanion, AddReportHandlerCompanion, LoggingOnCompanion, LoggingOffCompanion,
        ExecFileCompanion, ScalaCompanion,
        InspectDefineCompanion, DefineCompanion, EndDefineCompanion, DoCompanion,
        CheckCompanion, CheckTermCompanion, NavigateCompanion, NavigateSourceCompanion,
        ShowArchivesCompanion, LocalCompanion, AddArchiveCompanion, AddMathPathFSCompanion,
        ServerInfoActionCompanion, ServerOnCompanion, ServerOffCompanion,
        MMTInfoCompanion, MMTLegalCompanion, MMTVersionCompanion, ClearConsoleCompanion, PrintAllCompanion, PrintAllXMLCompanion, PrintConfigCompanion, HelpActionCompanion,
        ShowLMHCompanion, SetLMHRootCompanion, LMHInitCompanion, LMHOpenCompanion, LMHUseCompanion, LMHInstallCompanion, LMHListCompanion, LMHPullCompanion, LMHPushCompanion, LMHSetRemoteCompanion, LMHListRemoteCompanion,
        ClearCompanion, ExitCompanion, SetBaseCompanion, SuppressErrorsCompanion,
        ListExtensionsCompanion, AddExtensionCompanion, RemoveExtensionCompanion, AddMWSCompanion,
        WindowCloseCompanion, WindowPositionCompanion, GUIOnCompanion, GUIOffCompanion,
        ArchiveBuildCompanion, ArchiveMarCompanion, ArchiveNewCompanion
    ).foreach{e => addExtension(e)}
    // This **must** be at the end, to act as a default for stuff
    addExtension(GetActionCompanion)
  }

  def clear: Unit = {
    extensions.foreach(_.clear)
  }

  def cleanup: Unit = {
    extensions.foreach(_.destroy)
    extensions = Nil
  }
}
