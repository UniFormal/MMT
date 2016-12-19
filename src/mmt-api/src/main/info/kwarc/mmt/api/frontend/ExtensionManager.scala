package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import archives._
import backend._
import checking._
import libraries._
import opaque._
import parser._
import presentation._
import proving._
import uom._
import utils._
import utils.MyList._
import web._


trait Extension extends Logger {
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
  protected def catchErrors(msg: String)(code: => Unit) {
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
    protected def addError(e: Error) {
      throw LocalError(msg).setCausedBy(e)
    }
  }


  /** MMT initialization */
  private[api] def init(controller: Controller) {
    this.controller = controller
    report = controller.report
  }

  /** any extension can initialize other extensions if those are not meant to be added to the ExtensionManager */
  protected def initOther(e: Extension) {
     e.init(controller)
  }

  protected def getFromFirstArgOrEnvvar(args: List[String], name: String,
                                        default: String = ""): String = {
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
  def start(args: List[String]) {}

  /** extension-specific cleanup (override as needed, empty by default)
    *
    * Extensions may create persistent data structures and processes,
    * but they must clean up after themselves in this method
    */
  def destroy {}
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

  override def init(controller: Controller) {
    objectLevel.init(controller)
    super.init(controller)
  }

  override def destroy {
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
    classOf[Plugin], classOf[Foundation], classOf[ParserExtension], classOf[QueryTransformer],
    classOf[ontology.QueryExtension],
    classOf[ChangeListener], classOf[ServerExtension],
    classOf[Parser], classOf[Checker], classOf[Prover], classOf[Interpreter], classOf[Simplifier], classOf[Presenter],
    classOf[BuildTarget]
  )


  def get[E <: Extension](cls: Class[E]): List[E] = extensions.collect {
    case e: E@unchecked if cls.isInstance(e) => e
  }

  def get[E <: FormatBasedExtension](cls: Class[E], format: String): Option[E] = extensions.collectFirst {
    case e: E@unchecked if cls.isInstance(e) && e.isApplicable(format) => e
  }

  /** like get, but if necessary looks up the key in the current [[MMTConfig]]
    *
    * @param args additional arguments for the extension (appended to those in configuration); ignored if extension has already been created
    */
  def getOrAddExtension[E <: FormatBasedExtension](cls: Class[E], format: String, args: List[String] = Nil): Option[E] = {
    get(cls, format) orElse {
      controller.getConfig.getEntry(classOf[ExtensionConf], format) map {tc =>
         val ext = addExtension(tc.cls, tc.args ::: args)
         ext match {
           case e: E@unchecked if cls.isInstance(e) => e
           case _ => throw RegistrationError(s"extension for $format exists but has unexpected type")
         }
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
       Class.forName(cls)
    } catch {
      case e: Exception => throw RegistrationError("error while trying to load class " + cls).setCausedBy(e)
    }
    extensions.find(e => e.getClass == clsJ).foreach {e =>
       log("... already loaded, skipping")
       return e
    }
    val ext = try {
      val Ext = clsJ.asInstanceOf[Class[Extension]]
      Ext.newInstance
    } catch {
      case e: Exception => throw RegistrationError("error while trying to instantiate class " + cls).setCausedBy(e)
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
  def addExtension(ext: Extension, args: List[String] = Nil) {
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
  def removeExtension(ext: Extension) {
    extensions = extensions diff List(ext)
  }

  /** retrieves an applicable parser extension */
  def getParserExtension(se: StructuralElement, keyword: String): Option[ParserExtension] =
    get(classOf[ParserExtension]) find {
      _.isApplicable(se, keyword)
    }

  /** retrieves the closest Foundation that covers a theory, if any */
  def getFoundation(p: MPath): Option[Foundation] = get(classOf[Foundation]) find {
    _.foundTheory == p
  } orElse {
    val mt = objects.TheoryExp.metas(objects.OMMOD(p))(controller.globalLookup)
    mt mapFind getFoundation
  }

  def getFoundation(thy: objects.Term): Option[Foundation] =
    objects.TheoryExp.metas(thy)(controller.globalLookup) mapFind getFoundation

  def stringDescription: String = {
    knownExtensionTypes.map {cls =>
      val es = get(cls)
      if (es.isEmpty) "" else cls.getName + "\n" + es.map("  " + _.toString + "\n").mkString("") + "\n\n"
    }.mkString("")
  }

  def addDefaultExtensions {
    // MMT's defaults for the main algorithms
    val nbp = new NotationBasedParser
    val kwp = new KeywordBasedParser(nbp)
    val rbc = new RuleBasedChecker
    val msc = new MMTStructureChecker(rbc)
    val mmtint = new TwoStepInterpreter(kwp, msc)// with MMTStructureEstimator
    val nbpr = new NotationBasedPresenter {
      override def twoDimensional = false
    }
    val msp = new MMTStructurePresenter(nbpr)
    val rbs = new RuleBasedSimplifier
    val mss = new MMTStructureSimplifier(rbs)
    val rbe = new execution.RuleBasedExecutor
    //use this for identifying structure and thus dependencies
    //val mmtStructureOnly = new OneStepInterpreter(new KeywordBasedParser(DefaultObjectParser))
    val mmtextr = ontology.MMTExtractor

    val rbp = new RuleBasedProver
    var prover: Extension = rbp
    //TODO temporary hack to replace old prover with Mark's AgentProver if the latter is on the classpath
    /*
    val className = "info.kwarc.mmt.leo.provers.AgentProver"
    try {
      prover = Class.forName(className).newInstance.asInstanceOf[Extension]
    } catch {
      case _: Exception =>
    }
    */

    List(new XMLStreamer, nbp, kwp, rbc, msc, mmtint, nbpr, rbs, mss, msp, mmtextr, prover, rbe).foreach {e => addExtension(e)}
    // build manager
    addExtension(new TrivialBuildManager)
    // pragmatic-strict converter
    addExtension(new notations.Pragmatics)
    //targets, opaque formats, and presenters
    List(new info.kwarc.mmt.api.presentation.HTMLExporter, new archives.PythonExporter, new uom.GenericScalaExporter, new uom.OpenMathScalaExporter,
      new TextInterpreter, new HTMLInterpreter,
      TextPresenter, OMDocPresenter).foreach {
      e => addExtension(e)
    }
    //parser
    List(new symbols.RuleConstantParser, parser.MetadataParser, parser.CommentIgnorer).foreach(addExtension(_))
    //parserExtensions ::= new ControlParser
    //serverPlugins
    List(new web.GetActionServer, new web.SVGServer, new web.QueryServer, new web.SearchServer,
      new web.TreeView, new web.BreadcrumbsServer, new web.ActionServer, new web.ContextMenuAggregator, new web.MessageHandler,
      new web.SubmitCommentServer).foreach(addExtension(_))
    //queryExtensions
    List(new ontology.Parse, new ontology.Infer, new ontology.Analyze, new ontology.Simplify,
      new ontology.Present, new ontology.PresentDecl).foreach(addExtension(_))
    // shell extensions
    List(new ShellSendCommand, new execution.ShellCommand, new Make).foreach(addExtension(_))
  }

  def cleanup {
    extensions.foreach(_.destroy)
    extensions = Nil
  }
}
