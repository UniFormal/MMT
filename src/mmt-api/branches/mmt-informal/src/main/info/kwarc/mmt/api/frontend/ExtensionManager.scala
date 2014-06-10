package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import backend._
import presentation._
import libraries._
import archives.BuildTarget
import ontology.QueryExtension
import parser._
import utils._
import web._
import utils.MyList._

trait Extension extends Logger {
   protected var controller : Controller = null
   protected var report : Report = null
   
   lazy val defaultPrefix = {
      val s = getClass.toString
      val p = s.lastIndexOf(".")
      if (p == -1) s else s.substring(p+1)
   }
   /** the prefix used to identify this extension for logging, by default the class name */
   def logPrefix = defaultPrefix
   
   /** a custom error class for this extension */
   case class LocalError(s: String) extends ExtensionError(logPrefix, s)
   
   /** MMT initialization */
   private[api] def init(controller: Controller) {
      this.controller = controller
      report = controller.report
   }
   
   protected def checkNumberOfArguments(min: Int, max: Int, args: List[String]) {
      if (args.length < min || args.length > max)
         throw LocalError("bad number of arguments: " + args.mkString(" ") + "; expected: " + min + " to " + max)
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

/**
 * Common super class of all extensions, whose functionality is systematically split between structure and object level
 * 
 * Implementing classes should have a constructor that takes an Extension providing the object level functionality
 * and add the structure level functionality.  
 */ 
trait LeveledExtension extends Extension {
   val objectLevel: Extension
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
 *  This includes Compilers, QueryTransformers, and Foundations.
 *  They are provided as plugins and registered via their qualified class name, which is instantiated by reflection.
 */
class ExtensionManager(controller: Controller) extends Logger {
   
   private[api] var foundations   : List[Foundation]   = Nil
   private[api] var targets       : List[BuildTarget]  = Nil
   private[api] var querytransformers : List[QueryTransformer] = Nil
   private[api] var changeListeners   : List[ChangeListener]  = Nil
   private[api] var presenters    : List[Presenter]    = Nil
   private[api] var serverPlugins : List[ServerExtension] = Nil
   private[api] var loadedPlugins : List[Plugin]       = Nil
   private[api] var parserExtensions : List[ParserExtension] = Nil
   private[api] var queryExtensions : List[QueryExtension] = Nil

   var lexerExtensions : List[LexerExtension] = Nil
   var notationExtensions: List[notations.NotationExtension] = Nil

   var mws : Option[ontology.MathWebSearch] = None

   val report = controller.report
   val logPrefix = "extman"

   val ruleStore = new checking.RuleStore
   
   def addDefaultExtensions {
      //targets and presenters
      addExtension(new MMTCompiler)
      List(new archives.HTMLExporter, new archives.PythonExporter, new uom.ScalaExporter, new uom.OpenMathScalaExporter,
           TextPresenter, OMDocPresenter, controller.presenter).foreach {
        e => addExtension(e)
      }
      //changeListeners
      List(new modules.RealizationListener, parser.MetadataParser, parser.CommentIgnorer).foreach(addExtension(_))
      //parserExtensions ::= new ControlParser
      //serverPlugins
      List(new web.ActionServer, new web.SVGServer, new web.QueryServer, new web.SearchServer,
            new web.BreadcrumbsServer, new web.AdminServer).foreach(addExtension(_))
      //queryExtensions
      List(new ontology.Parse, new ontology.Infer, new ontology.Analyze, new ontology.Simplify,
                                new ontology.Present, new ontology.PresentDecl).foreach(addExtension(_))
      lexerExtensions ::= GenericEscapeLexer
      lexerExtensions ::= QuoteLexer
      lexerExtensions ::= UnicodeReplacer
      lexerExtensions ::= new PrefixedTokenLexer('\\')
      
      notationExtensions ::= notations.MixfixNotation
   }

   /** instantiates an extension, initializes it, and adds it
    *  @param cls qualified class name (e.g., org.my.Extension), must be on the class path at run time
    *  @param args arguments that will be passed when initializing the extension
    */
   def addExtension(cls: String, args: List[String]) {
       log("trying to create extension " + cls)
       val clsJ = java.lang.Class.forName(cls)
       val ext = try {
          val Ext = clsJ.asInstanceOf[java.lang.Class[Extension]]
          Ext.newInstance
       } catch {
          case e : Exception => throw RegistrationError("error while trying to instantiate class " + cls).setCausedBy(e) 
       }
       addExtension(ext, args)
   }
   /** initializes and adds an extension */
   def addExtension(ext: Extension, args: List[String] = Nil) {
       log("adding extension " + ext.getClass.toString)
       ext.init(controller)
       if (ext.isInstanceOf[Plugin]) {
          log("  ... as plugin")
          val pl = ext.asInstanceOf[Plugin]
          loadedPlugins ::= pl
          pl.dependencies foreach {d => if (! loadedPlugins.exists(_.getClass == java.lang.Class.forName(d))) addExtension(d, Nil)}
       }
       try {
          ext.start(args)
       } catch {
          case e: Error => throw RegistrationError("error while starting extension: " + e.getMessage).setCausedBy(e)
          case e: Exception => throw RegistrationError("unknown error while starting extension: " + e.getClass.toString + ": " + e.getMessage)
       }
       if (ext.isInstanceOf[Foundation]) {
          log("  ... as foundation")
          foundations ::= ext.asInstanceOf[Foundation]
       }
       if (ext.isInstanceOf[ChangeListener]) {
          log("  ... as change listener")
          changeListeners ::= ext.asInstanceOf[ChangeListener]
       }
       if (ext.isInstanceOf[BuildTarget]) {
          log("  ... as build target")
          targets ::= ext.asInstanceOf[BuildTarget]
       }
       if (ext.isInstanceOf[QueryTransformer]) {
          log("  ... as query transformer")
          querytransformers ::= ext.asInstanceOf[QueryTransformer]
       }
       if (ext.isInstanceOf[Presenter]) {
          log("  ... as presenter")
          presenters ::= ext.asInstanceOf[Presenter]
       }
       if (ext.isInstanceOf[ParserExtension]) {
          log("  ... as parser extension")
          parserExtensions ::= ext.asInstanceOf[ParserExtension]
       }
       if (ext.isInstanceOf[QueryExtension]) {
          log("  ... as query extension")
          queryExtensions ::= ext.asInstanceOf[QueryExtension]
       }
       if (ext.isInstanceOf[ServerExtension]) {
          log("  ... as server plugin")
          serverPlugins ::= ext.asInstanceOf[ServerExtension]
       }
   }

   /** retrieves an applicable build target */
   def getTarget(key: String) : Option[BuildTarget] = targets.find(t => t.key == key)
   /** retrieves an applicable query transformer */
   def getQueryTransformer(src: String) : Option[QueryTransformer] = querytransformers.find(_.isApplicable(src))
   /** retrieves an applicable Presenter */
   def getPresenter(format: String) : Option[Presenter] = presenters.find(_.isApplicable(format))
   /** retrieves an applicable server plugin */
   def getServerPlugin(cont : String) : Option[ServerExtension] = serverPlugins.find(_.isApplicable(cont))
   /** retrieves an applicable parser extension */
   def getParserExtension(se: StructuralElement, keyword: String) : Option[ParserExtension] = parserExtensions find {_.isApplicable(se, keyword)}
   /** retrieves the closest Foundation that covers a theory, if any */
   def getFoundation(p: MPath) : Option[Foundation] = foundations find {_.foundTheory == p} orElse {
      val mt = objects.TheoryExp.metas(objects.OMMOD(p))(controller.globalLookup)
      mt mapFind getFoundation
   }
   def getFoundation(thy: objects.Term) : Option[Foundation] =
      objects.TheoryExp.metas(thy)(controller.globalLookup) mapFind getFoundation
   
   /** retrieves all registered extensions */
   private def getAll = foundations:::targets:::querytransformers:::changeListeners:::presenters:::
                        serverPlugins:::parserExtensions:::queryExtensions:::loadedPlugins
   
   def stringDescription = {
      def mkL(label: String, es: List[Extension]) =
         if (es.isEmpty) "" else label + "\n" + es.map("  " + _.toString + "\n").mkString("") + "\n\n"
      mkL("foundations", foundations) +
      mkL("build targets", targets) +
      mkL("query transformers", querytransformers) +
      mkL("change listeners", changeListeners) +
      mkL("presenters", presenters) +
      mkL("server plugins", serverPlugins) +
      mkL("parser extensions", parserExtensions) +
      mkL("query extensions", queryExtensions) +
      mkL("plugins", loadedPlugins) +
      "rules\n" + ruleStore.stringDescription 
   }
   
   def cleanup {
      getAll.foreach(_.destroy)
      foundations = Nil
      targets = Nil
      querytransformers = Nil
      changeListeners = Nil
      presenters = Nil
      serverPlugins = Nil
      loadedPlugins = Nil
      parserExtensions = Nil
      queryExtensions = Nil
   }
}