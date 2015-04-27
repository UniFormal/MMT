package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import backend._
import presentation._
import libraries._
import archives.BuildTarget
import ontology.QueryExtension
import parser._
import checking._
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

   protected def getFromFirstArgOrEnvvar(args: List[String], name: String): String = {
      args match {
         case Nil => controller.getEnvVar(name).getOrElse(throw LocalError(s"1 argument or environment variable $name needed"))
         case hd::Nil => hd
         case _ => throw LocalError("too many arguments: " + args.mkString(" ") + "; expected: 1") 
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
    *  @param format the format/key/other identifier, for which an extension is needed
    *  @return true if this extension is applicable
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
   
   private[api] var extensions : List[Extension] = Nil
   private val knownExtensionTypes = List(
         classOf[Plugin], classOf[Foundation], classOf[ParserExtension], classOf[QueryTransformer],
         classOf[QueryExtension],
         classOf[ChangeListener], classOf[ServerExtension],
         classOf[Parser], classOf[Checker], classOf[Interpreter], classOf[Presenter],
         classOf[BuildTarget]
         )
         

   def get[E <: Extension](cls: Class[E]): List[E] = extensions.flatMap {e =>
      if (cls.isInstance(e)) List(e.asInstanceOf[E]) else Nil
   }
   def get[E <: FormatBasedExtension](cls: Class[E], format: String): Option[E] = extensions.flatMap {e =>
      if (cls.isInstance(e)) {
         val fbe = e.asInstanceOf[E]
         if (fbe.isApplicable(format)) List(fbe)
         else Nil
      } else Nil
   }.headOption
   
   var lexerExtensions : List[LexerExtension] = Nil
   var notationExtensions: List[notations.NotationExtension] = Nil

   var mws : Option[ontology.MathWebSearch] = None

   val report = controller.report
   val logPrefix = "extman"

   def addDefaultExtensions {
      // MMT's defaults for the main algorithms
      val nbp = new NotationBasedParser
      val kwp = new KeywordBasedParser(nbp)
      val rbc = new RuleBasedChecker
      val msc = new MMTStructureChecker(rbc)
      val mmtint = new TwoStepInterpreter(kwp, msc)
      List(new XMLStreamer, nbp, kwp, rbc, msc, mmtint).foreach {e => addExtension(e)}
      //targets and presenters
      List(new archives.HTMLExporter, new archives.PythonExporter, new uom.ScalaExporter, new uom.OpenMathScalaExporter,
           TextPresenter, OMDocPresenter, controller.presenter).foreach {
        e => addExtension(e)
      }
      //parser
      List(new symbols.RuleConstantParser, parser.MetadataParser, parser.CommentIgnorer).foreach(addExtension(_))
      //parserExtensions ::= new ControlParser
      //serverPlugins
      List(new web.GetActionServer, new web.SVGServer, new web.QueryServer, new web.SearchServer,
            new web.TreeView, new web.BreadcrumbsServer, new web.ActionServer, new web.AlignServer, 
            new web.SubmitCommentServer).foreach(addExtension(_))
      //queryExtensions
      List(new ontology.Parse, new ontology.Infer, new ontology.Analyze, new ontology.Simplify,
                                new ontology.Present, new ontology.PresentDecl).foreach(addExtension(_))
      lexerExtensions ::= GenericEscapeLexer
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
       extensions ::= ext
       if (ext.isInstanceOf[Plugin]) {
          val loadedPlugins = get(classOf[Plugin])
          log("  ... as plugin")
          val pl = ext.asInstanceOf[Plugin]
          pl.dependencies foreach {d => if (! loadedPlugins.exists(_.getClass == java.lang.Class.forName(d))) addExtension(d, Nil)}
       }
       try {
          ext.start(args)
       } catch {
          case e: Error =>
             removeExtension(ext)
             throw RegistrationError("error while starting extension: " + e.getMessage).setCausedBy(e)
          case e: Exception =>
             removeExtension(ext)
             throw RegistrationError("unknown error while starting extension: " + e.getClass.toString + ": " + e.getMessage).setCausedBy(e)
       }
       knownExtensionTypes.foreach {cls =>
          if (cls.isInstance(ext))
             log("... as " + cls.getName)
       }
   }
   
   /** remove an extension (must have been stopped already) */
   def removeExtension(ext: Extension) {
      extensions = extensions diff List(ext)
   }

   /** retrieves an applicable parser extension */
   def getParserExtension(se: StructuralElement, keyword: String) : Option[ParserExtension] =
      get(classOf[ParserExtension]) find {_.isApplicable(se, keyword)}
   /** retrieves the closest Foundation that covers a theory, if any */
   def getFoundation(p: MPath) : Option[Foundation] = get(classOf[Foundation]) find {_.foundTheory == p} orElse {
      val mt = objects.TheoryExp.metas(objects.OMMOD(p))(controller.globalLookup)
      mt mapFind getFoundation
   }
   def getFoundation(thy: objects.Term) : Option[Foundation] =
      objects.TheoryExp.metas(thy)(controller.globalLookup) mapFind getFoundation
   
   def stringDescription = {
      knownExtensionTypes.map {cls =>
         val es = get(cls)
         if (es.isEmpty) "" else cls.getName + "\n" + es.map("  " + _.toString + "\n").mkString("") + "\n\n"
      }.mkString("")
   }
   
   def cleanup {
      extensions.foreach(_.destroy)
      extensions = Nil
   }
}