package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import backend._
import presentation._
import libraries._
import parser._
import utils._

trait Extension {
   protected var controller : Controller = null
   protected var report : Report = null
   /** initialization (empty by default) */
   def init(controller: Controller, args: List[String]) {
      this.controller = controller
      report = controller.report
   }
   /** termination (empty by default)
    *
    * Extensions may create persistent data structures and processes,
    * but they must clean up after themselves in this method
    */
   def destroy {}
}

/** An ExtensionManager maintains all language-specific extensions of MMT.
 *  This includes Compilers, QueryTransformers, and Foundations.
 *  They are provided as plugins and registered via their qualified class name, which is instantiated by reflection.
 */
class ExtensionManager(controller: Controller) {
   private val report = controller.report
   private var foundations : List[Foundation] = Nil
   private var compilers : List[Compiler] = List(MMTCompiler)
   private var querytransformers : List[QueryTransformer] = Nil
   private var presenters : List[Presenter] = List(TextPresenter,OMDocPresenter)
   private var mws : Option[URI] = None

   private def log(msg : => String) = report("extman", msg)

   val ruleStore = new objects.RuleStore
   val pragmaticStore = new pragmatics.PragmaticStore

   private var loadedPlugins : List[java.lang.Class[_ <: Plugin]] = Nil
   def addPlugin(pl: Plugin, args: List[String]) {
       loadedPlugins ::= pl.getClass
       pl.dependencies foreach {d => if (! (loadedPlugins contains d)) addPlugin(d, Nil)}
       pl.init(controller, args)
   }
   def addPlugin(cls: String, args: List[String]) {
       log("adding plugin " + cls)
       val pl = try {
          val Pl = java.lang.Class.forName(cls).asInstanceOf[java.lang.Class[Plugin]]
          Pl.newInstance
       } catch {
          case e : java.lang.Throwable => throw ExtensionError("error while trying to instantiate class " + cls).setCausedBy(e) 
       }
       addPlugin(pl, args)
   }
   /** adds an Importer and initializes it */
   def addExtension(cls: String, args: List[String]) {
       log("adding importer " + cls)
       val ext = try {
          val Ext = java.lang.Class.forName(cls).asInstanceOf[java.lang.Class[Extension]]
          Ext.newInstance
       } catch {
          case e : java.lang.Throwable => throw ExtensionError("error while trying to instantiate class " + cls).setCausedBy(e) 
       }
       ext.init(controller, args)
       if (ext.isInstanceOf[Compiler]) {
          log("  ... as compiler")
          compilers ::= ext.asInstanceOf[Compiler]
       }
       if (ext.isInstanceOf[QueryTransformer]) {
          log("  ... as query transformer")
          querytransformers ::= ext.asInstanceOf[QueryTransformer]
       }
       if (ext.isInstanceOf[Presenter]) {
          log("  ... as presenter")
          presenters ::= ext.asInstanceOf[Presenter]
       }
   }

   /** retrieves an applicable Compiler */
   def getCompiler(src: String) : Option[Compiler] = compilers.find(_.isApplicable(src))
   /** retrieves an applicable Compiler */
   def getQueryTransformer(src: String) : Option[QueryTransformer] = querytransformers.find(_.isApplicable(src))
   /** retrieves an applicable Presenter */
   def getPresenter(format: String) : Option[Presenter] = presenters.find(_.isApplicable(format))
  
   /** adds a foundation, must be initialized already */
   def addFoundation(cls: String, args: List[String]) {
       log("adding foundation " + cls)
       val found = try {
          val Found = java.lang.Class.forName(cls).asInstanceOf[java.lang.Class[Foundation]]
          Found.newInstance
       } catch {
          case e : java.lang.ClassNotFoundException => throw ExtensionError("cannot instantiate class " + cls).setCausedBy(e)
       }
       found.init(report, args)
       foundations ::= found
   }
   /** retrieves an applicable Foundation */
   def getFoundation(p: MPath) : Option[Foundation] = foundations find {_.foundTheory == p}

   /** sets the URL of the MathWebSearch backend */
   def setMWS(uri: URI) {mws = Some(uri)}
   def getMWS : Option[URI] = mws
   
   def cleanup {
      compilers.foreach(_.destroy)
      compilers = Nil
   } 

}

//info.kwarc.mmt.mizar.test.MwsService
// info.kwarc.mmt.tptp.TptpCompiler