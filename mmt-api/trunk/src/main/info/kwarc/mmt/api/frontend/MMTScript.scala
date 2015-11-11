package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.utils._

/**
 * parent class for classes generated from .mbt
 * 
 * all methods defined in this class can be used inside .mbt scripts
 */
abstract class MMTScript extends Extension {
  //MMT Actions
  def logToFile(f: String, ts: Boolean = false): Unit = controller.handle(AddReportHandler(new TextFileHandler(File(f), ts)))

  def logToConsole(): Unit = controller.handle(AddReportHandler(ConsoleHandler))

  def logToHTML(f: String): Unit = controller.handle(AddReportHandler(new HtmlFileHandler(File(f))))

  def logModule(s: String): Unit = controller.handle(LoggingOn(s))

  def addArchive(location: String): Unit = controller.handle(AddArchive(File(location)))

  def loadExtension(uri: String, args: List[String] = Nil) {
     controller.handle(AddExtension(uri, args))
  }
  
  /**
   * loads a configuration file and appends to the current [[MMTConfig]]
   * 
   * @param f the configuration file
   * @param autoload also load all archives and extensions in f
   */
  def loadConfig(f: String, autoload: Boolean = true) {
     val conf = MMTConfig.parse(File(f))
     controller.getConfig.add(conf)
     if (autoload) {
        conf.loadAllNeededTargets(controller)
        conf.loadAllArchives(controller)
     }
  }
     

  //Utility

  def config = controller.getConfig
  
  def runImporters(aid: String, btm: BuildTargetModifier = Build) {
    config.getImportersForArchive(aid) foreach {imp =>
      build(List(aid), imp, btm)
    }
  }

  def runExporters(aid: String, btm: BuildTargetModifier = Build) {
    config.getExportersForArchive(aid) foreach {exp =>
      build(List(aid), exp, btm)
    }
  }

  def cleanBuild() {
    val archives = config.getArchives
    archives.foreach(a => runImporters(a.id, Clean))
    archives.foreach(a => runImporters(a.id, Build))
    archives.foreach(a => runExporters(a.id, Clean))
    archives.foreach(a => runExporters(a.id, Build))
  }

  def updateBuild(ifHadErrors: Boolean) {
    val archives = config.getArchives
    archives.foreach(a => runImporters(a.id, Update(Level.Error)))
    archives.foreach(a => runExporters(a.id, Update(Level.Error)))
  }

  def plainBuild(btm: BuildTargetModifier): Unit = {
    val archives = config.getArchives
    archives.foreach(a => runImporters(a.id, btm))
    archives.foreach(a => runExporters(a.id, btm))
  }

  def compUpdateBuild(changedCompsIds: List[String], btm: BuildTargetModifier): Unit = {
    config.getArchives foreach {a =>
      config.getArchive(a.id).formats foreach {f =>
        val imps = config.getImporters(f)
        val exps = config.getExporters(f)
        var foundChanged = false
        imps foreach { imp =>
          if (changedCompsIds.contains(imp)) foundChanged = true
          if (foundChanged) build(List(a.id), imp, Build)
        }

        exps foreach { exp =>
          if (changedCompsIds.contains(exp) || foundChanged) build(List(a.id), exp, Build)
        }
      }
    }
  }

  def build(ids: List[String], target: String, modifier: archives.BuildTargetModifier, in: FilePath = EmptyPath): Unit = {
    controller.handle(ArchiveBuild(ids, target, modifier, in))
  }

  /**
   * run the script; the content of the .mbt file will be used to implement this method
   */
  def main(): Unit
}

/**
 * executes a file as an [[MMTScript]] by calling Scala reflection
 */
class MMTScriptEngine(controller: Controller) {
  /**
   * @param f an .mbt file containing scala code that is valid inside a class extending [[MMTScript]]
   */
  def apply(f: File) {
    val code = File.read(f)
    val imports = "import info.kwarc.mmt.api._\nimport info.kwarc.mmt.api.archives.{Build}\n"
    val textscript = imports + s"object UserScript extends info.kwarc.mmt.api.frontend.MMTScript {\ndef main() {\n$code\n}\n}\nUserScript"
    import scala.reflect.runtime._
    val cm = universe.runtimeMirror(getClass.getClassLoader)
    import scala.tools.reflect.ToolBox
    val tb = cm.mkToolBox()
    //println(textscript)
    val scalascript = tb.eval(tb.parse(textscript))
    scalascript match {
      case s: MMTScript =>
        s.init(controller)
        s.main()
      case _ =>
    }
  }
}
