package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import archives._
import utils._

/**
 * parent class for classes generated from .mbt
 *
 * all methods defined in this class can be used inside .mbt scripts
 */
abstract class MMTScript extends Extension {
  //MMT Actions
  def logToFile(f: String, ts: Boolean = false) {
    controller.report.addHandler(new TextFileHandler(File(f), ts))
  }

  def logToConsole() {
    controller.report.addHandler(ConsoleHandler)
  }

  def logToHTML(f: String) {
    controller.report.addHandler(new HtmlFileHandler(File(f)))
  }

  def logModule(s: String) {
    controller.report.groups += s
  }

  def addArchive(location: String) {
    controller.addArchive(File(location))
  }

  def loadExtension(uri: String, args: List[String] = Nil) {
     controller.extman.addExtension(uri, args)
  }

  /**
   * loads a configuration file and appends to the current [[MMTConfig]]
   *
   * @param f the configuration file
   * @param autoload also load all archives and extensions in f
   */
  def loadConfig(f: String, autoload: Boolean = true) {
     val conf = MMTConfig.parse(File(f))
     controller.loadConfig(conf, autoload)
     loadRelationalFor(!_.readonly) //TODO for now only loading writable archives for efficiency
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
    archives.foreach(a => runImporters(a.id, Build(Update(Level.Error))))
    archives.foreach(a => runExporters(a.id, Build(Update(Level.Error))))
  }

  def plainBuild(btm: BuildTargetModifier) {
    val archives = config.getArchives
    archives.foreach(a => runImporters(a.id, btm))
    archives.foreach(a => runExporters(a.id, btm))
  }

  def loadRelationalFor(select : ArchiveConf => Boolean) {
    val aids = config.getArchives.filter(select).map(_.id)
    controller.archiveBuildAction(aids, "relational", Build, EmptyPath)
  }

  def smartBuild(mod : String, profile : String, targets : List[String]) {
    controller.confBuildAction(mod, targets, profile)
  }

  def make(comp : String, files : List[String]) = controller.makeAction(comp, files)

  def build(ids: List[String], target: String, modifier: archives.BuildTargetModifier, in: FilePath = EmptyPath) {
    controller.archiveBuildAction(ids, target, modifier, in)
  }
  /**
   * run the script; the content of the .mbt file will be used to implement this method
   */
  def main
}

/**
 * executes a file as an [[MMTScript]] by calling Scala reflection
 */
class MMTScriptEngine(controller: Controller) {
  /**
   * @param f an .mbt file containing scala code that is valid inside a class extending [[MMTScript]]
   */
  def apply(f: File, expr: String = "") {
    val code = File.read(f)
    val imports = """
      import info.kwarc.mmt.api._
      import info.kwarc.mmt.api.archives._
      import frontend._
    """
    val textscript = imports +
      s"object UserScript extends info.kwarc.mmt.api.frontend.MMTScript {\ndef main() {\n$code\n$expr\n}\n}\nUserScript"
    import scala.reflect.runtime._
    val cm = universe.runtimeMirror(getClass.getClassLoader)
    import scala.tools.reflect.ToolBox
    val tb = cm.mkToolBox()
    //println(textscript)
    val scalascript = tb.eval(tb.parse(textscript))
    scalascript match {
      case s: MMTScript =>
        s.init(controller)
        s.main
      case _ =>
    }
  }
}
