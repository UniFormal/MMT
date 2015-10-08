package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import utils._
import scala.collection.immutable.{ListMap}


/**
 * .mbt files schould contain a single object that extends MMTScript
 */
abstract class MMTScript extends Extension {
  //MMT Actions
  def logToFile(f : String, ts : Boolean = false) = controller.handle(AddReportHandler(new TextFileHandler(File(f), ts)))
  def logToConsole() = controller.handle(AddReportHandler(ConsoleHandler))
  def logToHTML(f : String) = controller.handle(AddReportHandler(new HtmlFileHandler(File(f))))
  def logModule(s : String) = controller.handle(LoggingOn(s))
  def addArchive(location : String) = controller.handle(AddArchive(File(location)))
  def loadExtension(uri : String, args : List[String] = Nil) = controller.handle(AddExtension(uri, args))
  
  def config = controller.config
  //Utility
  
    def runImporters(aid : String, btm : BuildTargetModifier = Build) = {
      config.getArchive(aid).formats.flatMap(config.getImporters).distinct foreach {imp =>
        build(List(aid), imp, btm)
      }
    }
    
    def runExporters(aid : String, btm : BuildTargetModifier = Build) = {
      config.getArchive(aid).formats.flatMap(config.getImporters).distinct foreach {imp =>
        build(List(aid), imp, btm)
      }
    }
    
    def cleanBuild() {
      config.archives.foreach(a => runImporters(a.id, Clean))
      config.archives.foreach(a => runImporters(a.id, Build))
      config.archives.foreach(a => runExporters(a.id, Clean))
      config.archives.foreach(a => runExporters(a.id, Build))
    }
    
    def updateBuild(ifHadErrors : Boolean) {
      config.archives.foreach(a => runImporters(a.id, Update(ifHadErrors)))
      config.archives.foreach(a => runExporters(a.id,  Update(ifHadErrors)))
    }
    
    def plainBuild(btm : BuildTargetModifier) = {
      config.archives.foreach(a => runImporters(a.id, btm))
      config.archives.foreach(a => runExporters(a.id, btm))
    }
    
    def compUpdateBuild(changedCompsIds : List[String], btm : BuildTargetModifier) = {
       config.archives foreach {a => 
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
    
    def parse(f : String, autoload : Boolean = true) = {
      val file = File(f)
      val s = File.read(file)
      val lines = s.split("\n")
      var section = ""
      for (line <- lines) {
        if (line.startsWith("//")) {
          //ignore
        } else if (line.startsWith("#")) {
          section = line.substring(1)
        } else section match {
          case "importers" => line.split(" ").toList match {
            case uri :: key :: args => 
              config.addImporter(ImporterConf(uri, key, args))
            case _ => println("Invalid importer line: `" + line + "`")
          }
          case "exporters" => line.split(" ").toList match {
            case uri :: key :: args => 
              config.addExporter(ExporterConf(uri, key, args))
            case _ => println("Invalid exporter line: `" + line + "`")
          }
          case "archives" => line.split(" ").toList match {
            case id :: fmtsS :: Nil => 
              val fmts = fmtsS.split(",").toList
              config.addArchive(ArchiveConf(id, fmts))
            case _ => println("Invalid archives line: `" + line + "`")
          }
          case "formats" => line.split(" ").toList match {
            case id :: impsS :: expsS :: Nil => 
              val imps = impsS.split(",").toList
              val exps = expsS.split(",").toList
              config.addFormat(FormatConf(id, imps, exps))
            case _ => println("Invalid formats line: `" + line + "`")
          }
          case "base" => config.setBase(line)
          case s => println("ignoring invalid section: `" + s + "`") 
        }
      }
    }
  
  
  def build(ids : List[String], target : String, modifier: archives.BuildTargetModifier, in : FilePath = EmptyPath) : Unit = {
    controller.handle(ArchiveBuild(ids, target, modifier, in))
  }
  
   def main()
}

class MMTScriptEngine(controller: Controller) {
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