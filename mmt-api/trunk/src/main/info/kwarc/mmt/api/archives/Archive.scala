package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import frontend._
import modules._
import lf._

import java.io.File
import scala.collection.mutable._

class Archive(root: File, report: Report) {
    private var base : Path = null
    private val files = new LinkedHashSet[File]
    private val modules = new HashMap[MPath,File]
    def log(msg: => String) = report("archive", msg)
/*    def init {
       val conf = utils.xml.readFile(new java.io.File(root, "META-INF.xml"))
       conf match {
          case <archive>{dims @ _*}</archive> =>
             base = Path.parse(utils.xml.attr(conf, "base"), null)
             dims foreach {
                case s @ <source/> =>
                  val label = utils.xml.attr(s, "language")
                  if (label == "twelf") {
                     val catalog = new Catalog(File(root, "source"), "elf")
                     catalog.start()
                     source = Some(catalog)
                  }
             }
       }
    }*/
    def MMTPathToFilePath(m: MPath) : java.io.File = null
    def narrToCont(controller : Controller) {
        // create content folder, iterate over all narration files using "files", put one theory per file into content folder (subfolder structure according to namespaces)
        // build index of all URIs
        val omdocfile = new java.io.File("")
        // controller.delete(omdocfile) or controller.clear
        val doc = controller.read(omdocfile)
        controller.getDocument(doc).getModulesResolved(controller.library) foreach put
    }
    def contToRel(controller: Controller) {}
    def contToPres(controller: Controller, label: String, style: MPath) {}
    def get(m: MPath) : scala.xml.Node = {
       utils.xml.readFile(MMTPathToFilePath(m))
    }
    def put(mod: Module) {
       val trg = MMTPathToFilePath(mod.path)
       val xml = mod.toNode
         //java.io.writeFile(trg, ... xml)
    }
    def toMar(target: java.io.File) {}
}

class TwelfArchive(root: File, report: Report, twelf : Twelf) extends Archive(root, report) {
    def srcToNarr(controller: Controller) {
       // create narration folder, iterate over all source files, call Twelf, put omdoc into narration folder (some subfolder structure)
       // build HashSet of all files
    }
}

object Test {
   def main(args: Array[String]) {
      val report = new frontend.FileReport(new java.io.File("archives.log"))
      report.groups += "archive"
      //val checker = new libraries.FoundChecker(libraries.DefaultFoundation)
      val controller = new Controller(libraries.NullChecker, report)
      val archive = new Archive(new java.io.File(""), report)
      archive.narrToCont(controller)
   }
}