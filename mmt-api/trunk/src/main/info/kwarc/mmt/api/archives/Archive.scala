package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._

class Archive(root: java.io.File, report: Report) {
    def log(msg: => String) = report("archive", msg)
    def MMTPathToFilePath(m: MPath) : java.io.File = null
    def narrToCont(controller : Controller) {
        val omdocfile = new java.io.File("")
        // controller.delete(omdocfile) or controller.clear
        val doc = controller.read(omdocfile)
        controller.getDocument(doc).getModulesResolved(controller.library) foreach put
    }
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