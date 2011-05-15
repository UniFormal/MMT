package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._

abstract class Dimension
case class Source(label: String)
case object Content
case object Narration
case class Presentation(label: String, style: MPath)
case object Relational

class Archive(root: java.io.File, report: Report) {
    private var base : Path = null
    private var dimensions : List[Dimension] = Nil
    def log(msg: => String) = report("archive", msg)
    def init {
       val conf = utils.xml.readFile(new java.io.File(root, "META-INF.xml"))
       conf match {
          case <archive>{dims}</archive> =>
             base = Path.parse(utils.xml.attr(conf, "base"), null)
             
       }
    }
    def MMTPathToFilePath(m: MPath) : java.io.File = null
    def narrToCont(controller : Controller) {
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