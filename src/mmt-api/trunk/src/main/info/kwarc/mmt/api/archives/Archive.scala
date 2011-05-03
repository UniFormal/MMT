package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._

class Archive(root: java.io.File) {
    def MMTPathToFilePath(m: MPath) : java.io.File = null
    def narrToCont(controller : Controller) {
        val omdocfile = new java.io.File("")
        // controller.delete(omdocfile)
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