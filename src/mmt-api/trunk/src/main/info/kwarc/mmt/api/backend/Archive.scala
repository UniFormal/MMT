package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import frontend._
import modules._
import lf._

import java.io.File
import scala.collection.mutable._

class Archive(val root: File, val properties: Map[String,String], compiler: Option[Compiler], report: Report) extends Storage {
    // the base URI of the documents in the narration folder, should be read from the meta-file and set in the init method
    private var narrationBase : utils.URI = null
    // the base path of the modules in the content folder, should be read from the meta-file and set in the init method
    private var contentBase : DPath = null
    private val files = new LinkedHashSet[File]
    private val modules = new HashMap[MPath,File]
    def log(msg: => String) = report("archive", msg)
    def init {}
/*       val conf = utils.xml.readFile(new java.io.File(root, "META-INF.xml"))
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
    val narrationBackend = LocalCopy(narrationBase.schemeNull, narrationBase.authorityNull, narrationBase.pathAsString, root)
    def get(p: Path, reader: Reader) {p match {
       case doc : DPath => narrationBackend.get(doc, reader)  
       case mod : MPath => reader.readModules(contentBase, None, get(mod))
       case sym : GlobalName => get(sym.mod, reader)
    }}
}