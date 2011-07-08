package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import libraries._
import frontend._
import modules._
import lf._
import utils._

import java.io.File
import scala.collection.mutable._


// source can miss, narration is always there

/** MAR archive management
  * @param root the root folder that contains the source folder
  * @param properties 
  * @param compiler an already initialized compiler
  * @param report the reporting mechanism
  */
class Archive(val root: File, val properties: Map[String,String], compiler: Option[Compiler], report: Report) extends Storage {
    /** the base URI of the documents in the narration folder, should be read from the meta-file and set in the init method */
    private var narrationBase : utils.URI = null
    
    /** the base path of the modules in the content folder, should be read from the meta-file and set in the init method */
    private var contentBase : DPath = null
    
    /** set of files in the narration folder, built in sourceToNarr */
    private val files = new LinkedHashSet[File]
    
    /** map from module MPaths found in narrToCont to its file in the narration folder */
    private val modules = new LinkedHashMap[MPath, File]
    
    /** Report a message using the given report handler */
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

    /** Get the disk path of the module in the content folder
      * @param m the MPath of the module 
      * @return the File descriptor of the destination .omdoc file
      */    
    def MMTPathToContentPath(m: MPath) : java.io.File = new File(root.getPath + File.separator + "content" + File.separator + m.parent.uri + File.separator + name + ".omdoc") 
    
    /** Generate narration from source */
    def sourceToNarr {
        compiler match {
            case None => throw CompilationError("no compiler defined")
            case Some(c) => {
                
                //c.check(sourceFile, narrDir)
            }
        }
    }
    
    /** Generate content from narration */
    def narrToCont {    // TODO: build index of all URIs
        // use a controller that accepts every file, i.e. doesn't check anything
        val controller = new Controller(NullChecker, report)
        
        // create content folder, iterate over all narration files using "files", put one theory per file into content folder (subfolder structure according to namespaces)
        
        val omdocfile = new File("")
        val doc = controller.read(omdocfile)
        controller.getDocument(doc).getModulesResolved(controller.library) foreach put
        controller.clear
    }
    
    /** Generate relation from content */
    def contToRel(controller: Controller) {}
    
    /** Generate presentation from content */
    def contToPres(controller: Controller, label: String, style: MPath) {}
    
    /** Get a module from content folder */ 
    def get(m: MPath) : scala.xml.Node = {
       utils.xml.readFile(MMTPathToContentPath(m))
    }
    
    /** Write a module to content folder */
    def put(mod: Module) {
       xml.writeFile(mod.toNode, MMTPathToContentPath(mod.path))
    }
    
    /** Pack everything in a MAR archive */
    def toMar(target: java.io.File) {}
    val narrationBackend = LocalCopy(narrationBase.schemeNull, narrationBase.authorityNull, narrationBase.pathAsString, root)
    def get(p: Path, reader: Reader) {p match {
       case doc : DPath => narrationBackend.get(doc, reader)  
       case mod : MPath => reader.readModules(contentBase, None, get(mod))
       case sym : GlobalName => get(sym.mod, reader)
    }}
}