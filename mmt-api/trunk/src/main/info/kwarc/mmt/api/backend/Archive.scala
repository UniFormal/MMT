package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import libraries._
import frontend._
import modules._
import lf._
import utils._
import FileConversion._

import scala.collection.mutable._


case class CompilationError(s: String) extends Exception(s)


// source can miss, narration is always there

/** MAR archive management
  * @param root the root folder that contains the source folder
  * @param properties 
  * @param compiler an already initialized compiler
  * @param report the reporting mechanism
  */
class Archive(val root: File, val properties: Map[String,String], compiler: Option[Compiler], report: Report) extends Storage {
    private val sourceBase = Path.parseD(properties.getOrElse("source-base", ""), utils.mmt.mmtbase)
    private val narrationBase = utils.URI(properties.getOrElse("narration-base", ""))
    
    /** set of files in the narration folder, built in sourceToNarr */
    private val files = new LinkedHashSet[File]
    
    /** map from module MPaths found in narrToCont to its file in the narration folder */
    private val modules = new LinkedHashMap[MPath, File]
    
    /** Report a message using the given report handler */
    def log(msg: => String) = report("archive", msg)

    /** Get the disk path of the module in the content folder
      * @param m the MPath of the module 
      * @return the File descriptor of the destination .omdoc file
      */    
    def MMTPathToContentPath(m: MPath) : java.io.File =
       root / "content" / m.parent.uri.authority.getOrElse("NONE") / m.parent.uri.path / (m.name + ".omdoc") 
    
    /** Generate narration from source */
    def sourceToNarr {
        compiler match {
            case None => throw CompilationError("no compiler defined")
            case Some(c) => {
                
                ////c.compile(sourceFile, narrDir / (name + ".omdoc"))
            }
        }
    }
    
    /** Generate content from narration */
    def narrToCont {    // TODO: build index of all URIs
        // use a controller that accepts every file, i.e. doesn't check anything
        val controller = new Controller(NullChecker, report)
        
        // create content folder, iterate over all narration files using "files", put one theory per file into content folder (subfolder structure according to namespaces)
        
        val omdocfile = File("")
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
    val narrationBackend = LocalCopy(narrationBase.schemeNull, narrationBase.authorityNull, narrationBase.pathAsString, root / "narration")
    def get(p: Path, reader: Reader) {p match {
       case doc : DPath => narrationBackend.get(doc, reader)  
       case mod : MPath => reader.readModules(utils.mmt.mmtbase, None, get(mod))
       case sym : GlobalName => get(sym.mod, reader)
    }}
}