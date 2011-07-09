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
    private val files = new LinkedHashMap[File, List[CompilerError]]
    
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
    
    /** Generate narration from source
      * ********************** WARNING:  .svn folders are ignored ************* only .elf files are taken ********************** 
      */
    def sourceToNarr {
        compiler match {
            case None => throw CompilationError("no compiler defined")
            case Some(c) => 
                // make sure the source folder exists
                val srcdir = root / "source"
                if (!srcdir.exists) {                                  
                    log("source to narration: error: no source folder found")
                    return                                                                                      
                }
                // make sure the narration folder exists
                val narrdir = root / "narration"
                if (!narrdir.exists) {
                    val success : Boolean = narrdir.mkdir   // create /narration
                    if (success == false) {
                        log("source to narration: error: cannot create narration folder")
                        return
                    }
                }
                // add the source folder to the catalog
                c.asInstanceOf[Twelf].addCatalogLocation(srcdir)
                
                // compile the Twelf files in the source folder
                processFolder(root / "source", srcfile => {
                    // relative path of the destination file in the narration folder (with old suffix)
                    val rel : String = srcfile.toString.substring((root / "source").toString.length)
                    // replace suffix .elf with .omdoc
                    val relativePath : String = rel.substring(0, rel.length - "elf".length) + "omdoc"
                    // replace the first part of the path of srcfile with narrdir to get the destination file path
                    val destfile = narrdir / relativePath
                    val destfolder = destfile.getParentFile
                    // make sure the destination folder exists
                    if (!destfolder.exists) {
                        val success : Boolean = destfolder.mkdirs  // create the destination folder (and its parents, if necessary)
                        if (success == false) {
                            log("source to narration: error: cannot create a narration subfolder")
                            return
                        }
                    }
                    // compile the file
                    val errors : List[CompilerError] = c.compile(srcfile, destfile)
                    println("[TWELF] " + srcfile + " -> " + destfile)
                    // update 
                    files.update(destfile, errors)
                    if (!errors.isEmpty)
                        println(errors.mkString("[TWELF] ", "\n[TWELF] ", ""))
                })
        }
    }
    
    /** Apply a function on each file in the given folder or its subfolders. 
      * ********************** WARNING:  .svn folders are ignored ************* only .elf files are taken **********************
      * This only works for physical folders and files on a disk */
    private def processFolder(f: File, actOn: File => Unit) {
        if (!f.canRead) return
        else if (f.isFile) {
            if (f.getName.endsWith(".elf"))
                actOn(f)         // process the file
        }
        else if (f.isDirectory && f.getName != ".svn") {
            // Get list of children
            var fileList : Array[File] = null
            try {
                fileList = f.listFiles.map(x => File(x))
            } catch {
                case e : SecurityException => return
            }
            if (fileList == null) 
                return
            else
                // process each child
                fileList.foreach(child => processFolder(child, actOn))
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


object ArchiveTest {
    def main(args: Array[String]) {
    val controller = new Controller(NullChecker, new ConsoleReport)
    val twelf = new Twelf(File("c:/twelf-mod/bin/twelf-server.bat"))
    controller.backend.addCompiler(twelf)
    val archive = controller.backend.openArchive(File(args(0)))
    //twelf.addCatalogLocation(File("c:/Twelf/Unsorted/testproject/source"))
    //val errors = twelf.compile(File("c:/Twelf/Unsorted/testproject/source/test.elf"), File("c:/Twelf/Unsorted/testproject/source/test.omdoc"))
    //println(errors.mkString("\n"))
    archive.sourceToNarr
    controller.cleanup
    }
}