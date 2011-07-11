package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import libraries._
import frontend._
import modules._
import lf._
import utils._
import FileConversion._

import java.io.{FileInputStream, FileOutputStream}
import java.util.zip._

import scala.collection.mutable._

case class CompilationError(s: String) extends Exception(s)

/** MAR archive management
  * @param root the root folder that contains the source folder
  * @param properties 
  * @param compiler an already initialized compiler
  * @param report the reporting mechanism
  */
class Archive(val root: File, val properties: Map[String,String], compiler: Option[Compiler], report: Report) extends Storage {
    private val sourceBase = Path.parseD(properties.getOrElse("source-base", ""), utils.mmt.mmtbase)
    private val narrationBase = utils.URI(properties.getOrElse("narration-base", ""))
    
    /** set of files in the compiled folder, built in sourceToNarr */
    private val files = new LinkedHashMap[File, List[CompilerError]]
    
    /** map from module MPaths found in narrToCont to its file in the narration folder */
    private val modules = new LinkedHashMap[MPath, File]                              
    
    val narrationDir = root / "narration"
    val contentDir = root / "content"
    val sourceDir = root / "source"
    
    def includeDir(n: String) : Boolean = n != ".svn"

    /** Report a message using the given report handler */
    def log(msg: => String) = report("archive", msg)

    /** Get the disk path of the module in the content folder
      * @param m the MPath of the module 
      * @return the File descriptor of the destination  file in the content folder
      */
    def MMTPathToContentPath(m: MPath) : File =              // TODO: Use narrationBase instead of "NONE"?
       contentDir / m.parent.uri.authority.getOrElse("NONE") / m.parent.uri.path / (m.name + ".omdoc")
    def ContentPathToMMTPath(f: File) : MPath = null 
        
    /** compile source into "compiled" */
    def compile(in : List[String] = Nil) {
        compiler match {
            case None => throw CompilationError("no compiler defined")
            case Some(c) => 
              val inFile = root / "source" / in
              if (inFile.isDirectory) {
                 inFile.list foreach {n =>
                    if (includeDir(n)) compile(in ::: List(n))
                 }
              } else if (c.includeFile(inFile.getName)) {
                 try {
                    val outFile = (root / "compiled" / in).setExtension("omdoc")
                    log("[SRC->NARR] " + inFile + " -> " + outFile)
                    val errors = c.compile(inFile, outFile)
                    files(inFile) = errors
                    if (!errors.isEmpty)
                        log(errors.mkString("[SRC->NARR] ", "\n[SRC->NARR] ", ""))
                 } catch {
                    case e: Error => report(e)
                 }
              }
        }
    }
/*                // make sure the source folder exists
                if (!sourceDir.exists) {                                  
                    log("source to narration: error: no source folder found")
                    return                                                                                      
                }
                // make sure the narration folder exists
                if (!narrationDir.exists) {
                    val success : Boolean = narrationDir.mkdir   // create /narration
                    if (success == false) {
                        log("source to narration: error: cannot create narration folder")
                        return
                    }
                }
                // compile the files in the source folder
                processFolder(sourceDir, srcExtension, srcExclusions, srcFile => {
                    // relative path of the destination file in the narration folder (with old suffix)
                    val rel : String = srcFile.toString.substring(sourceDir.toString.length)
                    // replace suffix srcExtension with narrExtension
                    val relativePath : String = rel.substring(0, rel.length - srcExtension.length) + narrExtension
                    // replace the first part of the path of srcfile with narrationDir to get the destination file path
                    val destfile = narrationDir / relativePath
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
                    val errors : List[CompilerError] = c.compile(srcFile, destfile)
                    // update 
                    files.update(destfile, errors)
                    if (!errors.isEmpty)
                        log(errors.mkString("[SRC->NARR] ", "\n[SRC->NARR] ", ""))
                })
        }*/
    
    /** Apply a function on each file in the given folder or its subfolders. 
      * This only works for physical folders and files on a disk */
    private def processFolder(f: File, extension: String, exclusions: LinkedHashSet[String], actOn: File => Unit) {
        if (!f.canRead) return
        else if (f.isFile) {
            if (f.getName.endsWith(extension))
                actOn(f)         // process the file
        }
        else if (f.isDirectory && exclusions.forall(_ != f.getName)) {
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
                fileList.foreach(child => processFolder(child, extension, exclusions, actOn))
        }
    }
    
    /** Generate content and narration from . It uses the files map generated in srcToNarr */
    def produceNarrCont(in : List[String] = Nil) {
        val controller = new Controller(NullChecker, report)
        val inFile = root / "compiled" / in
        if (inFile.isDirectory) {
           inFile.list foreach {n =>
              if (includeDir(n)) produceNarrCont(in ::: List(n))
           }
        } else {
           try {
              val dpath = controller.read(inFile)
              val doc = controller.getDocument(dpath)
              doc.getModulesResolved(controller.library) foreach writeToContent
              //narration = write doc.toNode to file root / "narration" / in TODO
           } catch {
              case e: Error => report(e)
           }
           // controller.clear
        }
/*        // make sure the narration folder exists
        if (!narrationDir.exists) {                                  
            log("narration to content: error: no narration folder found")
            return                                                                                      
        }
        // make sure the content folder exists
        if (!contentDir.exists) {
            val success : Boolean = contentDir.mkdir   // create /content
            if (success == false) {
                log("narration to content: error: cannot create content folder")
                return
            }
        }
        // use a controller that accepts every file, i.e. doesn't check anything
        // iterate over all narration files, put one theory per file into content folder (subfolder structure according to namespaces)
        if (files.isEmpty)  // if the files index is empty, run over the narration folder
            processFolder(narrationDir, narrExtension, narrExclusions, narrFile => {
                log("[NARR->CONT] " + narrFile.getPath)
                var doc : DPath = null
                try {
                    doc = controller.read(narrFile)
                    controller.getDocument(doc).getModulesResolved(controller.library) foreach writeToContent
                } catch {
                    case e: Error => report(e)
                }
                controller.clear
            })
        else       // if the filex index exists, use it instead
            for (narrFile <- files.keySet) {
                log("[NARR->CONT] " + narrFile.getPath)
                var doc : DPath = null
                try {
                    doc = controller.read(narrFile)
                    controller.getDocument(doc).getModulesResolved(controller.library) foreach writeToContent
                } catch {
                    case e: Error => report(e)
                }
                controller.clear
            }
  */      
    }
    
    /** Generate relation from content */
    def produceRelational(in : List[String] = Nil, controller: Controller) {
        val lib = controller.library
        val inFile = root / "content" / in
        if (inFile.isDirectory) {
           inFile.list foreach {n =>
              if (includeDir(n)) produceRelational(in ::: List(n), controller)
           }
        } else {
           try {
              val mpath = ContentPathToMMTPath(inFile)
              lib.getModule(mpath)
              // open root / "relational" / in
              (controller.depstore.getInds ++ controller.depstore.getDeps) foreach {
                 case d : RelationalElement => if (d.path <= mpath) {} // write (d.toNode) to file 
              }
              // close
           } catch {
              case e: Error => report(e)
           }
        }       
    }
    
    /** Generate presentation from content */
    def producePresentation(controller: Controller, label: String, style: MPath) {}
    
    /** Get a module from content folder */ 
    def get(m: MPath) : scala.xml.Node = {
       utils.xml.readFile(MMTPathToContentPath(m))
    }
    
    /** Write a module to content folder */
    def writeToContent(mod: Module) {
       val destfile = MMTPathToContentPath(mod.path)
       log("[NARR->CONT]        -> " + destfile.getPath)
       val destfolder = destfile.getParentFile
       // make sure the destination folder exists
       if (!destfolder.exists) {
           val success : Boolean = destfolder.mkdirs  // create the destination folder (and its parents, if necessary)
           if (success == false) {
               log("narration to content: error: cannot create a content subfolder")
               return
           }
       }
       xml.writeFile(mod.toNode, destfile)
    }
    
    
    /** Add a file to a MAR file (only used internally by toMar)
      * @throws java.io.IOException */
    private def addFileToMar(f: File, base: File, out: ZipOutputStream, buffer: Array[Byte] = new Array(100000)) {
        var bytesRead = 0
        val in = new FileInputStream(f)
        out.putNextEntry(new ZipEntry(f.getPath.substring(base.getPath.length + 1)))
        var stop = false
        while (bytesRead != -1) {
            bytesRead = in.read(buffer)
            if (bytesRead != -1)
                out.write(buffer, 0, bytesRead)
        }   
        in.close
    }
      
    
    /** Add a folder to a MAR file (only used internally by toMar). Caution: empty folders are not put in the archive.
      * @throws java.io.IOException */
    private def addFolderToMar(f: File, base: File, out: ZipOutputStream, buffer: Array[Byte] = new Array(100000)) {
        val childList = f.listFiles
        // if the folder is empty, add a special entry for it
        //if (childList.isEmpty)
        //    out.putNextEntry(new ZipEntry(f.getPath.substring(base.getPath.length + 1)))
        for (child <- childList) {
            if (child.isDirectory)
                addFolderToMar(child, base, out, buffer)
            else
                addFileToMar(child, base, out, buffer)
        }
    }
    
    /** Pack everything in a MAR archive. Caution: empty folders are not put in the archive.
      * @param target the target MAR file. Default is <name>.mar in the root folder, where <name> is the name of the root */
    def toMar(target: java.io.File = root / (root.getName + ".mar")) {
        val out = new ZipOutputStream(new FileOutputStream(target))
        val buffer = new Array[Byte] (100000)   // 100KB buffer size
        try {
            for (component <- List("META-INF", "source", "narration", "content", "presentation", "relation"))
                if ((root/component).canRead)
                    addFolderToMar(root/component, root, out, buffer)
        } catch {
            case e: java.io.IOException => log("error when packing into a MAR file: " + (if (e.getCause == null) "" else e.getCause))
        }
        log("Created " + target.getPath)
        out.close
    }
    
    val narrationBackend = LocalCopy(narrationBase.schemeNull, narrationBase.authorityNull, narrationBase.pathAsString, narrationDir)
    def get(p: Path, reader: Reader) {p match {
       case doc : DPath => narrationBackend.get(doc, reader)  
       case mod : MPath => reader.readModules(utils.mmt.mmtbase, None, get(mod))
       case sym : GlobalName => get(sym.mod, reader)
    }}
}

/*
object ArchiveTest {
    def main(args: Array[String]) {
    val controller = new Controller(NullChecker, new ConsoleReport)
    controller.handle(ExecFile(File("test.mmt")))
    val archive = controller.backend.getArchive("latin").get
    //twelf.addCatalogLocation(File("c:/Twelf/Unsorted/testproject/source"))
    //val errors = twelf.compile(File("c:/Twelf/Unsorted/testproject/source/test.elf"), File("c:/Twelf/Unsorted/testproject/source/test.omdoc"))
    //println(errors.mkString("\n"))
    archive.sourceToNarr()
    archive.narrToCont()
    archive.toMar()
    controller.cleanup
    }
}*/