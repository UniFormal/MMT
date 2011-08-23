package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import libraries._
import frontend._
import modules._
import symbols._
import patterns._
import objects._
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
    val flatDir = root / "flat"
    
    def includeDir(n: String) : Boolean = n != ".svn"

    /** Report a message using the given report handler */
    def log(msg: => String) = report("archive", msg)

    /** Get the disk path of the module in the content folder
      * @param m the MPath of the module 
      * @return the File descriptor of the destination  file in the content folder
      */
    def MMTPathToContentPath(m: MPath) : File = contentDir / Archive.MMTPathToContentPath(m)
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
                    log("[SRC->COMP] " + inFile + " -> " + outFile)
                    val errors = c.compile(inFile, outFile)
                    files(inFile) = errors
                    if (!errors.isEmpty)
                        log(errors.mkString("[SRC->COMP] ", "\n[SRC->COMP] ", ""))
                 } catch {
                    case e: Error => report(e)
                    case e => report("error", e.getMessage)
                 }
              }
        }
    }

    /** Generate content and narration from compiled. It uses the files map generated in compile */
    def produceNarrCont(in : List[String] = Nil) {
        val controller = new Controller(NullChecker, report)
        val inFile = root / "compiled" / in
        if (inFile.isDirectory) {
           inFile.list foreach {n =>
              if (includeDir(n)) produceNarrCont(in ::: List(n))
           }
        } else if (inFile.getExtension == Some("omdoc")) {
           try {
              val dpath = controller.read(inFile)
              val doc = controller.getDocument(dpath)
              val narrFile = narrationDir / in
              log("[COMPILED->CONT+NARR] " + inFile)
              log("[COMPILED->NARR]        -> " + narrFile)
              // write narration file
              narrFile.getParentFile.mkdirs
              xml.writeFile(doc.toNode, narrFile)
              // write content files
              doc.getModulesResolved(controller.library) foreach writeToContent
           } catch {
              case e: Error => report(e)
              case e => report("error", e.getMessage)
           }
        }
    }
    
    def produceFlat(in: List[String] = Nil, controller: Controller) {
       val inFile = contentDir / in
       if (inFile.isDirectory) {
           inFile.list foreach {n =>
              if (includeDir(n)) produceFlat(in ::: List(n), controller)
           }
        } else {
           val mpath = Archive.ContentPathToMMTPath(in)
           val mod = controller.globalLookup.getModule(mpath)
           val flatNode = mod match {
              case thy: DeclaredTheory =>
                 Instance.elaborate(thy)(controller.globalLookup)
                 thy.toNodeElab
              case _ => mod.toNode
           }
           xml.writeFile(flatNode, flatDir / in)
        }
    }
    
    /** Generate relation from content */
    def produceRelational(in : List[String] = Nil, controller: Controller) {
        val inFile = contentDir / in
        if (inFile.isDirectory) {
           inFile.list foreach {n =>
              if (includeDir(n)) produceRelational(in ::: List(n), controller)
           }
        } else {
           try {
              val mpath = Archive.ContentPathToMMTPath(in)
              controller.get(mpath)
              val outFile = (root / "relational" / in).setExtension("rel")
              log("[CONT->REL] " + inFile + " -> " + outFile)
              outFile.getParentFile.mkdirs
              val outFileHandle = new java.io.BufferedWriter(new java.io.OutputStreamWriter(new java.io.FileOutputStream(outFile),"UTF-8"))
              (controller.depstore.getInds ++ controller.depstore.getDeps) foreach {
                 case d : RelationalElement => if (d.path <= mpath) outFileHandle.write(d.toNode.toString + "\n")
              }
              outFileHandle.close
           } catch {
              case e: Error => throw e
              //case e => report("error", e.getMessage)
           }
        }       
    }
    def produceMWS(in : List[String] = Nil, dim: String) {
        val inFile = root / dim / in
        val mwsbase = properties("mws-base")
        if (inFile.isDirectory) {
           inFile.list foreach {n =>
              if (includeDir(n)) produceMWS(in ::: List(n), dim)
           }
        } else {
           val controller = new Controller(NullChecker, report)
           val mpath = Archive.ContentPathToMMTPath(in)
           controller.get(mpath)
           controller.library.getTheory(mpath) match {
              case thy : DeclaredTheory =>
                 val outFile = root / "mws" / dim / in
                 val outStream = null //new ... (outFile)
                 def writeEntry(t: Term, url: String) {
                    //outStream.writeln("..." + t.toCML.toString)
                 }
                 //outStream.writeln("..")
                 thy.valueList foreach {
                    case c: Constant =>
                       List(c.tp,c.df).map(tO => tO map { 
                          t => writeEntry(t, mwsbase + "/" + c.name.flat)
                       })
                    case _ =>
                 }
                 //outStream.writeln(...)
                 //outStream.close
              case t: DefinedTheory =>
           }
        }
    }
    
    
    /** Generate presentation from content */
    def producePresentation(controller: Controller, label: String, style: MPath) {}
    
    /** Write a module to content folder */
    def writeToContent(mod: Module) {
       val destfile = MMTPathToContentPath(mod.path)
       log("[COMP->CONT]        -> " + destfile.getPath)
       destfile.getParentFile.mkdirs // make sure the destination folder exists
       val omdocNode = <omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath"> { mod.toNode } </omdoc>
       xml.writeFile(omdocNode, destfile)
    }
    
    
    /** Add a file to a MAR file (only used internally by toMar)
      * @throws java.io.IOException */
    private def addFileToMar(f: File, base: File, out: ZipOutputStream, buffer: Array[Byte]) {
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
    private def addFolderToMar(f: File, base: File, out: ZipOutputStream, buffer: Array[Byte]) {
        // if the folder is empty, add a special entry for it
        //if (childList.isEmpty)
        //    out.putNextEntry(new ZipEntry(f.getPath.substring(base.getPath.length + 1)))
        f.listFiles foreach {child =>
            if (child.isDirectory) {
                if (includeDir(child.getName)) addFolderToMar(child, base, out, buffer)
            } else
                addFileToMar(child, base, out, buffer)
        }
    }
    
    /** Pack everything in a MAR archive. Caution: empty folders are not put in the archive.
      * @param target the target MAR file. Default is <name>.mar in the root folder, where <name> is the name of the root */
    def toMar(target: java.io.File = root / (root.getName + ".mar")) {
        log("building archive at " + target.getPath)
        val out = new ZipOutputStream(new FileOutputStream(target))
        val buffer = new Array[Byte] (100000)   // 100KB buffer size
        try {
            List("META-INF", "source", "narration", "content", "presentation", "relational") foreach {dim =>
                if ((root/dim).canRead)
                    addFolderToMar(root/dim, root, out, buffer)
            }
        } catch {
            case e: java.io.IOException => log("error when packing into a MAR file: " + (if (e.getCause == null) "" else e.getCause))
        }
        log("done")
        out.close
    }
    
    val narrationBackend = LocalCopy(narrationBase.schemeNull, narrationBase.authorityNull, narrationBase.pathAsString, narrationDir)
    /** Get a module from content folder */ 
    def get(m: MPath) : scala.xml.Node = {
       utils.xml.readFile(MMTPathToContentPath(m)).child(0)
    }
    def get(p: Path, reader: Reader) {p match {
       case doc : DPath => narrationBackend.get(doc, reader)  
       case mod : MPath => reader.readModules(utils.mmt.mmtbase, None, get(mod))
       case sym : GlobalName => get(sym.mod, reader)
    }}
}

object Archive {
   /** a string containing all characters that are illegal in file names */ 
   val illegalChars = "'"
   // TODO: (un)escape illegal characters
   def escape(s: String) : String = s
   def unescape(s: String) : String = s
    // scheme..authority / seg / ments / name.omdoc ----> scheme :// authority / seg / ments ? name
   def ContentPathToMMTPath(segs: List[String]) : MPath = segs match {
       case Nil => throw ImplementationError("")
       case hd :: tl =>
          val p = hd.indexOf("..")
          val tllast = tl.last
          val name = unescape(tllast.substring(0, tllast.length - 6))
          DPath(URI(hd.substring(0,p), hd.substring(p+2)) / tl.init) ? name
    }
    /** Get the disk path of the module in the content folder
      * @param m the MPath of the module 
      * @return the File descriptor of the destination  file in the content folder
      */
    def MMTPathToContentPath(m: MPath) : List[String] = {            // TODO: Use narrationBase instead of "NONE"?
       val uri = m.parent.uri
       val schemeString = uri.scheme.map(_ + "..").getOrElse("")
       (schemeString + uri.authority.getOrElse("NONE")) :: uri.path ::: List(escape(m.name.flat) + ".omdoc")
    }

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