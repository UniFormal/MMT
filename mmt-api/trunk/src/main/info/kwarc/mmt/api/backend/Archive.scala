package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import libraries._
import frontend._
import modules._
import symbols._
import patterns._
import objects._
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
    val id = properties("id")
    val sourceBase = Path.parseD(properties.getOrElse("source-base", ""), utils.mmt.mmtbase)
    val narrationBase = utils.URI(properties.getOrElse("narration-base", ""))
   
    private val custom : ArchiveCustomization = {
       properties.get("customization") match {
          case None => new DefaultCustomization
          case Some(c) => java.lang.Class.forName(c).asInstanceOf[java.lang.Class[ArchiveCustomization]].newInstance
       }
    }
    
    /** set of files in the compiled folder, built in sourceToNarr */
    private val files = new LinkedHashMap[File, List[CompilerError]]
    
    /** map from module MPaths found in narrToCont to its file in the narration folder */
    private val modules = new LinkedHashMap[MPath, File]                              
    
    val narrationDir = root / "narration"
    val contentDir = root / "content"
    val sourceDir = root / "source"
    val relDir = root / "relational"
    val compiledDir = root / "compiled"
    val flatDir = root / "flat"
    
    def includeDir(n: String) : Boolean = n != ".svn"

    /** Report a message using the given report handler */
    def log(msg: => String) = report("archive", msg)

    /** Get the disk path of the module in the content folder
      * @param m the MPath of the module 
      * @return the File descriptor of the destination  file in the content folder
      */
    def MMTPathToContentPath(m: MPath) : File = contentDir / Archive.MMTPathToContentPath(m)

    /** holds information about the current file during a traversal */
    case class Current(file: File, path: List[String])
    /** returns a functions that filters by file name extension */
    def extensionIs(e: String) : String => Boolean = _.endsWith("." + e)  
    /** traverses a dimension; it seems reasonable to reimplement all methods in terms of (a method similar to) this */
    def traverse(dim: String, in: List[String], filter: String => Boolean)(f: Current => Unit) {
        val inFile = root / dim / in
        if (inFile.isDirectory) {
           log("entering " + inFile)
           inFile.list foreach {n =>
              if (includeDir(n)) traverse(dim, in ::: List(n), filter)(f)
           }
           log("leaving  " + inFile)
        } else {
           try {
              if (filter(inFile.getName)) f(Current(inFile, in))
           } catch {
              case e : Error => report(e)
              case e => throw e
           }
        }
    }
    
    /** compile source into "compiled" */
    def compile(in : List[String] = Nil) {
       compiler match {
          case None => throw CompilationError("no compiler defined")
          case Some(c) => 
             traverse("source", in, c.includeFile) {case Current(inFile, in) =>
                 val outFile = (compiledDir / in).setExtension("omdoc")
                 log("[SRC -> COMP] " + inFile + " -> " + outFile)
                 val errors = c.compile(inFile, outFile)
                 files(inFile) = errors
                 if (!errors.isEmpty)
                     log(errors.mkString("[SRC -> COMP] ", "\n[SRC -> COMP] ", ""))
             }
        }
    }

    /** Write a module to content folder */
    private def writeToContent(mod: Module) {
       val contFile = MMTPathToContentPath(mod.path)
       log("[  -> CONT]     " + contFile.getPath)
       val omdocNode = <omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath">{mod.toNode}</omdoc>
       xml.writeFile(omdocNode, contFile)
    }
    private def writeToRel(mod: Module) {
       val relFile = (relDir / Archive.MMTPathToContentPath(mod.path)).setExtension("rel")
       log("[  -> REL ]     " + relFile.getPath)
       relFile.getParentFile.mkdirs
       val relFileHandle = File.Writer(relFile)
       ontology.Extract(mod, r => relFileHandle.write(r.toPath + "\n")) 
       relFileHandle.close
    }
    private def writeToNot(mod: Module, nots : Iterator[presentation.Notation]) {
       mod match {
          case t: DeclaredTheory =>
             val tpath = t.path
             val notFile = (root / "notation" / Archive.MMTPathToContentPath(tpath)).setExtension("not")
             log("[  -> NOT ]     " + notFile.getPath)
             notFile.getParentFile.mkdirs
             val notFileHandle = File.Writer(notFile)
             nots foreach {n =>
                if (n.nset == tpath) notFileHandle.write(n.toString + "\n") 
             }
             notFileHandle.close
          case _ => // nothing to do
       }
    }
    
    /** Generate content, narration, notation, and relational from compiled. */
    def produceNarrCont(in : List[String] = Nil) {
        traverse("compiled", in, extensionIs("omdoc")) {case Current(inFile, in) =>
           val narrFile = narrationDir / in
           log("[COMP ->  ]  " + inFile)
           log("[  -> NARR]     " + narrFile)
           val controller = new Controller(NullChecker, report)
           val dpath = controller.read(inFile, Some(DPath(narrationBase / in)))
           val doc = controller.getDocument(dpath)
           // write narration file
           xml.writeFile(doc.toNode, narrFile)
           doc.getModulesResolved(controller.library) foreach {mod => {
              // write content file
              writeToContent(mod)
              // write relational file
              writeToRel(mod)
              // write notation file using fresh iterator on all notations declared within one of the theories of this document
              writeToNot(mod, controller.notstore.getDefaults)
           }}
        }
    }
    /** deletes content, narration, notation, and relational; argument is are treated as paths in compiled */
    def deleteNarrCont(in:List[String] = Nil) {
       def deleteFile(f: File) {
          log("deleting " + f)
          f.delete
       }
       val controller = new Controller(NullChecker, report)
       traverse("narration", in, extensionIs("omdoc")) {case Current(inFile, inPath) =>
          val dpath = controller.read(inFile, Some(DPath(narrationBase / inPath)))
          val doc = controller.getDocument(dpath)
          //TODO if the same module occurs in multiple narrations, we have to use getLocalItems and write/parse the documents in narration accordingly 
          doc.getItems foreach {
             case r: documents.MRef =>
                val cPath = Archive.MMTPathToContentPath(r.target)
                deleteFile(root / "content" / cPath)
                deleteFile((root / "notation" / cPath).setExtension("not"))
                deleteFile((root / "relational" / cPath).setExtension("rel"))
             case r: documents.DRef => //TODO recursively delete subdocuments
          }
          deleteFile(inFile)
       }
    }
        
    /** Extract scala from a dimension */
    def extractScala(in : List[String] = Nil, dim: String) {
        val inFile = root / dim / in
        if (inFile.isDirectory) {
           inFile.list foreach {n =>
              if (includeDir(n)) extractScala(in ::: List(n), dim)
           }
        } else if (inFile.getExtension == Some("omdoc")) {
           try {
              val controller = new Controller(NullChecker, report)
              val dpath = controller.read(inFile, Some(DPath(narrationBase / in)))
              val outFile = (root / "scala" / in).setExtension("scala")
              outFile.getParentFile.mkdirs
              info.kwarc.mmt.uom.Extractor.doDocument(controller, dpath, outFile)
           } catch {
              case e: Error => report(e)
              //case e => report("error", e.getMessage)
           }
        }
    }
    
    /** Integrate scala into a dimension */
    def integrateScala(in : List[String] = Nil, dim: String) {
        val inFile = root / "scala" / in
        if (inFile.isDirectory) {
           inFile.list foreach {n =>
              if (includeDir(n)) integrateScala(in ::: List(n), dim)
           }
        } else if (inFile.getExtension == Some("omdoc")) {
           try {
              val controller = new Controller(NullChecker, report)
              val dpath = controller.read(inFile, Some(DPath(narrationBase / in)))
              val scalaFile = (root / "scala" / in).setExtension("scala")
              info.kwarc.mmt.uom.Synthesizer.doDocument(controller, dpath, scalaFile)
              val doc = controller.getDocument(dpath)
              xml.writeFile(doc.toNodeResolved(controller.library), inFile)
           } catch {
              case e: Error => report(e)
              //case e => report("error", e.getMessage)
           }
        }
    }
    
    def readRelational(in: List[String] = Nil, controller: Controller) {
       if ((root / "relational").exists) {
          traverse("relational", in, extensionIs("rel")) {case Current(inFile, _) =>
             ontology.RelationalElementReader.read(inFile, sourceBase, controller.depstore)
          }
       }
    }
    def readNotation(in: List[String] = Nil, controller: Controller) {
       if ((root / "notation").exists) {
          traverse("notation", in, extensionIs("not")) {case Current(inFile, inPath) =>
             val thy = Archive.ContentPathToMMTPath(inPath)
             File.ReadLineWise(inFile) {line => controller.notstore.add(presentation.Notation.parseString(line, thy))}
          }
       }
    }

    def produceFlat(in: List[String], controller: Controller) {
       val inFile = contentDir / in
       log("to do: [CONT -> FLAT]        -> " + inFile)
       if (inFile.isDirectory) {
           inFile.list foreach {n =>
              if (includeDir(n)) produceFlat(in ::: List(n), controller)
           }
       } else if (inFile.getExtension == Some("omdoc")) {
              val mpath = Archive.ContentPathToMMTPath(in)
              val mod = controller.globalLookup.getModule(mpath)
              val flatNode = mod match {
                 case thy: DeclaredTheory =>
                    Instance.elaborate(thy)(controller.globalLookup, report)
                    thy.toNodeElab
                 case _ => mod.toNode
              }
              val flatNodeOMDoc = <omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath">{flatNode}</omdoc>
              xml.writeFile(flatNodeOMDoc, flatDir / in)
              controller.delete(mpath)
        }
       log("done:  [CONT -> FLAT]        -> " + inFile)
    }
    
    def produceMWS(in : List[String] = Nil, dim: String) {
        val sourceDim = dim match {
          case "mws-flat" => "flat"
          case _ => "content"
        }
        traverse(sourceDim, in, extensionIs("omdoc")) {case Current(inFile, inPath) =>
           val outFile = (root / "mws" / dim / inPath).setExtension("mws")
           log("[  -> MWS]  " + inFile + " -> " + outFile)
           val controller = new Controller(NullChecker, NullReport)
           controller.read(inFile,None)
           val mpath = Archive.ContentPathToMMTPath(in)
           val mod = controller.localLookup.getModule(mpath)
           mod match {
              case thy : DeclaredTheory =>
                 outFile.toJava.getParentFile().mkdirs()
                 val outStream = new java.io.FileWriter(outFile)
                 def writeEntry(t: Term, url: String) {
                   val node = <mws:expr url={url}>{t.toCML}</mws:expr> 
                   outStream.write(node.toString + "\n")
                 }
                 outStream.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
                 outStream.write("<mws:harvest xmlns:mws=\"http://search.mathweb.org/ns\" xmlns:m=\"http://www.w3.org/1998/Math/MathML\">\n")
                 thy.valueList foreach {
                    case c: Constant =>
                       List(c.tp,c.df).map(tO => tO map { 
                          t =>
                            val url = custom.mwsurl(c.path) 
                            val cml = custom.prepareQuery(t)
                            val node = <mws:expr url={url}>{cml}</mws:expr> 
                            outStream.write(node.toString + "\n")
                            //writeEntry(t, mwsbase + "/" + c.name.flat)
                       })
                    /*case i : Instance => {
                      val url = mwsbase + "/" + thy.name + ".html" + "#" + i.name.flat
                      val node = <mws:expr url={url}>{i.matches.toCML}</mws:expr>
                      outStream.write(node.toString + "\n")
                    }*/
                    case _ =>
                 }
                 outStream.write("</mws:harvest>\n")
                 outStream.close
              case _ => //TODO index other modules
           }
        }
    }
    
    
    /** Generate presentation from content */
    def producePresentation(controller: Controller, label: String, style: MPath) {}
        
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
    def get(m: MPath) : scala.xml.Node = utils.xml.readFile(MMTPathToContentPath(m)).child(0)
    def get(p: Path, reader: Reader) {p match {
       case doc : DPath => narrationBackend.get(doc, reader)  
       case mod : MPath =>
          val node = try { get(mod) }
          catch {case e: java.io.FileNotFoundException => throw NotApplicable}
          reader.readModules(mod.doc, None, get(mod))
       case sym : GlobalName => get(sym.mod, reader)
    }}
}

object Archive {
   /** a string containing all characters that are illegal in file names */ 
   val illegalChars = "'"
   // TODO: (un)escape illegal characters
   def escape(s: String) : String = s.replace("'","(apos)")
   def unescape(s: String) : String = s.replace("(apos)", "'")
    // scheme..authority / seg / ments / name.omdoc ----> scheme :// authority / seg / ments ? name
   def ContentPathToMMTPath(segs: List[String]) : MPath = segs match {
       case Nil => throw ImplementationError("")
       case hd :: tl =>
          val p = hd.indexOf("..")
          val fileNameNoExt = tl.length match {
            case 0 => throw ImplementationError("")
            case _ => tl.last.lastIndexOf(".") match {
               case -1 => tl.last
               case i => tl.last.substring(0,i)
            }
          }
          DPath(URI(hd.substring(0,p), hd.substring(p+2)) / tl.init) ? unescape(fileNameNoExt)
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