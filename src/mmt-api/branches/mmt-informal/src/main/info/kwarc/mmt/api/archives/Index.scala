package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import frontend._
import backend._
import modules._
import objects._
import ontology._
import utils._
import utils.FileConversion._

/** the build target for indexing an Archive */
class Index extends BuildTarget {
    val inDim = "compiled"
    val key = "index"

    /** Write a module to content folder */
    private def writeToContent(a: Archive, mod: Module) {
       val contFile = a.MMTPathToContentPath(mod.path)
       log("[  -> CONT]     " + contFile.getPath)
       val omdocNode = <omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath">{mod.toNode}</omdoc>
       xml.writeFile(omdocNode, contFile)
    }
    private def writeToRel(a: Archive, mod: Module) {
       val relFile = (a.relDir / Archive.MMTPathToContentPath(mod.path)).setExtension("rel")
       log("[  -> REL ]     " + relFile.getPath)
       relFile.getParentFile.mkdirs
       val relFileHandle = File.Writer(relFile)
       ontology.Extractor(mod) {
          r => relFileHandle.write(r.toPath + "\n")
       }
       relFileHandle.close
    }
    /* obsolete
       used to write notation file using fresh iterator on all notations declared within one of the theories of this document
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
    */

    /** Generate content, narration, notation, and relational from compiled. */
   def build(a: Archive, args: List[String], in: List[String] = Nil) {
        val indexTimestamps = a.timestamps("index")
        a.traverse(a.compiledDim, in, Archive.extensionIs("omdoc")) {case Current(inFile, inPath) =>
           val narrFile = a.narrationDir / inPath
           log("[COMP ->  ]  " + inFile)
           log("[  -> NARR]     " + narrFile)
           val controller = new Controller(report)
           val (doc,_) = controller.read(inFile, Some(DPath(a.narrationBase / inPath)))
           // write narration file
           xml.writeFile(doc.toNode, narrFile)
           doc.getModulesResolved(controller.library) foreach {mod => {
              // write content file
              writeToContent(a, mod)
              // write relational file
              writeToRel(a, mod)
           }}
           indexTimestamps.set(inPath)
        }
    }
    
    
    /** deletes content, narration, notation, and relational; argument is treated as paths in narration */
    def clean (a: Archive, args: List[String], in: List[String] = Nil) {
       val controller = new Controller(report)
       a.traverse("narration", in, Archive.extensionIs("omdoc")) {case Current(inFile, inPath) =>
          val (doc,_) = controller.read(inFile, Some(DPath(a.narrationBase / inPath)))
          //TODO if the same module occurs in multiple narrations, we have to use getLocalItems and write/parse the documents in narration accordingly 
          doc.getItems foreach {
             case r: documents.MRef =>
                val cPath = Archive.MMTPathToContentPath(r.target)
                delete(a.contentDir / cPath)
                delete((a.relDir / cPath).setExtension("rel"))
             case r: documents.DRef => //TODO recursively delete subdocuments
          }
          delete(inFile)
       }
    }
    /** partially reruns produceNarrCont using the time stamps and the system's last-modified information */  
    def update(a: Archive, args: List[String], in: List[String] = Nil) {
       val indexTimestamps = a.timestamps("index")
       a.traverse(a.compiledDim, in, _ => true) {case Current(_, inPath) =>
          indexTimestamps.modified(inPath) match {
             case Deleted =>
                clean(a, args, inPath)
             case Added =>
                build(a, args, inPath)
             case Modified =>
                clean(a, args, inPath)
                build(a, args, inPath)
             case Unmodified => //nothing to do
          }
       }
    }
}