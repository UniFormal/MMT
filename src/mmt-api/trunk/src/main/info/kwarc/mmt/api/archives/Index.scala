package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import frontend._
import backend._
import documents._
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
    private def writeToRel(se: StructuralElement, file: File) {
       val relFile = file.setExtension("rel")
       log("[  -> REL ]     " + relFile.getPath)
       val relFileHandle = File.Writer(relFile)
       ontology.Extractor(se) {
          r => relFileHandle.write(r.toPath + "\n")
       }
       relFileHandle.close
    }
    
    private def writeDoc(a: Archive, doc: Document, inPath: List[String]) {
        // write narration file
        val narrFile = a.narrationDir / inPath
        log("[  -> NARR]     " + narrFile)
        xml.writeFile(doc.toNode, narrFile)
        // write relational file
        writeToRel(doc, a.relDir / inPath)
    }

    /** Generate content, narration, and relational from compiled */
   def build(a: Archive, args: List[String], in: List[String] = Nil) {
        val indexTimestamps = a.timestamps("index")
        a.traverse[Unit](a.compiledDim, in, Archive.extensionIs("omdoc")) ({case Current(inFile, inPath) =>
           log("[COMP ->  ]  " + inFile)
           val (doc,_) = controller.read(inFile, Some(DPath(a.narrationBase / inPath)))
           writeDoc(a, doc, inPath)
           doc.getModulesResolved(controller.library) foreach {mod => {
              // write content file
              writeToContent(a, mod)
              // write relational file
              writeToRel(mod, a.relDir / Archive.MMTPathToContentPath(mod.path))
           }}
           indexTimestamps.set(inPath)
        }, {case (Current(_, inPath), _) => buildDir(a, inPath)
        })
    }
    
   private def buildDir(a: Archive, inPath: List[String]) {
        val doc = controller.get(DPath(a.narrationBase / inPath)).asInstanceOf[Document]
        val inPathFile = Archive.narrationSegmentsAsFile(inPath, "omdoc")
        writeToRel(doc, a.relDir / inPathFile)
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
       a.traverse[Boolean](a.compiledDim, in, _ => true) ({case Current(_, inPath) =>
          //we return true if a file was added or deleted so that the directory can be rebuilt
          indexTimestamps.modified(inPath) match {
             case Deleted =>
                clean(a, args, inPath)
                true
             case Added =>
                build(a, args, inPath)
                true
             case Modified =>
                clean(a, args, inPath)
                build(a, args, inPath)
                false
             case Unmodified => //nothing to do
                false
          }
       }, {case (Current(_, inPath), childChanged) =>
          if (childChanged.exists(_ == true))
             buildDir(a, inPath)
          false
       })
    }
}