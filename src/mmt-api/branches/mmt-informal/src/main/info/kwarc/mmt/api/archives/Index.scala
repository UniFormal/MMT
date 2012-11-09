package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import frontend._
import backend._
import modules._
import objects._
import ontology._
import utils._
import utils.FileConversion._

/** This trait adds indexing operations to Archive's */
trait IndexedArchive extends WritableArchive {
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
       ontology.Extractor(mod) {
          r => relFileHandle.write(r.toPath + "\n")
       }
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
    // stores for each file the time of the last call of produceNarrCont
    private val narrContTimestamps = new Timestamps(root / compiledDim, root / "META-INF" / "timestamps" / compiledDim)
    /** Generate content, narration, notation, and relational from compiled. */
    def produceNarrCont(in : List[String] = Nil) {
        traverse(compiledDim, in, extensionIs("omdoc")) {case Current(inFile, inPath) =>
           val narrFile = narrationDir / inPath
           log("[COMP ->  ]  " + inFile)
           log("[  -> NARR]     " + narrFile)
           val controller = new Controller(report)
           val dpath = controller.read(inFile, Some(DPath(narrationBase / inPath)))
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
           narrContTimestamps.set(inPath)
        }
    }
    /** deletes content, narration, notation, and relational; argument is treated as paths in narration */
    def deleteNarrCont(in:List[String] = Nil) {
       val controller = new Controller(report)
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
    /** partially reruns produceNarrCont using the time stamps and the system's last-modified information */  
    def updateNarrCont(in: List[String] = Nil) {
       traverse(compiledDim, in, _ => true) {case Current(_, inPath) =>
          narrContTimestamps.modified(inPath) match {
             case Deleted =>
                deleteNarrCont(inPath)
             case Added =>
                produceNarrCont(inPath)
             case Modified =>
                deleteNarrCont(inPath)
                produceNarrCont(inPath)
             case Unmodified => //nothing to do
          }
       }
    }
    def readRelational(in: List[String] = Nil, controller: Controller, kd: String) {
       if ((root / "relational").exists) {
          traverse("relational", in, extensionIs(kd)) {case Current(inFile, inPath) =>
             ontology.RelationalElementReader.read(inFile, DPath(narrationBase), controller.depstore)
          }
          //TODO this should only add implicits for the dependencies it read
          controller.depstore.getDeps foreach {
             case Relation(Includes, to: MPath, from: MPath) => controller.library.addImplicit(OMMOD(from), OMMOD(to), OMIDENT(OMMOD(to)))
             case Relation(HasMeta, thy: MPath, meta: MPath) => controller.library.addImplicit(OMMOD(meta), OMMOD(thy), OMIDENT(OMMOD(thy)))
             case _ => 
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
}