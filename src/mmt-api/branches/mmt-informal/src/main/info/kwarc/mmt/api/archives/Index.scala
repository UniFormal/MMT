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

/**
 * a build target for importing an archive in some source syntax
 * 
 * This should only be needed when OMDoc is received from a third party.
 * OMDoc produced by [[Compiler]]s is indexed automatically.
 *  
 */
abstract class Compiler extends TraversingBuildTarget {
   val inDim = "source"
   val outDim = "narration"
   override val outExt = "omdoc"

   def buildOne(bf: BuiltFile) : Document

   def buildFile(a: Archive, bf: BuiltFile) {
      val doc = buildOne(bf)
      indexDocument(a, doc, bf.inPath)
   }
   
   override def buildDir(a: Archive, bd: BuiltDir, builtChildren: List[BuildResult]) {
      val doc = controller.get(DPath(a.narrationBase / bd.inPath)).asInstanceOf[Document]
      val inPathFile = Archive.narrationSegmentsAsFile(bd.inPath, "omdoc")
      writeToRel(doc, a.relDir / inPathFile)
   }
    
    /** Write a module to content folder */
    private def writeToContent(a: Archive, mod: Module) {
       val contFile = a.MMTPathToContentPath(mod.path)
       log("[  -> CONT]     " + contFile.getPath)
       val omdocNode = <omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath">{mod.toNode}</omdoc>
       xml.writeFile(omdocNode, contFile)
    }
    /** extract and write the relational information about a knowledge item */
    private def writeToRel(se: StructuralElement, file: File) {
       val relFile = file.setExtension("rel")
       log("[  -> REL ]     " + relFile.getPath)
       val relFileHandle = File.Writer(relFile)
       ontology.Extractor(se) {
          r => relFileHandle.write(r.toPath + "\n")
       }
       relFileHandle.close
    }
    /** index a document */
    private def indexDocument(a: Archive, doc: Document, inPath: List[String]) {
        // write narration file
        val narrFile = (a.narrationDir / inPath).setExtension("omdoc")
        log("[  -> NARR]     " + narrFile)
        xml.writeFile(doc.toNode, narrFile)
        // write relational file
        writeToRel(doc, a.relDir / inPath)
        doc.getModulesResolved(controller.library) foreach {mod => {
           // write content file
           writeToContent(a, mod)
           // write relational file
           writeToRel(mod, a.relDir / Archive.MMTPathToContentPath(mod.path))
        }}
   }    
   /** deletes content, narration, notation, and relational */
   override def cleanFile(a: Archive, curr: Current) {
       val controller = new Controller(report)
       val Current(inFile, inPath) = curr
       val (doc,_) = controller.read(inFile, Some(DPath(a.narrationBase / inPath)))
       //TODO if the same module occurs in multiple narrations, we have to use getLocalItems and write/parse the documents in narration accordingly 
       doc.getItems foreach {
          case r: documents.MRef =>
             val cPath = Archive.MMTPathToContentPath(r.target)
             delete(a.contentDir / cPath)
             delete((a.relDir / cPath).setExtension("rel"))
          case r: documents.DRef => //TODO recursively delete subdocuments
       }
       delete((a.relDir / inPath).setExtension("rel"))
       delete(inFile)
    }
    override def cleanDir(a: Archive, curr: Current) {
       val inPathFile = Archive.narrationSegmentsAsFile(curr.path, "omdoc")
       delete((a.relDir / inPathFile).setExtension("rel"))
    }
}

/** a trivial importer that reads OMDoc documents and returns them */
class OMDocImporter extends Compiler {
   val key = "index"
   def includeFile(s: String) = s.endsWith(".omdoc")
   
   def buildOne(bf: BuiltFile) = {
      log("[COMP ->  ]  " + bf.inFile)
      val (doc,_) = controller.read(bf.inFile, Some(bf.dpath))
      doc
   }
}