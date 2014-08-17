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
abstract class Importer extends TraversingBuildTarget {
   protected var _inDim: ArchiveDimension = source
   /** source by default, may be overridden */
   def inDim = _inDim
   /** narration, produces also content and relational */ 
   val outDim = narration
   override val outExt = "omdoc"

   /** the main abstract method to be implemented by importers
    *  
    *  @param information about the input document and error reporting
    *  @param seCont a continuation function to be called on every generated document
    */
   def importDocument(bf: BuildTask, seCont: Document => Unit)

   def buildFile(a: Archive, bf: BuildTask) {
      importDocument(bf, doc => indexDocument(a, doc, bf.inPath))
   }
   
   override def buildDir(a: Archive, bd: BuildTask, builtChildren: List[BuildTask]) {
      bd.outFile.toJava.getParentFile().mkdirs()
      val doc = controller.get(DPath(a.narrationBase / bd.inPath)).asInstanceOf[Document]
      val inPathFile = Archive.narrationSegmentsAsFile(bd.inPath, "omdoc")
      writeToRel(doc, a/relational / inPathFile)
   }
    
    /** Write a module to content folder */
    private def writeToContent(a: Archive, mod: Module) {
       val contFile = a.MMTPathToContentPath(mod.path)
       log("[  -> content   ]     " + contFile.getPath)
       val w = new presentation.FileWriter(contFile)
       w("""<omdoc xmlns="http://omdoc.org/ns" xmlns:om="http://www.openmath.org/OpenMath">""")
       mod.toNode(w)
       w("</omdoc>")
       w.done
    }
    /** extract and write the relational information about a knowledge item */
    private def writeToRel(se: StructuralElement, file: File) {
       val relFile = file.setExtension("rel")
       log("[  -> relational]     " + relFile.getPath)
       val relFileHandle = File.Writer(relFile)
       ontology.Extractor(se) {
          r => relFileHandle.write(r.toPath + "\n")
       }
       relFileHandle.close
    }
    /** index a document */
    private def indexDocument(a: Archive, doc: Document, inPath: List[String]) {
        // write narration file
        val narrFile = outPath(a, inPath)
        log("[  -> narration ]     " + narrFile)
        val node = doc.toNode
        xml.writeFile(node, narrFile)
        // write relational file
        writeToRel(doc, a/relational / inPath)
        doc.getModulesResolved(controller.library) foreach {mod => {
           // write content file
           writeToContent(a, mod)
           // write relational file
           writeToRel(mod, a/relational / Archive.MMTPathToContentPath(mod.path))
        }}
   }
   /** deletes content, narration, and relational */
   override def cleanFile(a: Archive, curr: Current) {
       val controller = new Controller(report)
       val Current(inFile, narrPath) = curr
       val narrFile = outPath(a, narrPath)
       val doc = controller.read(narrFile, Some(DPath(a.narrationBase / narrPath)))(new ErrorLogger(report))
       //TODO if the same module occurs in multiple narrations, we have to use getLocalItems and write/parse the documents in narration accordingly 
       doc.getItems foreach {
          case r: documents.MRef =>
             val cPath = Archive.MMTPathToContentPath(r.target)
             delete(a/content / cPath)
             delete((a/relational / cPath).setExtension("rel"))
          case r: documents.DRef => //TODO recursively delete subdocuments
       }
       delete((a/relational / narrPath).setExtension("rel"))
       delete(narrFile)
    }
    override def cleanDir(a: Archive, curr: Current) {
       val inPathFile = Archive.narrationSegmentsAsFile(curr.path, "omdoc")
       delete((a/relational / inPathFile).setExtension("rel"))
    }
}

/** An StringBasedImporter is a more flexible importer whose input does not have to be a file */
abstract class StringBasedImporter extends Importer {
   /** the main abstract method to import a document given by its content */
   def importString(base: DPath, input: String, seCont: Document => Unit)

   def importDocument(bf: BuildTask, seCont: Document => Unit) {
      val input = utils.File.read(bf.inFile)
      importString(bf.narrationDPath, input, seCont)
   }
}

/** a trivial importer that reads OMDoc documents and returns them */
class OMDocImporter extends Importer {
   val key = "index"
   def includeFile(s: String) = s.endsWith(".omdoc")
   
   /**
    * 1 argument - the input dimension, defaults to source
    */
   override def start(args: List[String]) {
      args match {
         case Nil =>
         case a :: Nil => _inDim = Dim(a)
         case _ => throw LocalError("too many arguments") 
      }
   }
   
   def importDocument(bf: BuildTask, seCont: Document => Unit) = {
      val doc = controller.read(bf.inFile, Some(bf.narrationDPath))(bf.errorCont)
      seCont(doc)
   }
}