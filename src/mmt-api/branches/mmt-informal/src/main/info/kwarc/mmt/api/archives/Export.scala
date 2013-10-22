package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import modules._
import presentation._
import utils._
import documents._

/** A BuildTarget that traverses the content dimension and applies continuation functions to each module.
 *
 *  Deriving this class is well-suited for writing exporters that transform MMT content into other formats.
 */
abstract class NarrationExporter extends GenericTraversingBuildTarget {

   /** must be set correctly before any of the abstract methods are called */
   private var _rh: RenderingHandler = null
   /** @return the RenderingHandler to which all produced output must be sent */ 
   protected def rh = _rh 
   
   /** applied to each theory */
   def doDocument(doc: Document)

   val inDim = "narration"
   def includeFile(name: String) = name.endsWith(".omdoc")
   
   def buildFile(a: Archive, inFile: File, inPath: List[String], outFile: File): List[Error] = {
      try {
        val dp = DPath(a.narrationBase / inPath)
        val doc = controller.getDocument(dp)
        _rh = new presentation.FileWriter(outFile)
        doDocument(doc)
        rh.done
        Nil
      } catch {
        case e : Error => List(e)
      }
   }
}


/** A BuildTarget that traverses the content dimension and applies continuation functions to each module.
 *
 *  Deriving this class is well-suited for writing exporters that transform MMT content into other formats.
 */
abstract class ContentExporter extends GenericTraversingBuildTarget {

   /** must be set correctly before any of the abstract methods are called */
   private var _rh: RenderingHandler = null
   /** @return the RenderingHandler to which all produced output must be sent */ 
   protected def rh = _rh 
   
   /** applied to each theory */
   def doTheory(t: DeclaredTheory)
   /** applied to each view */
   def doView(v: DeclaredView)
   /** applied to every namespace
    *  @param dpath the namespace
    *  @param namespaces the sub-namespace in this namespace
    *  @param modules the modules in this namespace
    *  @param the intended output file
    */
   def doNamespace(dpath: DPath, namespaces: List[(BuiltDir,DPath)], modules: List[(BuiltFile,MPath)])
   
   val inDim = "content"
   def includeFile(name: String) = name.endsWith(".omdoc")
   
   override def buildDir(a: Archive, inDir: File, inPath: List[String],
                         builtChildren: List[BuildResult], outFile: File): List[Error] = {
      val dp = Archive.ContentPathToDPath(inPath)
      val nss = builtChildren flatMap {
         case bd: BuiltDir => List((bd, Archive.ContentPathToDPath(bd.inPath)))
         case _ => Nil
      }
      val mps = builtChildren flatMap {
         case bf: BuiltFile => List((bf, Archive.ContentPathToMMTPath(bf.inPath)))
         case _ => Nil
      }
      _rh = new presentation.FileWriter(outFile)
      doNamespace(dp, nss, mps)
      _rh.done
      Nil
   }
   def buildFile(a: Archive, inFile: File, inPath: List[String], outFile: File): List[Error] = {
      val mp = Archive.ContentPathToMMTPath(inPath)
      val mod = controller.globalLookup.getModule(mp)
      _rh = new presentation.FileWriter(outFile)
      mod match {
         case t: DeclaredTheory => doTheory(t)
         case v: DeclaredView => doView(v)
         case _ =>
      }
      _rh.done
      Nil
   }
}

trait IndentedExporter extends ContentExporter {
   private var indentLevel = 0
   val indentString = "  "
   protected var afterIndentationString = ""
   def indent(body: => Unit) {
      indentLevel += 1
      nl
      try {body}
      finally {indentLevel -= 1}
   }
   def nl {
      rh("\n")
      Range(0,indentLevel).foreach {_ =>
         rh(indentString)
      }
   }
}

/** A ContentExporter that exports relative to a bifoundation
 *  @param meta the syntactic meta-theory, e.g., the logic or specification language
 *  @param found the semantic domain, e.g., the foundation or programming language
 */
abstract class FoundedExporter(meta: MPath, found: MPath) extends ContentExporter {
   def doTheory(t: DeclaredTheory, outFile: File) {
      if (t.meta == Some(meta))
         doCoveredTheory(t, outFile)
   }
   def doView(v: DeclaredView, outFile: File) {
      
   }
   
   /** called on covered theories, i.e., theories with meta-theory meta */
   def doCoveredTheory(t: DeclaredTheory, outFile: File)
   /** called on views between covered theories */
   def doCoveredView(v: DeclaredView, outFile: File)
   /** called on realizations, i.e., views from a covered theory to found, e.g., models or implementations */
   def doRealization(r: DeclaredView, outFile: File)

}