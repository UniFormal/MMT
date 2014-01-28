package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import modules._
import presentation._
import utils._
import documents._
import frontend._

trait Exporter extends BuildTarget { self => 
   /** must be set correctly before any of the abstract methods are called */
   protected var _rh: RenderingHandler = null
   /** @return the RenderingHandler to which all produced output must be sent */ 
   protected def rh = _rh 
   
   val outDim: Dim
   
   protected val folderName = ""
   
   override def init(controller: Controller) {
     this.controller = controller
     report = controller.report
     contentExporter.init(controller)
     narrationExporter.init(controller)
   }  
     
   /** the file extension used for generated files, defaults to outDim, override as needed */
   def outExt: String = outDim match {
      case Dim(path@_*) => path.last
      case d => d.toString
   }
   
   /** applied to each leaf document (i.e., .omdoc file) */
   def exportDocument(doc : Document, bf: BuildTask)
   
   /** applied to each theory */
   def exportTheory(thy : DeclaredTheory, bf: BuildFile)
   /** applied to each view */
   def exportView(view : DeclaredView, bf: BuildFile)
   /** applied to every namespace
    *  @param dpath the namespace
    *  @param namespaces the sub-namespace in this namespace
    *  @param modules the modules in this namespace
    */
   def exportNamespace(dpath: DPath, bd: BuildDir, namespaces: List[(BuildDir,DPath)], modules: List[(BuildFile,MPath)])
  
   
   def build (a: Archive, args: List[String], in: List[String]) {
     contentExporter.build(a, args, in)
     narrationExporter.build(a, args, in)
   }
   
   def update(a: Archive, args: List[String], in: List[String]) {
     contentExporter.build(a, args, in)
     narrationExporter.build(a, args, in)
   }
   
   def clean (a: Archive, args: List[String], in: List[String]) {
     contentExporter.build(a, args, in)
     narrationExporter.build(a, args, in)
   }
   
   override def register(arch : Archive) {
     contentExporter.register(arch)
     narrationExporter.register(arch)
   }
   
   /** closes the FileWriter and removes the file if it is empty */
   private def done(rh: RenderingHandler) {
      rh.done
      rh match {
         case fw: presentation.FileWriter => 
            if (fw.filename.toJava.length == 0)
               fw.filename.toJava.delete
         case _ =>
      }
   }
   
   /** A BuildTarget that traverses the content dimension and applies continuation functions to each module.
    *
    *  Deriving this class is well-suited for writing exporters that transform MMT content into other formats.
    */
   private lazy val contentExporter = new TraversingBuildTarget {
      val inDim = content
      val outDim = self.outDim / "content"
      
      override val outExt = self.outExt
      
      override val folderName = self.folderName
      
      def key = self.key + "_content"
      def buildFile(a: Archive, bf: BuildFile) {
        val mp = Archive.ContentPathToMMTPath(bf.inPath)
        val mod = controller.globalLookup.getModule(mp)
        _rh = new presentation.FileWriter(bf.outFile)
        mod match {
          case t: DeclaredTheory =>
            exportTheory(t, bf)
          case v: DeclaredView =>
            exportView(v, bf)
          case _ =>
        }
        done(_rh)
      }
      
      override def buildDir(a: Archive, bd: BuildDir, builtChildren: List[BuildTask]) = {
        val dp = Archive.ContentPathToDPath(bd.inPath)
        val nss = builtChildren flatMap {
          case d: BuildDir if ! d.skipped => List((d, Archive.ContentPathToDPath(d.inPath)))
          case _ => Nil
        }
        val mps = builtChildren flatMap {
          case f: BuildFile if ! f.skipped => List((f, Archive.ContentPathToMMTPath(f.inPath)))
          case _ => Nil
        }
        _rh = new presentation.FileWriter(bd.outFile)
        exportNamespace(dp, bd, nss, mps)
        done(_rh)
      }
      
      def includeFile(name: String) = self.includeFile(name)
   }
   
   /** A BuildTarget that traverses the content dimension and applies continuation functions to each module.
    *
    *  Deriving this class is well-suited for writing exporters that transform MMT content into other formats.
    */
   private lazy val narrationExporter = new TraversingBuildTarget {
      val inDim = narration
      val outDim = self.outDim / "narration"
      
      override  val outExt = self.outExt
      override val folderName = self.folderName
      
      def key = self.key + "_narration"

      def buildFile(a: Archive, bf: BuildFile) = {
        val doc = controller.getDocument(bf.dpath)
        _rh = new presentation.FileWriter(bf.outFile)
        exportDocument(doc, bf)
        done(_rh)
      }
      def includeFile(name : String) = self.includeFile(name)
   }
   
   def includeFile(name: String) = name.endsWith(".omdoc")
}

trait IndentedExporter extends Exporter {
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

/** An Exporter that exports relative to a bifoundation
 *  @param meta the syntactic meta-theory, e.g., the logic or specification language
 *  @param found the semantic domain, e.g., the foundation or programming language
 */
abstract class FoundedExporter(meta: MPath, found: MPath) extends Exporter {
   protected def covered(m: MPath): Boolean = {
      objects.TheoryExp.metas(objects.OMMOD(m))(controller.globalLookup) contains meta
   }
   def exportTheory(t: DeclaredTheory, bf: BuildFile) {
      if (covered(t.path))
         exportCoveredTheory(t)
      else
         bf.skipped = true
   }
   def exportView(v: DeclaredView, bf: BuildFile) {
      if (covered(v.from.toMPath)) {
         val to = v.to.toMPath
         if (to == found)
            exportRealization(v) //TODO check if v includes certain fixed morphism
         else if (covered(to))
            exportFunctor(v)
         else
            bf.skipped = true
      } else
         bf.skipped = true
   }
   
   /** called on covered theories, i.e., theories with meta-theory meta */
   def exportCoveredTheory(t: DeclaredTheory)
   /** called on views between covered theories */
   def exportFunctor(v: DeclaredView)
   /** called on realizations, i.e., views from a covered theory to found, e.g., models or implementations */
   def exportRealization(r: DeclaredView)

}