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
   
   def buildFile(a: Archive, bf: BuiltFile) = {
      try {
        val doc = controller.getDocument(bf.dpath)
        _rh = new presentation.FileWriter(bf.outFile)
        doDocument(doc)
        rh.done
      } catch {
        case e : Error => bf.errors ::= e
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
   def doTheory(t: DeclaredTheory, bf: BuiltFile)
   /** applied to each view */
   def doView(v: DeclaredView, bf: BuiltFile)
   /** applied to every namespace
    *  @param dpath the namespace
    *  @param namespaces the sub-namespace in this namespace
    *  @param modules the modules in this namespace
    */
   def doNamespace(dpath: DPath, namespaces: List[(BuiltDir,DPath)], modules: List[(BuiltFile,MPath)])
   
   val inDim = "content"
   def includeFile(name: String) = name.endsWith(".omdoc")
   
   override def buildDir(a: Archive, bd: BuiltDir, builtChildren: List[BuildResult]) = {
      val dp = Archive.ContentPathToDPath(bd.inPath)
      val nss = builtChildren flatMap {
         case d: BuiltDir if ! d.skipped => List((d, Archive.ContentPathToDPath(d.inPath)))
         case _ => Nil
      }
      val mps = builtChildren flatMap {
         case f: BuiltFile if ! f.skipped => List((f, Archive.ContentPathToMMTPath(f.inPath)))
         case _ => Nil
      }
      _rh = new presentation.FileWriter(bd.outFile)
      doNamespace(dp, nss, mps)
      _rh.done
   }
   def buildFile(a: Archive, bf: BuiltFile) = {
      val mp = Archive.ContentPathToMMTPath(bf.inPath)
      val mod = controller.globalLookup.getModule(mp)
      _rh = new presentation.FileWriter(bf.outFile)
      mod match {
         case t: DeclaredTheory => doTheory(t, bf)
         case v: DeclaredView => doView(v, bf)
         case _ =>
      }
      _rh.done
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
   protected def covered(m: MPath): Boolean = {
      objects.TheoryExp.metas(objects.OMMOD(m))(controller.globalLookup) contains meta
   }
   def doTheory(t: DeclaredTheory, bf: BuiltFile) {
      if (covered(t.path))
         doCoveredTheory(t)
      else
         bf.skipped = true
   }
   def doView(v: DeclaredView, bf: BuiltFile) {
      if (covered(v.from.toMPath)) {
         val to = v.to.toMPath
         if (to == found)
            doRealization(v) //TODO check if v includes certain fixed morphism
         else if (covered(to))
            doFunctor(v)
         else
            bf.skipped = true
      } else
         bf.skipped = true
   }
   
   /** called on covered theories, i.e., theories with meta-theory meta */
   def doCoveredTheory(t: DeclaredTheory)
   /** called on views between covered theories */
   def doFunctor(v: DeclaredView)
   /** called on realizations, i.e., views from a covered theory to found, e.g., models or implementations */
   def doRealization(r: DeclaredView)

}