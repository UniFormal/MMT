package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import documents._
import ontology._
import utils._

class GraphViz extends TraversingBuildTarget {
   val inDim = "narration"
   val key = "narration-svg"
   val outDim = "svg"
   def includeFile(name: String) : Boolean = name.endsWith(".omdoc")
   
   private var tg : ontology.TheoryGraph = null
   private var graphviz: String = null
   
   /** expects one argument: the path to graphviz */
   override def start(args: List[String]) {
      tg = new ontology.TheoryGraph(controller.depstore)
      args match {
         case hd::Nil =>
            graphviz = hd
         case _ => logError("missing/extraneous arguments; expected 1 argument (path to graphviz)")
      }
   }
   
   /** creates dot file and calls graphviz on it to produce svg file
    *
    *  the resulting theory graph contains all modules declared anywhere in this document  
    */
   def buildFile(a: Archive, bf: BuiltFile) = {
      val theories = controller.depstore.querySet(bf.dpath, Transitive(+Declares) * HasType(IsTheory))
      val views = controller.depstore.querySet(bf.dpath, Transitive(+Declares) * HasType(IsView))
      val dotFile = bf.outFile.setExtension("dot")
      val gv = new ontology.GraphExporter(theories, views, tg)
      gv.exportDot(dotFile)
      val command : List[String] = List("\"" + graphviz + "\"", "-Tsvg", "-o" + bf.outFile, dotFile.toString)
      log(command.mkString(" "))
      val proc = new java.lang.ProcessBuilder(command: _*).start() // use .inheritIO() for debugging
      proc.waitFor
      val ev = proc.exitValue
      if (ev != 0) {
         val scanner = new java.util.Scanner(proc.getErrorStream).useDelimiter("\\A")
         val message = if (scanner.hasNext) scanner.next else ""
         bf.errors ::= LocalError(message)
      }
   }
   
   /** same as buildOne but for the document given by the directory */
   override def buildDir(a: Archive, bd: BuiltDir, buildChildren: List[BuildResult]) = {
      val bf = new BuiltFile(bd.inFile, bd.inPath, DPath(a.narrationBase / bd.inPath), bd.outFile)
      buildFile(a,bf)
      bd.errors = bf.errors
   }
}