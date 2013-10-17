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
   def buildOne(inFile: File, dpath: DPath, outFile: File): List[Error] = {
      val theories = controller.depstore.querySet(dpath, Transitive(+Declares) * HasType(IsTheory))
      val views = controller.depstore.querySet(dpath, Transitive(+Declares) * HasType(IsView))
      val dotFile = outFile.setExtension("dot")
      val gv = new ontology.GraphExporter(theories, views, tg)
      gv.exportDot(dotFile)
      val command : List[String] = List("\"" + graphviz + "\"", "-Tsvg", "-o" + outFile, dotFile.toString)
      log(command.mkString(" "))
      val proc = new java.lang.ProcessBuilder(command: _*).start() // use .inheritIO() for debugging
      proc.waitFor
      val ev = proc.exitValue
      if (ev != 0) {
         val scanner = new java.util.Scanner(proc.getErrorStream).useDelimiter("\\A")
         val message = if (scanner.hasNext) scanner.next else ""
         List(LocalError(message))
      } else
         Nil
   }
   
   /** same as buildOne but for the document given by the directory */
   override def buildDir(a: Archive, inDir: File, inPath: List[String], buildChildren: List[BuildResult], outFile: File): List[Error] = {
      buildOne(inDir, DPath(a.narrationBase / inPath), outFile)
   }
}