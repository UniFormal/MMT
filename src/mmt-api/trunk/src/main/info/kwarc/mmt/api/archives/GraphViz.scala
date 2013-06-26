package info.kwarc.mmt.api.archives
import info.kwarc.mmt.api._
import utils._

class GraphViz extends TraversingBuildTarget {
   val inDim = "narration"
   val key = "narration-svg"
   val outDim = "svg"
   def includeFile(name: String) : Boolean = name.endsWith(".omdoc")
   
   private var tg : ontology.TheoryGraph = null
   private var graphviz: String = null
   
   /** expects one argument: the path to graphviz */
   override def init(controller: frontend.Controller, args: List[String]) {
      super.init(controller,args)
      tg = new ontology.TheoryGraph(controller.depstore)
      args match {
         case hd::Nil =>
            graphviz = hd
         case _ => logError("missing/extraneous arguments; expected 1 argument (path to graphviz)")
      }
   }
   
   def buildOne(inFile: File, dpathOpt: Option[DPath], outFile: File): List[Error] = {
      val (doc,_) = controller.read(inFile, dpathOpt)
      val mods = doc.getItems.flatMap {
         case r: documents.MRef => List(r.target)
         case _ => Nil
      }
      val dotFile = outFile.setExtension("dot")
      tg.exportDot(dotFile)(p => mods.contains(p))
      val command : List[String] = List("\"" + graphviz + "\"", "-Tsvg", "-o" + outFile, dotFile.toString)
      log(command.mkString(" "))
      val proc = new java.lang.ProcessBuilder(command: _*).start() // use .inheritIO() for debugging
      proc.waitFor
      val ev = proc.exitValue
      if (ev != 0) {
         val scanner = new java.util.Scanner(proc.getErrorStream).useDelimiter("\\A")
         val message = if (scanner.hasNext) scanner.next else ""
         val error = OtherError("graphviz error: " + message)
         List(error)
      } else
         Nil
   }
}