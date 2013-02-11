package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import archives._
import frontend._
import utils.File

trait Importer {
   protected var controller : Controller = null
   protected var report : Report = null
   /** true iff this Importer can handle input of type 'src' */
   def isApplicable(src: String): Boolean
   /** initialization (empty by default) */
   def init(controller: Controller, args: List[String]) {
      this.controller = controller
      report = controller.report
   }
   /** termination (empty by default)
    * Importers may create persistent data structures and processes,
    * but they must clean up after themselves in this method
    */
   def destroy {}
}

/**
 * A Compiler transforms source files into OMDoc files
 */
trait Compiler extends Importer {
   /** source file names that the compiler is able to process
    * 
    * by default, true iff the Compiler is applicable to the file extension
    */
   def includeFile(n: String) : Boolean = {
      val i = n.lastIndexOf(".")
      if (i == -1) false
      else {
         val s = n.substring(i+1)
         isApplicable(s)
      }
   }

   /** the compilation method
     * @param in the input file 
     * @param out the output file without extension
     */
   def compile(in: File, out: File) : List[SourceError]

   /** registers an archive with this compiler */
   def register(arch: Archive, dim: String) {}
   /** unregisters an archive with this compiler */
   def unregister(arch: Archive, dim: String) {}
}

trait QueryTransformer extends Importer {
   def transformSearchQuery(n: scala.xml.Node, params : List[String]) : List[scala.xml.Node]
}

object TrivialQueryTransformer extends QueryTransformer {
   def isApplicable(src : String) = true
   def transformSearchQuery(n: scala.xml.Node, params : List[String]) = List(n)
}