package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import frontend._
import utils.File

trait Importer {
   protected var report : Report = null
   /** true if this compiler can compile a certain kind of source files */
   def isApplicable(src : String): Boolean
   /** initialization (empty by default) */
   def init(report: Report, args: List[String]) {
      this.report = report
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
   /** source file names that the compiler is able to process */
   def includeFile(n: String) : Boolean = true

   /** the compilation method
     * @param in the input file 
     * @param out the output file without extension
     */
   def compile(in: File, out: File) : List[CompilerError]

   /** registers an archive with this compiler */
   def register(arch: Archive, dim: String) {}

}

/** an error or warning returned by the compiler */
case class CompilerError(region: parser.SourceRegion, msg : List[String], warning: Boolean) {
   def getMessage = msg.mkString("\n","\n","\n") 
   override def toString = region.toString + getMessage
}

trait QueryTransformer extends Importer {
   def transformSearchQuery(n: scala.xml.Node, params : List[String]) : List[scala.xml.Node]
}

object TrivialQueryTransformer extends QueryTransformer {
   def isApplicable(src : String) = true
   def transformSearchQuery(n: scala.xml.Node, params : List[String]) = List(n)
}