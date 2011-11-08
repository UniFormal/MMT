package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import utils.File

trait Importer {
   /** true if this compiler can compile a certain kind of source files */
   def isApplicable(src : String): Boolean
   /** initialization (empty by default) */
   def init(report: frontend.Report, args: List[String]) {}
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
   /** source files that the compiler is able to process */
   def includeFile(n: String) : Boolean = true

   /** the compilation method
     * @param in the input file 
     * @param the folder in which to put the output file(s)
     */
   def compile(in: File, out: File) : List[CompilerError]

   /** registers an archive with this compiler */
   def register(arch: Archive) {}

}

/** an error or warning returned by the compiler */
case class CompilerError(region: Region, msg : List[String], warning: Boolean) {
   override def toString = region.toString + msg.mkString("\n","\n","\n") 
}

/** represents the location of an error */
case class Region(file: File, beginLine: Int, beginColumn: Int, endLine: Int, endColumn: Int) {
   override def toString = file.toString + "#" + beginLine + "." + beginColumn + "-" + endLine + "." + endColumn
}

trait QueryTransformer extends Importer {
   def transformSearchQuery(n: scala.xml.Node, params : List[String]) : List[scala.xml.Node]
}

object TrivialQueryTransformer extends QueryTransformer {
   def isApplicable(src : String) = true
   def transformSearchQuery(n: scala.xml.Node, params : List[String]) = List(n)
}