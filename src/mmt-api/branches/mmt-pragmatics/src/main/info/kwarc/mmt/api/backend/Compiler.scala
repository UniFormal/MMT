package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import utils.File

/**
 * A Compiler transforms source files into OMDoc files
 */
abstract class Compiler {
   /** true if this compiler can compile a certain kind of source files */
   def isApplicable(src : String): Boolean
   /** source files that the compiler is able to process */
   def includeFile(n: String) : Boolean = true
   /** the compilation method
     * @param in the input file 
     * @param the folder in which to put the output file(s)
     */
   def compile(in: File, out: File) : List[CompilerError]
   /** initialization (empty by default) */
   def init(args: List[String]) {}
   /** registers an archive with this compiler */
   def register(arch: Archive) {}
   /** termination (empty by default)
    * Compilers may create persistent data structures and processes,
    * but they must clean up after themselves in this method
    */
   def destroy {}
}

/** an error or warning returned by the compiler */
case class CompilerError(region: Region, msg : List[String], warning: Boolean) {
   override def toString = region.toString + msg.mkString("\n","\n","\n") 
}

/** represents the location of an error */
case class Region(file: File, beginLine: Int, beginColumn: Int, endLine: Int, endColumn: Int) {
   override def toString = file.toString + "#" + beginLine + "." + beginColumn + "-" + endLine + "." + endColumn
}
