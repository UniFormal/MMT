package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import utils.File

/**
 * A Compiler transforms source files into OMDoc files
 */
abstract class Compiler {
   /** true if this compiler can compile a certain kind of source files */
   def isApplicable(src : String): Boolean
   /** the compilation method */
   def compile(in: File, out: File) : List[CompilerError]
   /** initialization (empty by default) */
   def init {}
   /** termination (empty by default)
    * Compilers may create persistent data structures and processes,
    * but they must clean up after themselves in this method
    */
   def destroy {}
}

/** an error or warning returned by the compiler */
case class CompilerError(region: Region, msg : List[String], warning: Boolean)

/** represents the location of an error */
case class Region(file: File, beginLine: Int, beginColumn: Int, endLine: Int, endColumn: Int)
