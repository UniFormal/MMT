package info.kwarc.mmt.mathscheme
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.backend._
import utils.File
import utils.FileConversion._

import java.io._
import scala.collection.mutable.HashSet

/** Utility for starting the catalog and calling the Twelf compiler
  */
class MathScheme extends Compiler {
   def isApplicable(src: String) = src == "mathscheme"
   
   override def includeFile(n: String) : Boolean = n.endsWith(".msl")
   
   var path : File = null

   private def log(msg : => String) {report("twelf", msg)}
   
   /** 
    * creates and intializes a Catalog
    * first argument is the location of the twelf-server script
    */
   override def init(rep: frontend.Report, args: List[String]) {
      super.init(rep, Nil)
      path = File(args(0))
   }

   /** 
     * Compile a MathScheme file to OMDoc
     * @param in the input Twelf file 
     * @param out the file in which to put the generated OMDoc
     */
   def compile(in: File, out: File) : List[SourceError] = {
      File(out.getParent).mkdirs
      val procBuilder = new java.lang.ProcessBuilder(path.toString, in.toString)
      //procBuilder.redirectErrorStream()
      val proc = procBuilder.start()
      //val input = new PrintWriter(proc.getOutputStream(), true)
      val output = new BufferedReader(new InputStreamReader(proc.getInputStream()))
      val outFile = File.Writer(out)
      var line : String = null
      while ({line = output.readLine; line != null}) {
         outFile.write(line + "\n")
      }
      Nil
   }
}

/*
object TwelfTest {
   def main(args: Array[String]) {
      val twelf = new Twelf(File("c:\\twelf-mod\\bin\\twelf-server.bat"))
      twelf.init
      twelf.addCatalogLocation(File("c:/Twelf/Unsorted/testproject/source"))
      //twelf.check(File("e:\\other\\twelf-mod\\examples-mod\\test.elf"), File(".")) 
      val errors = twelf.compile(File("c:/Twelf/Unsorted/testproject/source/test.elf"), File("c:/Twelf/Unsorted/testproject/source/test.omdoc"))
      println(errors.mkString("\n"))
      twelf.destroy
   }
}
*/