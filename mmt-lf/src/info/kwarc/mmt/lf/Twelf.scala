package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import utils.File
import utils.FileConversion._

import java.io._
import scala.collection.mutable.HashSet

/** helper methods for Twelf */
object Twelf {
   /** parses filename:col.line-col.line */
   def parseRegion(s: String) = {
      val i = s.lastIndexOf(":")
      val file = File(s.substring(0,i))
      val numbers = s.substring(i+1).split("[-\\.]")
      Region(file, numbers(0).toInt, numbers(1).toInt, numbers(2).toInt, numbers(3).toInt)
   }
}

/** Utility for starting the catalog and calling the Twelf compiler
  */
class Twelf extends Compiler {
   def isApplicable(src: String) = src == "twelf"
   
   override def includeFile(n: String) : Boolean = n.endsWith(".elf")
   
   var path : File = null
   /** Twelf setting "set unsafe ..." */
   var unsafe : Boolean = true
   /** Twelf setting "set chatter ..." */
   var chatter : Int = 5
   var catalogOpt : Option[Catalog] = None
   var port = 8083
   private def log(msg : => String) {report("twelf", msg)}
   
   /** 
    * creates and intializes a Catalog
    * first argument is the location of the twelf-server script
    */
   override def init(rep: frontend.Report, args: List[String]) {
      super.init(rep, Nil)
      path = File(args(0))
      val cat = new Catalog(HashSet(), HashSet("*.elf"), HashSet(".svn"), port, true, report("lfcatalog", _))
      cat.init    //  throws PortUnavailable
      catalogOpt = Some(cat)
   }
   override def register(arch: Archive, dim: String) {
      addCatalogLocation(arch.root / dim)
   }
   override def destroy {
      catalogOpt.foreach(_.destroy)
   }
   
   /** add a location to the catalog */
   def addCatalogLocation(f: File) {
       catalogOpt.foreach(_.addStringLocation(f.getPath))
   }

   /** 
     * Compile a Twelf file to OMDoc
     * @param in the input Twelf file 
     * @param out the file in which to put the generated OMDoc
     */
   def compile(in: File, out: File) : List[CompilerError] = {
      File(out.getParent).mkdirs
      val procBuilder = new java.lang.ProcessBuilder(path.toString)
      procBuilder.redirectErrorStream()
      val proc = procBuilder.start()
      val input = new PrintWriter(proc.getOutputStream(), true)
      val output = new BufferedReader(new InputStreamReader(proc.getInputStream()))
      input.println("set chatter " + chatter)
      input.println("set unsafe " + unsafe)
      catalogOpt foreach {cat =>
         input.println("set catalog " + cat.queryURI)
      }
      input.println("loadFile " + in)
      input.println("Print.OMDoc.printDoc " + in + " " + out.setExtension("omdoc"))
      input.println("OS.exit")
      var line : String = null
      var errors : List[CompilerError] = Nil
      while ({line = output.readLine; line != null}) {
         line = line.trim
         val (treat, dropChars, warning) =
            if (line.endsWith("Warning:")) (true, 9, true)
            else if (line.endsWith("Error:")) (true, 7, false)
            else (false, 0, false)
         if (treat) {
            val r = Twelf.parseRegion(line.substring(0, line.length - dropChars))
            var msg : List[String] = Nil
            do {
               msg ::= output.readLine
            } while (! msg.head.startsWith("%%"))
            errors ::= CompilerError(r, msg.reverse, warning)
         }
      }
      errors.reverse
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