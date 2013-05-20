package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import archives._
import backend._
import utils.File
import utils.FileConversion._

import info.kwarc.mmt.twelf.Catalog

import java.io._
import scala.collection.mutable.HashSet

/** helper methods for Twelf */
object Twelf {
   /** parses filename:col.line-col.line */
   def parseRef(s: String) = {
      val i = s.lastIndexOf(":")
      val file = File(s.substring(0,i))
      val numbers = s.substring(i+1).split("[-\\.]")
      val reg = parser.SourceRegion(parser.SourcePosition(-1, numbers(0).toInt - 1, numbers(1).toInt - 1),
                                    parser.SourcePosition(-1, numbers(2).toInt - 1, numbers(3).toInt - 1))
      parser.SourceRef(utils.FileURI(file), reg)
                          
   }
}

/** Utility for starting the catalog and calling the Twelf compiler
  */
class Twelf extends Compiler {
   val key = "twelf-omdoc"
   
   def includeFile(n: String) : Boolean = n.endsWith(".elf")
   
   var path : File = null
   /** Twelf setting "set unsafe ..." */
   var unsafe : Boolean = true
   /** Twelf setting "set chatter ..." */
   var chatter : Int = 5
   var catalogOpt : Option[Catalog] = None
   var port = 8083
   
   /** 
    * creates and intializes a Catalog
    * first argument is the location of the twelf-server script
    */
   override def init(con: frontend.Controller, args: List[String]) {
      super.init(con, Nil)
      if (args.isEmpty) throw ParseError("no path to Twelf given")
      path = File(args(0))
      val cat = new Catalog(HashSet(), HashSet("*.elf"), HashSet(".svn"), port, true, report("lfcatalog", _))
      cat.init    //  throws PortUnavailable
      catalogOpt = Some(cat)
   }
   override def register(arch: Archive) {
      val dim = arch.properties.get("twelf").getOrElse(arch.sourceDim)
      val stringLoc = (arch.root / dim).getPath
      catalogOpt.foreach(_.addStringLocation(stringLoc))
      super.register(arch)
   }
   override def destroy {
      catalogOpt.foreach(_.destroy)
   }
   
   /** 
     * Compile a Twelf file to OMDoc
     * @param in the input Twelf file 
     * @param out the file in which to put the generated OMDoc
     */
   def buildOne(in: File, dpath: Option[DPath], out: File) : List[SourceError] = {
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
      var errors : List[SourceError] = Nil
      while ({line = output.readLine; line != null}) {
         line = line.trim
         val (treat, dropChars, warning) =
            if (line.endsWith("Warning:")) (true, 9, true)
            else if (line.endsWith("Error:")) (true, 7, false)
            else (false, 0, false)
         if (treat) {
            val r = Twelf.parseRef(line.substring(0, line.length - dropChars))
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