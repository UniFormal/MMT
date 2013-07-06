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
   var catalog : Catalog = null
   var port = 8083
   
   /** 
    * creates and intializes a Catalog
    * first argument is the location of the twelf-server script
    */
   override def init(con: frontend.Controller, args: List[String]) {
      super.init(con, Nil)
      if (args.isEmpty) throw ParseError("no path to Twelf given")
      path = File(args(0))
      catalog = new Catalog(HashSet(), HashSet("*.elf"), HashSet(".svn"), port, true, report("lfcatalog", _))
      catalog.init    //  throws PortUnavailable
   }
   override def register(arch: Archive) {
      val dim = arch.properties.get("twelf").getOrElse(arch.sourceDim)
      val stringLoc = (arch.root / dim).getPath
      catalog.addStringLocation(stringLoc)
      super.register(arch)
   }
   override def destroy {
      catalog.destroy
   }
   
   /** 
     * Compile a Twelf file to OMDoc
     * @param in the input Twelf file 
     * @param dpath unused (could be passed to Twelf as the default namespace in the future)
     * @param out the file in which to put the generated OMDoc
     */
   def buildOne(in: File, dpath: DPath, out: File) : List[SourceError] = {
      File(out.getParent).mkdirs
      val procBuilder = new java.lang.ProcessBuilder(path.toString)
      procBuilder.redirectErrorStream()
      val proc = procBuilder.start()
      val input = new PrintWriter(proc.getOutputStream(), true)
      val output = new BufferedReader(new InputStreamReader(proc.getInputStream()))
      input.println("set chatter " + chatter)
      input.println("set unsafe " + unsafe)
      input.println("set catalog " + catalog.queryURI)
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
   
   def compileOne(inText : String, dpath : DPath) : (String, List[SourceError]) = {
     val inFileName = "/tmp/in.elf"
     val outFileName = "/tmp/out.omdoc"
     val in = new PrintWriter("/tmp/in.elf")
     in.write(inText)
     in.close()
     val errors = buildOne(File(inFileName), dpath, File(outFileName))
     val source = scala.io.Source.fromFile(outFileName)
     val lines = source.getLines.mkString("\n")
     source.close()     
     (lines, errors)
   }
   
}