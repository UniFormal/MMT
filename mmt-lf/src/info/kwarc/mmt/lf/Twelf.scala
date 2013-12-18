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
class Twelf extends Importer {
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
   override def start(args: List[String]) {
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
   def buildOne(bf: BuildFile, seCont: documents.Document => Unit) = {
      File(bf.outFile.getParent).mkdirs
      val procBuilder = new java.lang.ProcessBuilder(path.toString)
      procBuilder.redirectErrorStream()
      val proc = procBuilder.start()
      val input = new PrintWriter(proc.getOutputStream(), true)
      val output = new BufferedReader(new InputStreamReader(proc.getInputStream()))
      val outFile = bf.outFile.setExtension("omdoc")
      input.println("set chatter " + chatter)
      input.println("set unsafe " + unsafe)
      input.println("set catalog " + catalog.queryURI)
      input.println("loadFile " + bf.inPath)
      input.println("Print.OMDoc.printDoc " + bf.inPath + " " + outFile)
      input.println("OS.exit")
      var line : String = null
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
            bf.errors ::= CompilerError(r, msg.reverse, warning)
         }
      }
      bf.errors = bf.errors.reverse
      val (doc,_) = controller.read(outFile, Some(bf.dpath))
      seCont(doc)
   }
   
   def compileOne(inText : String, dpath : DPath) : (String, List[Error]) = {
     val tmp = File(System.getProperty("java.io.tmpdir"))
     val inFileName = tmp / "in.elf"
     val outFileName = tmp / "out.omdoc"
     utils.File.write(inFileName,inText)
     val bf = new archives.BuildFile(File(inFileName), List("string"), dpath, File(outFileName))
     buildOne(bf, doc => ())
     val lines = utils.File.read(outFileName)
     (lines, bf.errors)
   }
   
}