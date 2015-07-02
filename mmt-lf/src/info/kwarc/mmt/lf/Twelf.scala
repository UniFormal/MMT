package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import archives._
import backend._
import parser._
import utils.{File,FileURI}

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
      val reg = SourceRegion(SourcePosition(-1, numbers(0).toInt, numbers(1).toInt),
                                    SourcePosition(-1, numbers(2).toInt, numbers(3).toInt))
      SourceRef(utils.FileURI(file), reg)
   }
   val dim = RedirectableDimension("twelf", Some(source))
}

/** importer wrapper for Twelf, which starts the catalog */
class Twelf extends Importer with frontend.ChangeListener {
   val key = "twelf-omdoc"
   override def inDim = Twelf.dim

   def inExts = List("elf","twelf","lf")

   /** path to Twelf executable */
   private var path : File = null
   /** Twelf setting "set unsafe ..." */
   private val unsafe : Boolean = true
   /** Twelf setting "set chatter ..." */
   private val chatter : Int = 5
   /** initial port of lfcatalog */
   private val port = 8083
   private var catalog : Catalog = null

   /**
    * creates and initializes a Catalog
    * first argument is the location of the twelf-server script; alternatively set variable GraphViz
    */
   override def start(args: List[String]) {
      val p = getFromFirstArgOrEnvvar(args, "Twelf")
      path = File(p)
      catalog = new Catalog(HashSet(), HashSet("*.elf"), HashSet(".svn"), port, true, report("lfcatalog", _))
      catalog.init    //  throws PortUnavailable
      controller.backend.getArchives foreach onArchiveOpen
   }
   override def onArchiveOpen(arch: Archive) {
      val stringLocs = arch.properties.get("lfcatalog-locations") match {
         case None =>
            List(arch / Twelf.dim)
         case Some(s) =>
            utils.stringToList(s, "\\s").map {f => arch / Twelf.dim / f}
      }
      stringLocs.foreach {l => catalog.addStringLocation(l.getPath)}
   }
   override def onArchiveClose(arch: Archive) {
      val stringLoc = (arch / Twelf.dim).getPath
      catalog.deleteStringLocation(stringLoc)
   }
   override def destroy {
      catalog.destroy
   }

   /**
     * Compile a Twelf file to OMDoc
     * @param bf the build task
     * @param seCont document continuation for indexing
     */
   def importDocument(bf: BuildTask, seCont: documents.Document => Unit) {
      val procBuilder = new java.lang.ProcessBuilder(path.toString)
      procBuilder.redirectErrorStream
      val proc = procBuilder.start
      val input = new PrintWriter(proc.getOutputStream, true)
      val output = new BufferedReader(new InputStreamReader(proc.getInputStream))
      val inFile = bf.inFile
      val outFile = bf.archive / RedirectableDimension(key) / ArchivePath(bf.inPath).setExtension(outExt).segments
      outFile.up.mkdirs()
      if (inFile.length > 100000000) {
         bf.errorCont(LocalError("skipped big elf file: " + inFile))
         return
      }
      def toTwelf(s: String) {
         log(s)
         input.println(s)
      }
      toTwelf("set chatter " + chatter)
      toTwelf("set unsafe " + unsafe)
      toTwelf("set catalog " + catalog.queryURI)
      toTwelf("loadFile " + inFile)
      toTwelf("Print.OMDoc.printDoc " + inFile + " " + outFile)
      toTwelf("OS.exit")
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
            bf.errorCont(CompilerError(key, r, msg.reverse, Level.Warning))
         }
      }
      def error(msg: String) {
         val ref = SourceRef(FileURI(inFile), parser.SourceRegion.none)
         val e = CompilerError(key, ref, List(msg), Level.Fatal)
         bf.errorCont(e)
      }
      if (! (outFile.exists && outFile.length > 0)) {
          error("unknown error: Twelf produced no omdoc file")
          return
      }
      val doc = try {
         val dp = bf.narrationDPath
         val ps = ParsingStream.fromFile(outFile, Some(dp.copy(uri = dp.uri.setExtension("omdoc"))))
         controller.read(ps, false)(bf.errorCont)
       } catch {
         case e: scala.xml.parsing.FatalError =>
            error("XML error in omdoc file (likely too big for Twelf to write)")
            return
       }
       seCont(doc)
   }
/*
   def importString(inText : String, dpath : DPath)(implicit errorCont: ErrorHandler) {
     val tmp = File(System.getProperty("java.io.tmpdir"))
     val inFileName = tmp / "in.elf"
     val outFileName = tmp / "out.omdoc"
     File.write(inFileName,inText)
     val bf = new archives.BuildTask(File(inFileName), false, List("string"), dpath.uri, File(outFileName), errorCont)
     importDocument(bf, doc => ())
   }
*/
}
