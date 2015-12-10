package info.kwarc.mmt.lf

import java.io._

import info.kwarc.mmt.api._
import archives._
import info.kwarc.mmt.twelf.Catalog
import parser._
import utils.{File, FileURI}

/** helper methods for Twelf */
object Twelf {
  /** parses filename:col.line-col.line */
  def parseRef(s: String): SourceRef = {
    val i = s.lastIndexOf(":")
    val file = File(s.substring(0, i))
    val numbers = s.substring(i + 1).split("[-\\.]")
    val reg = SourceRegion(SourcePosition(-1, numbers(0).toInt, numbers(1).toInt),
      SourcePosition(-1, numbers(2).toInt, numbers(3).toInt))
    SourceRef(utils.FileURI(file), reg)
  }
}

/** importer wrapper for Twelf, which starts the catalog */
class Twelf extends Importer with frontend.ChangeListener {
  val key = "twelf-omdoc"

  override def inDim: RedirectableDimension = RedirectableDimension("twelf", Some(source))

  def inExts: List[String] = List("elf", "twelf", "lf")

  /** path to Twelf executable, will be overridden in start */
  private var path: File = File("twelf-server")
  /** Twelf setting "set unsafe ..." */
  private val unsafe: Boolean = true
  /** Twelf setting "set chatter ..." */
  private val chatter: Int = 5
  /** initial port of lfcatalog */
  private val port = 8083
  /** will be overridden in start */
  private var catalog: Catalog = null

  /**
    * creates and initializes a Catalog
    * first argument is the location of the twelf-server script; alternatively set variable Twelf
    */
  override def start(args: List[String]) {
    super.start(args)
    val p = getFromFirstArgOrEnvvar(remainingStartArguments, "Twelf", "twelf-server")
    path = File(p)
    catalog = new Catalog(port = port, searchPort = true, log = report("lfcatalog", _))
    catalog.init //  throws PortUnavailable
    controller.backend.getArchives foreach onArchiveOpen
  }

  override def onArchiveOpen(arch: Archive) {
    val stringLocs = arch.properties.get("lfcatalog-locations") match {
      case None =>
        List(arch / inDim)
      case Some(s) =>
        utils.stringToList(s, "\\s").map { f => arch / inDim / f }
    }
    stringLocs.foreach { l => catalog.addStringLocation(l.getPath) }
  }

  override def onArchiveClose(arch: Archive) {
    val stringLoc = (arch / inDim).getPath
    catalog.deleteStringLocation(stringLoc)
  }

  override def destroy {
    catalog.destroy
  }

  def runTwelf(bf: BuildTask, outFile: File) {
    val procBuilder = new java.lang.ProcessBuilder(path.toString)
    procBuilder.redirectErrorStream
    val proc = procBuilder.start
    val input = new PrintWriter(proc.getOutputStream, true)
    val output = new BufferedReader(new InputStreamReader(proc.getInputStream))
    val inFile = bf.inFile
    if (inFile.length > 100000000) {
      bf.errorCont(LocalError("skipped big elf file: " + inFile))
    } else {
      def sendToTwelf(s: String) {
        //log(s)
        input.println(s)
      }
      sendToTwelf("set chatter " + chatter)
      sendToTwelf("set unsafe " + unsafe)
      sendToTwelf("set catalog " + catalog.queryURI)
      sendToTwelf("loadFile " + inFile)
      sendToTwelf("Print.OMDoc.printDoc " + inFile + " " + outFile)
      sendToTwelf("OS.exit")
      var optLine: Option[String] = None
      while ( {
        optLine = Option(output.readLine)
        optLine.isDefined
      }) {
        val line = optLine.get.trim
        val (treat, dropChars) =
          if (line.endsWith("Warning:")) (true, 9)
          else if (line.endsWith("Error:")) (true, 7)
          else (false, 0)
        if (treat) {
          val r = Twelf.parseRef(line.substring(0, line.length - dropChars))
          var msg: List[String] = Nil
          do {
            msg ::= Option(output.readLine).getOrElse("")
          } while (!msg.head.startsWith("%%"))
          bf.errorCont(CompilerError(key, r, msg.reverse, Level.Warning))
        }
      }
    }
  }

  /**
    * Compile a Twelf file to OMDoc
    * @param bf the build task
    * @param seCont document continuation for indexing
    */
  def importDocument(bf: BuildTask, seCont: documents.Document => Unit): BuildResult = {
    val outFile = bf.archive / RedirectableDimension(key) / bf.inPath.toFile.setExtension(outExt).toFilePath
    outFile.up.mkdirs()
    outFile.delete()
    runTwelf(bf, outFile)
    def error(msg: String) {
      val ref = SourceRef(FileURI(bf.inFile), parser.SourceRegion.none)
      val e = CompilerError(key, ref, List(msg), Level.Fatal)
      bf.errorCont(e)
    }
    if (!(outFile.exists && outFile.length > 0)) {
      error("unknown error: Twelf produced no omdoc file")
    } else
      try {
        val dp = bf.narrationDPath
        val ps = ParsingStream.fromFile(outFile, Some(dp.copy(uri = dp.uri.setExtension("omdoc"))))
        seCont(controller.read(ps, interpret = false)(bf.errorCont))
      } catch {
        case e: scala.xml.parsing.FatalError =>
          error("XML error in omdoc file (likely too big for Twelf to write)")
      }
    BuildResult.empty
  }
}
