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
class Twelf extends ExternalImporter with frontend.ChangeListener {
  val toolName = "Twelf"

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
  override def start(args: List[String]): Unit = {
    val p = getFromFirstArgOrEnvvar(args, "Twelf", "twelf-server")
    path = File(p)
    catalog = new Catalog(port = port, searchPort = true, log = report("lfcatalog", _))
    catalog.init //  throws PortUnavailable
    controller.backend.getArchives foreach onArchiveOpen
  }

  override def onArchiveOpen(arch: Archive): Unit = {
    val stringLocs = arch.properties.get("lfcatalog-locations") match {
      case None =>
        List(arch / inDim)
      case Some(s) =>
        utils.stringToList(s, "\\s").map { f => arch / inDim / f }
    }
    stringLocs.foreach { l => if (l.exists) catalog.addStringLocation(l.getPath) }
  }

  override def onArchiveClose(arch: Archive): Unit = {
    val stringLoc = (arch / inDim).getPath
    catalog.deleteStringLocation(stringLoc)
  }

  override def destroy: Unit = {
    catalog.destroy
  }

  def runExternalTool(bf: BuildTask, outFile: File): Unit = {
    val procBuilder = new java.lang.ProcessBuilder(path.toString)
    procBuilder.redirectErrorStream
    val proc = procBuilder.start
    val input = new PrintWriter(proc.getOutputStream, true)
    val output = new BufferedReader(new InputStreamReader(proc.getInputStream))
    val inFile = bf.inFile
    if (inFile.length > 100000000) {
      bf.errorCont(LocalError("skipped big elf file: " + inFile))
    } else {
      def sendToTwelf(s: String): Unit = {
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
}
