package info.kwarc.mmt.specware

import errors._
import info.kwarc.mmt.api._
import archives._
import documents._
import parser._
import utils._

/** imports Specware files
  *
  * The import is rather inefficient, not using Specware's recursive export facility
  *
  * It currently only works on Windows because the specware-xmlprint.cmd script has not been ported to Unix.
  * That should be trivial though.
  *
  */
/*
 * Specware already produces omdoc and err files. These are somewhat awkwardly, reread and indexed.
 * However, that allows fixing them as the generated OMDoc might not always be correct.
 * In particular, names do not always reference the URI.
 *
 */
class SpecwareImporter extends Importer {
  val key = "specware-omdoc"

  def inExts = List("sw")

  private var swdir: File = null
  private var debug = false

  /** parses the XML file of Specware errors */
  private val ErrorParser = new XMLToScala("info.kwarc.mmt.specware.errors")

  /** wraps around the Specware executable to compile one file */
  private class SwCommand(arch: Archive, inFile: File) {
    val windows = System.getProperty("os.name").startsWith("Windows")
    val workDir = swdir / "Applications" / "Specware" / "bin" / (if (windows) "windows" else "linux")
    val binName = if (debug) "specware-local-xmlprint.cmd" else if (windows) "specware-xmlprint.cmd" else "specware-xmlprint"
    val inUId = FileURI(inFile).pathAsString
    val physRoot = FileURI(arch / source)
    val command = List(workDir / binName, physRoot, arch.narrationBase).map(_.toString) ::: List(inUId, "nil")

    def run = ShellCommand.runIn(workDir, command: _*)

    private val baseFile = inFile.up / "omdoc" / inFile.name
    val outFile = baseFile.setExtension("omdoc")
    val errFile = baseFile.setExtension("sw.err")

    def getErrors = ErrorParser(errFile).asInstanceOf[errors]._children
  }

  /** one argument: the location of the Specware directory */
  override def start(args: List[String]): Unit = {
    val rest = if (args.headOption.contains("debug")) {
      debug = true
      args.tail
    } else
      args
    val p = getFromFirstArgOrEnvvar(rest, "Specware")
    swdir = File(p)
  }

  def importDocument(bt: BuildTask, index: Document => Unit): BuildResult = {
    val swC = new SwCommand(bt.archive, bt.inFile)
    log(swC.command.mkString(" "))
    swC.outFile.delete
    swC.errFile.delete
    val result = swC.run
    result.errorO foreach { r =>
      throw LocalError("failed to run Specware: " + r)
    }
    swC.getErrors foreach { e => bt.errorCont(SourceError(key, e.getSourceRef, e.shortMsg)) }
    if (swC.outFile.exists) {
      val dp = bt.narrationDPath
      val ps = ParsingStream.fromFile(swC.outFile, Some(dp.copy(uri = dp.uri.setExtension("omdoc"))))
      val doc = controller.read(ps, interpret = false)(bt.errorCont)
      index(doc)
    } else {
      bt.errorCont(LocalError("no output file produced"))
    }
    BuildResult.empty
  }
}
