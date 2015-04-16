package info.kwarc.mmt.stex

import java.nio.file.Files

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.utils.{File, ShellCommand}

/** importer wrapper for stex
  */
class STeX extends Importer with frontend.ChangeListener {
  override val key = "stex-omdoc"

  private var lmlPath: File = File("run-latexml.sh")

  def includeFile(n: String): Boolean = n.endsWith(".tex") && n != "localpaths.tex"

  /**
   * Compile a .tex file to OMDoc
   */
  def importDocument(bt: BuildTask, seCont: documents.Document => Unit) {
    val command: List[String] = List(lmlPath, bt.inFile) map (_.toString)
    log(command.mkString(" "))
    bt.outFile.delete()
    try {
      val result = ShellCommand.run(command: _*)
      result foreach { s =>
        bt.errorCont(LocalError(s))
        return
      }
      val lmhOut = bt.inFile.setExtension("omdoc")
      val logFile = bt.inFile.setExtension("ltxlog")
      if (lmhOut.exists()) Files.move(lmhOut.toPath, bt.outFile.toPath)
      log(logFile.toString)
    }
    catch {
      case e: Throwable =>
        val msg = e.getMessage match {
          case null => "(no message)"
          case m => m
        }
        bt.outFile.delete()
        bt.errorCont(LocalError("exception for file: " + bt.inFile + "\n" + msg).setCausedBy(e))
    }
  }
}
