package info.kwarc.mmt.stex

import java.nio.file.Files

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils.{File, FileURI, ShellCommand}

/** importer wrapper for stex
  */
class STeX extends Importer with frontend.ChangeListener {
  override val key = "stex-omdoc"

  private var lmlPath: File = File("run-latexml.sh")

  def includeFile(n: String): Boolean =
    n.endsWith(".tex") && ! n.endsWith("localpaths.tex")

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
      if (logFile.exists()) {
        val source = scala.io.Source.fromFile(logFile)
        val itr = source.getLines()
        var msg: List[String] = Nil
        var newMsg = true
        var optLevel: Option[Level.Level] = None
        var region = SourceRegion.none
        def reportError() = {
          optLevel match {
            case Some(lev) =>
              val ref = SourceRef(FileURI(bt.inFile), region)
              bt.errorCont(CompilerError(key, ref, msg.reverse, lev))
              optLevel = None
            case None =>
          }
          msg = Nil
          newMsg = true
          region = SourceRegion.none
        }
        while (itr.hasNext) {
          val line = itr.next()
          val msgLine = """(Info|Warning|Error|Fatal):.*""".r
          line match {
            case msgLine(lev) =>
              optLevel = Some(lev match {
                case "Info" => Level.Info
                case "Error" => Level.Error
                case "Fatal" => Level.Fatal
                case _ => Level.Warning
              })
              msg = List(line)
              newMsg = false
            case _ => if (line.startsWith("\t")) {
              val sLine = line.substring(1)
              val regEx = """#textrange\(from=(\d+);(\d+),to=(\d+);(\d+)\)""".r
              val range = sLine.stripPrefix(bt.inFile.toString)
              range match {
                case regEx(l1, c1, l2, c2) =>
                  region = SourceRegion(SourcePosition(-1, l1.toInt, c1.toInt),
                    SourcePosition(-1, l2.toInt, c2.toInt))
                case _ =>
              }
              if (region == SourceRegion.none) msg = sLine :: msg
            }
            else reportError()
          }
        }
        reportError()
      }
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
