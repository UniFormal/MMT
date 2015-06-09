package info.kwarc.mmt.stex

import java.nio.file.Files

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.parser.{SourcePosition, SourceRef, SourceRegion}
import info.kwarc.mmt.api.utils.{File, FileURI, ShellCommand}

/** importer wrapper for stex
  */
class LaTeXML extends Importer with frontend.ChangeListener {
  override val key = "latexml"

  private var lmlPath: File = File("run-latexml.sh")

  override def includeFile(n: String): Boolean =
    super.includeFile(n) && !n.endsWith("localpaths.tex")

  def inExts = List("tex")

  def str2Level(lev: String): Level.Level = lev match {
    case "Info" => Level.Info
    case "Error" => Level.Error
    case "Fatal" => Level.Fatal
    case _ => Level.Warning
  }

  def line2Region(sLine: String, inFile: File): SourceRegion = {
    val regEx = """#textrange\(from=(\d+);(\d+),to=(\d+);(\d+)\)""".r
    val range = sLine.stripPrefix(inFile.toString)
    range match {
      case regEx(l1, c1, l2, c2) =>
        SourceRegion(SourcePosition(-1, l1.toInt, c1.toInt),
          SourcePosition(-1, l2.toInt, c2.toInt))
      case _ => SourceRegion.none
    }
  }

  def line2Level(line: String): (Option[Level.Level], String) = {
    val msgLine = """(Info|Warning|Error|Fatal):(.*)""".r
    line match {
      case msgLine(lev, rest) => (Some(str2Level(lev)), rest)
      case _ => (None, line)
    }
  }

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
      var optLevel: Option[Level.Level] = None
      var msg: List[String] = Nil
      var newMsg = true
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
      if (logFile.exists()) {
        val source = scala.io.Source.fromFile(logFile)
        source.getLines().foreach { line =>
          val (newLevel, restLine) = line2Level(line)
          if (newLevel.isDefined) {
            reportError()
            optLevel = newLevel
            msg = List(restLine)
            newMsg = false
          }
          else if (line.startsWith("\t")) {
            val sLine = line.substring(1)
            val newRegion = line2Region(sLine, bt.inFile)
            if (newRegion == SourceRegion.none) msg = sLine :: msg
            else region = newRegion
          }
          else reportError()
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
