package info.kwarc.mmt.tptp

import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.utils._

class TPTPImporter extends TraversingBuildTarget {
  val key: String = "tptp-twelf"
  val inDim = source
  val outDim = Dim("twelf")

  val inExts: List[String] = List("ax", "p", "tptp")

  override val outExt: String = "elf"

  def includeFile(n: String): Boolean = inExts.exists(e => n.endsWith("." + e))

  /** command to run TPTP */
  private var swipl: String = "swipl"
  /** relative path in input dimension */
  private var tptp2Xmain: String = "TPTP2X/tptp2X.main"
  private var tptp2Xgoal: String = "tptp2X('INFILE',none,lf,'OUTDIR'),halt."
  /** to be adjusted before being called */
  private var tptpCommand: List[String] = List(swipl, "-f", tptp2Xmain, "-g", tptp2Xgoal)

  /**
   * expects command to run TPTP as arguments, e.g.,
   *
   * swipl -f /tptp/Distribution/sourceTPTP2X/tptp2x.main -g tptp2X('INFILE',none,lf,'OUTDIR'),halt.
   * INFILE and OUTDIR must be absolute files
   * the (swi) prolog binary, file, and goal can also be given via "envvar" (below)
   */
  override def start(args: List[String]): Unit = {

    args match {
      case hd :: tl =>
        swipl = hd
        tl match {
          case "-f" :: snd :: rt =>
            tptp2Xmain = snd
            rt match {
              case "-g" :: gl :: Nil => tptp2Xgoal = gl
              case Nil => tptp2Xgoal = controller.getEnvVar("Tptp2XGoal").getOrElse(tptp2Xgoal)
              case _ => LocalError("expected TPTP goal (-g) instead of: " + rt.mkString(" "))
            }
          case Nil => tptp2Xmain = controller.getEnvVar("Tptp2X").getOrElse(tptp2Xmain)
          case _ => LocalError("expected TPTP prolog file (-f) instead of: " + tl.mkString(" "))
        }
      case Nil => swipl = controller.getEnvVar("SwiProlog").getOrElse(swipl)
    }
  }

  private def escape(f: File) = f.toString.replace("\\", "/")

  def buildFile(bt: BuildTask): Unit = {
    // make tptp2Xmain absolute
    if (!File(tptp2Xmain).isAbsolute)
      tptp2Xmain = File(bt.archive / inDim / tptp2Xmain).toString
    if (!File(tptp2Xmain).exists)
      tptp2Xmain = File(bt.archive.root.up.up / "tptp" / "Distribution" / "source" / "TPTP2X" / "tptp2X.main").toString
    tptpCommand = List(swipl, "-f", tptp2Xmain, "-g", tptp2Xgoal)
    val outFile = bt.outFile
    val tempFile = outFile.setExtension("temp")
    outFile.delete()
    val command = tptpCommand.map { s =>
      s.replace("INFILE", escape(bt.inFile)).replace("OUTDIR", escape(outFile.up))
    }
    log(command.mkString(" "))
    try {
      val result = ShellCommand.run(command: _*)
      result foreach { s =>
        bt.errorCont(LocalError(s))
        outFile.delete()
      }
      // outFile wraps module header/footer around non-modular tempFile
      if (outFile.exists) {
        outFile.renameTo(tempFile)
        val outWriter = File.Writer(outFile)
        val problemName = bt.inFile.removeExtension.name.replace(".", "_")
        val prefix =
          s"""%namespace "${bt.archive.narrationBase}".
%namespace tptp = "http://latin.omdoc.org/logics/tptp".

%sig $problemName = {
   %meta tptp.THF.
"""
        outWriter.write(prefix)
        File.ReadLineWise(tempFile) { l =>
          outWriter.println(l)
        }
        outWriter.println("}.")
        outWriter.close()
        tempFile.delete()
      }
    } catch {
      case e: Throwable =>
        outFile.delete()
        tempFile.delete()
        bt.errorCont(LocalError("exception for file: " + bt.inFile + "\n" +
          Option(e.getMessage).getOrElse("(no message)")).setCausedBy(e))
    }
  }
}
