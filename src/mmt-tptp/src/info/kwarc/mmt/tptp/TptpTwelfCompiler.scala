package info.kwarc.mmt.tptp

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.utils._

import scala.sys.process._

// deprecated, probably obsolete
/*
/**
  * TPTP twelf Compiler, translates TPTP sources to twelf using tptp2x
  */
class TptpTwelfCompiler extends TraversingBuildTarget {
  val key = "tptp-twelf"
  val inDim = source
  val outDim = Dim("twelf")
  override val outExt = "elf"

  private var tptp2x: File = null

  /** one argument - path to TPTP folder */
  override def start(args: List[String]) {
    tptp2x = File(args(0)) / "TPTP2X" / "tptp2X"
  }

  def includeFile(n: String) = n.endsWith(".tptp")

  def buildFile(bf: BuildTask): BuildResult = {
    // should be  .../TPTP/TPTP2X
    //  runs tptp2X script with parameters -flf format twelf -d- output directory - stdout
    val flags = "-flf -d- -q2"
    val cmd = "tcsh " + tptp2x + " " + flags + " " + bf.inFile.toString
    // log(cmd)
    (cmd #> bf.outFile.toString).!
    BuildResult.empty
  }
}
*/