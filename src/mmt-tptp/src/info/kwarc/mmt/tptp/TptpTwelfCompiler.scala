package info.kwarc.mmt.tptp

import tptp._

import info.kwarc.mmt.api._
import utils._
import archives._

import scala.sys.process._ // need this to execute shell commands


/**
 * TPTP twelf Compiler, translates TPTP sources to twelf using tptp2x
 */
class TptpTwelfCompiler extends TraversingBuildTarget {
  val key = "tptp-twelf"
  val inDim = source
  val outDim = Dim("twelf")
  override val outExt = "elf"

  private var tptp2x : File = null
  /** one argument - path to TPTP folder */
  override def start(args: List[String]) {
     checkNumberOfArguments(1,1,args)
     tptp2x = File(args(0)) / "TPTP2X" / "tptp2X"     
  }
  
  def includeFile(n: String) = n.endsWith(".tptp")
  def buildFile(a: Archive, bf: BuildTask) {
    // should be  .../TPTP/TPTP2X  
    //  runs tptp2X script with parameters -flf format twelf -d- output directory - stdout
    val flags = "-flf -d- -q2"
    val cmd = "tcsh " + tptp2x + " " + flags + " " + bf.inFile.toString
    // log(cmd)
    cmd #> bf.outFile.toString !
  }
}