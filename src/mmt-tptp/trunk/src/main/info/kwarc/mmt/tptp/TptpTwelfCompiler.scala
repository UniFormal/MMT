package info.kwarc.mmt.tptp

import tptp._

import info.kwarc.mmt.api._
import documents._
import utils._
import frontend._
import backend._
import symbols._
import libraries._
import modules._
import objects._
import presentation._

import info.kwarc.mmt.lf.Twelf

/* ?	need this to execute shell commands	?
*/
import scala.sys.process._


/**
 * TPTP twelf Compiler, translates TPTP sources to twelf using tptp2x
 */
class TptpTwelfCompiler extends Compiler {
  private var tptppath : String = null
  private def log(msg: => String) {report("tptp", msg)}
  def isApplicable(src : String) : Boolean = src == "tptp"
  override def init(rep: Report, args: List[String]) {
     tptppath = args(0)     
     super.init(rep, Nil)
  }

  override def compile(in : File, out : File) : List[CompilerError] = {
    var errors: List[CompilerError] = Nil    
   
   // should be  .../TPTP/TPTP2X  
    var tptp2Xpath : String = tptppath.substring(0, tptppath.indexOf("MMT")) + "TPTP2X/"
    
    // compiled file name
    var fileName : String = in.toString().substring(in.toString().lastIndexOf("/"))
    
    // output file
     var outFile : String = out.toString() 
     
    // output dir
    var outDir : String = out.toString().substring(0,out.toString().lastIndexOf("/")) 
    
    out.setExtension("elf")
     
    
    log("running tptp2X script on file " + fileName + " .....")
    //log(tptp2Xpath.toString)
 //   log(in.toString())
    log(out.toString())
  //  log(outDir)
    
    val myEnv = new Env(tptp2Xpath.toString())
    
    /* 
     * runs tptp2X script
     * 
     * with parameters:
     * 
     *  -f+lf format twelf
     *  -d output directory
     */ 
    //myEnv.run("tcsh","tptp2X", "-flf", "-d-", in.toString(), ">" , out.toString())

    
    Runtime.getRuntime().exec("tcsh /home/aivaras/tptp2X -flf -d- " + in.toString() + " > " + out.toString())
    
    log("compiled to " + out.toString())
    
    
    
    
    
    errors
  }
  
}


class Env(working : String) { 
         private val workDir = new java.io.File(working) 
         def run(cmd : String*) = if (Runtime.getRuntime(). 
                 exec(cmd.toArray[String], null, workDir).waitFor() != 0) 
             throw new Exception("Execution failed. Better luck next time!") 
}