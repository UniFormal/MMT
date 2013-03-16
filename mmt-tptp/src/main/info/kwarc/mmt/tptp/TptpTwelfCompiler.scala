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

//import info.kwarc.mmt.lf.Twelf // maybe don't need this

import scala.sys.process._ // need this to execute shell commands
//import scala.io._ // to write to file
//import java.io._ // *


/**
 * TPTP twelf Compiler, translates TPTP sources to twelf using tptp2x
 */
class TptpTwelfCompiler extends Compiler {
  private var tptppath : String = null
  private def log(msg: => String) {report("tptptwelf", msg)}
  def isApplicable(src : String) : Boolean = src == "tptp"
  override def init(con: Controller, args: List[String]) {
     tptppath = args(0)     
     super.init(con, Nil)
  }

  override def compile(in : File, dpath: Option[DPath], out : File) : List[SourceError] = {
    var errors: List[SourceError] = Nil    
   
   // should be  .../TPTP/TPTP2X  
    var tptp2Xpath : String = tptppath.substring(0, tptppath.indexOf("MMT")) + "TPTP2X/"    
    // compiled file name
    var fileName : String = in.toString().substring(in.toString().lastIndexOf("/"))    
    // output file
     var outFile : String = out.toString()      
    // output dir
    var outDir : String = out.toString().substring(0,out.toString().lastIndexOf("/"))
//    set file extension
    val fileout = out.setExtension("elf")
         
    log("running tptp2X script on file " + fileName + " .....")
    log(fileout.toString())
    log(in.toString)
    /* 
     * runs tptp2X script
     * 
     * with parameters:
     * 
     *  -flf format twelf
     *  -d- output directory - stdout
     */    
    val tptp2xcomp = "tptp2X"
    val flags : String = "-flf -d- -q2"
    val cmd : String = "tcsh " + tptp2Xpath + tptp2xcomp + " " + flags + " " + in.toString()
  
//    log("about to run!!!")
//     check if file already exists, overwrite!  
    // check if output target dir exists, if not, create dirs
    var outf : java.io.File = new java.io.File(outDir.toString())
    if (! outf.exists()) {
      outf.mkdirs()
    }
    
    cmd #> new java.io.File(fileout.toString()) !        
    
    
    log("compiled to " + fileout.toString())
    
    
//    a nice way of dealing with concrete files should be found
//    right now the compiler is called with the same inFile as during the first compilation
//    i.e. .elf is sought even after 
    outFile
    errors
  }
  
}

