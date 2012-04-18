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

/**
 * TPTP twelf Compiler, translates TPTP sources to twelf using tptp2x
 */
class TptpTwelfCompiler extends Compiler {
  
  def isApplicable(src : String) : Boolean = src == "tptp"

  override def compile(in : File, out : File) : List[CompilerError] = {
    var errors: List[CompilerError] = Nil

    
    /*
     *	get the file paths here
     * 
     *   */
     
    /*
     * run tptp2X shell script with the file paths here
     * 
     */
    
    errors
  }
  
}


