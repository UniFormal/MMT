package info.kwarc.mmt.lf.compile

import info.kwarc.mmt.lf.compile._

import info.kwarc.mmt.api._
import utils._


import modules._
import symbols._
import libraries._
import objects._
import utils._
import MyList._
import frontend._
import patterns._
import presentation._

class LogicWriter() {
  
   case class LogicWriterError(msg : String) extends java.lang.Throwable(msg)
   
   // a holder for classification to files
   private case class FileSys(logicName : String, var basic : List[DECL] = Nil,var sign : List[DECL] = Nil, var morphism : List[DECL] = Nil, var tools : List[DECL] = Nil)
   private def split(d : DECL, fs : FileSys) : FileSys = {
     d match {
     	case d : ADT => fs.basic ::= d
     	case d : ADTRec => fs.basic ::= d//l map {x => split(x,fs)} 
     	case d : TYPEDEF => if (d.name == "sigs") fs.sign ::= d else fs.basic ::= d
     	case d : FUNCTION => fs.tools ::= d 
     	case d : FUNCTIONRec => fs.tools ::= d
     	case d : RECORD => if (d.name == "theo") fs.sign ::= d else fs.basic ::= d
     	case d : EXCEPTION => fs.basic ::= d
     	case _ => 
     }
     fs
   }
  /*
   * splits logic to files
   * figure out what part of logic syntax a declaration is --> compile an appropriate file name --> write declaration to file
   */
  def compile(decs : List[DECL], lname : String, dir : String) : Unit = {
    
    val ldir = dir + "/" + lname
    val mod = new java.io.File(ldir)
    if (!mod.exists()) { if (!mod.mkdirs()) throw LogicWriterError("logic directory " + mod.toString() + " could not be created, must quit") } 
    
    val fs = FileSys(lname)
    decs.reverse foreach { x => split(x,fs)  }
//    println(fs.toString)
    
    val cont : List[String] = Haskell.prog(decs)
    
    decs foreach {x => println(x)}
    
    write( fs, ldir)
    
  }
  
  /*
   * takes a list of DECLarations and an output file name, writes declarations to a file
   */
  def write(fs : FileSys, logdir : String) : Unit = {    
	val lname = fs.logicName
	val main = logdir + "/" + "Logic_" + lname + ".hs"
	val sign = logdir + "/" + "Sign.hs"
	val basic = logdir + "/" + "AS_BASIC_" + lname + ".hs"
	val morphism = logdir + "/" + "Morphism.hs"
	val tools = logdir + "/" + "Tools.hs"
    
    // write 
    var fw = File(main)
    var pre_main = "module " + " " + lname + "." + "Logic_" + lname + " where\n\n"
    pre_main += "import " + lname + "." + "AS_BASIC_" + lname + "\n\n"
    File.write(fw,pre_main)
    fw = File(basic)
    var pre_basic = "module " + " " + lname + "." + "AS_BASIC_" + lname + " where\n\n" 
    File.write(fw,pre_basic + (fs.basic map {x => Haskell.decl(x)} mkString))
    fw = File(sign)
    File.write(fw,fs.sign map {x => Haskell.decl(x)} mkString)
    fw = File(morphism)
    File.write(fw,fs.morphism map {x => Haskell.decl(x)} mkString)
    fw = File(tools)
    File.write(fw,fs.tools map {x => Haskell.decl(x)} mkString)
  }
  
//  private val preamble = "module " 
  
  private case class LogDescr(lname : String){
    var lines : List[String] = List()
    val name = lname.head.toUpperCase + lname.substring(1)
    def apply() {
      lines ::= "data " + name + " = " + name + " deriving Show"
//      val language = ""
//      val category = ""
//      val sentences = ""
//      val monoid = ""
//      val syntax = ""
//      val logic = ""
    }
  }
  
}

sealed abstract class CLASS 
case class language(lname : String) extends CLASS {
  val name = lname.head.toUpperCase + lname.substring(1)
  var description = "no decription" 
  def apply(descr : String) = { description = descr}
  def print : String = "data " + name + " = " + name + " deriving Show" + "\n" + 
    "instance Language " + name + " where\n" + "description _ = " + "\"" + description + "\"" 
}

case class category(lname : String) extends CLASS {
  val name = lname.head.toUpperCase + lname.substring(1)
  var sign = "()"
  var morphism = "()"
  
//  def print : String =   
}
