/**
 * takes an abstract logic syntax, produces concrete syntax declarations via Compiler, then translates those to haskell code via Haskell and writes the declarations to their according files in Hets-like file collection 
 * 
 * created by Aivaras Jakubauskas a.jakubauskas@jacobs-university.de
 */
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
//   private case class FileSys(logicName : String, var basic : List[DECL] = Nil,var sign : List[DECL] = Nil, var morphism : List[DECL] = Nil, var tools : List[DECL] = Nil)
//   private def split(d : DECL, fs : FileSys) : FileSys = {
//     d match {
//     	case d : ADT => fs.basic ::= d
//     	case d : ADTRec => fs.basic ::= d//l map {x => split(x,fs)} 
//     	case d : TYPEDEF => if (d.name == "sigs") fs.basic ::= d else fs.basic ::= d
//     	case d : FUNCTION => fs.tools ::= d 
//     	case d : FUNCTIONRec => fs.tools ::= d
//     	case d : RECORD => if (d.name == "theo") fs.basic ::= d else fs.basic ::= d
//     	case d : EXCEPTION => fs.basic ::= d
//     	case _ => 
//     }
//     fs
//   }
  /*
   * splits logic to files
   * figure out what part of logic syntax a declaration is --> compile an appropriate file name --> write declaration to file
   */
   /**
    * ls - logic syntax
    * lname - logic name
    * dir - Hets main directory
    */
  def compile(ls : LogicSyntax, lname : String, hetsdir : String) : Unit = {
    
    val ldir = hetsdir + "/" + lname
    val mod = new java.io.File(ldir)
    if (!mod.exists()) { if (!mod.mkdirs()) throw LogicWriterError("logic directory " + mod.toString() + " could not be created, must quit") } 
    
    val comp = new Compiler(ls)
    val decls = comp.get
    val labels = comp.getLabels
    
    val decls_l = decls.zip(labels)

    // crawl through templates instead?
    val templdir = "/home/aivaras/Hets-src/MMT/newLogicTemplates/" 
    
    comp.getLabels.distinct foreach { x =>
    	val dd = decls_l mapPartial { 
    	  case (d,y) => if (x == y) Some(d) else  None
    	}
    	// pick the corresponding template file
    	val (templ,fname) : (java.io.File,String) = x match {
    	  case "sig" => (new java.io.File(templdir + "/" + "Sign.tmpl"),"Sign.hs")
    	  case "tree" => (new java.io.File(templdir + "/" + "GTools.hs"),"GTools.hs")
    	  case "basic" => (new java.io.File(templdir + "/" + "AS.tmpl"),"AS_BASIC_" + lname + ".hs")
    	  case "mor" => (new java.io.File(templdir + "/" + "Morphism.tmpl"),"Morphism.hs")
    	  case "funs" => (new java.io.File(templdir + "/" + "Tools.tmpl"),"Tools.hs")
    	}
    	write(ldir,lname :: Haskell.prog(dd),fname, templ)
    }
    val mainLName = "Logic_" + lname + ".hs"
    val tempL = new java.io.File(templdir + "/" + "Logic.tmpl")
    write(ldir, List(lname), mainLName, tempL)
  }
  
  /*
   * takes a list of DECLarations and an output file name, writes declarations to a file
   */
   /**
    * logdir - output directory
    * cont - generated code
    * filename - destination of generated code file name
    * templ - template file
    */
  def write(logdir : String, cont : List[String], filename : String, templ : java.io.File) : Unit = {
    //TODO GTools has been dropped
    if (filename == "GTools.hs") 
      return    
      
    println("reading template " + templ.toString())
    val templdir = "/home/aivaras/Hets-src/MMT/newLogicTemplates/"
    val file = File(new java.io.File(logdir + "/" + filename))    
    println("writing to " + file.toString())
    
    val src = scala.io.Source.fromFile(templ).mkString
//    println(src)
    val lname = cont.head
    val logic : List[String] = cont.tail
    File.write(file,src.replaceAll("""<LogicName>""", lname).replaceAll("""<insert>""",logic.mkString))
//    fillTemplate(new java.io.File(logdir + "/" + filename), cont)
  }
  
  /** takes a file and content, fills in the content of the file, returns error list (unfilled tags) */
  def fillTemplate(template : java.io.File, cont : List[String]) : List[LogicWriterError] = {
    var errList = Nil
    val tag = """<[\w]*>""".r
	   // read logic template file
       //TODO local, make global!
    val lname = cont(0) // assume first element is logic name
	   // replace tags with names 

    var logic : List[String] = cont.tail
    File.ReadLineWise(File(template))(x => logic ++= List(x) )
    val filled = logic map   {
    		x => val q = x //replaceAll("""<LogicName>""", lname) replaceAll("""<insert>""","logic.mkString")// replaceAll("<ptree>","") replaceAll("<sign>","") replaceFirst("<fun","")
//    		errList = errList ::: (tag findAllIn filled)
//    		q map { y => 
//    		  errList = errList ::: (tag findAllIn y.toString()).toList
//    		}
    		q
    }
//    println(filled.mkString("\n"))
    File.write(File(template),filled.mkString("\n"))	
    
	   // write file
    //TODO regex match any remaining <tags>, give list as errors
    
    Nil map {x => LogicWriterError("tag not matched: " + x)}
    
  }
  
}
