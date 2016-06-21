/**
 * This is the main wrapper class for MMT.
 * It imports main MMT class, parses twelf/spec files upon invocation, calls '
 * logic translation->abstract syntax->concrete syntax->Hets style logic definition' 
 *   pipeline
 * 
 * created by Aivaras:
 * a.jakubauskas@jacobs-university.de
 */
package info.kwarc.mmt.hets


import info.kwarc.mmt.api._
import utils.File._ //maybe the stuff here is useful
import backend.TextReader // the parser

import info.kwarc.mmt.lf.compile._

import scala.util.control.Exception._

// for exception handling
import java.io.FileNotFoundException
import java.io.IOException
import utils._
import utils.MyList._
import modules._
import patterns._
import libraries._
import frontend._
import symbols._
import objects._
import objects.Conversions._
import info.kwarc.mmt.api.frontend._ // for report
//import utils.MyList


import scala.sys.process._

//TODO implementation
/*	+ should be able to read in twelf files
 *  + twelf++ (typecheck and pattern match) the read files
 *  + read in pattern instance specification file (we have a logic + patterns already)
 *  - check the specifications, give sensible error messages if does not pass
 */
case class TheoryLookupError(msg : String) extends java.lang.Throwable(msg)

object Main {
  
   val latinbase = "http://latin.omdoc.org/logics/syntax"
   val foundational = "http://cds.omdoc.org/foundational"
   val hetstest = "http://cds.omdoc.org/hets-test"
  
   def main(args: Array[String]) {
     
     if (args.length == 0) {
    	 println("No arguments given. Expecting [flag] [filepath] arguments.\nTerminating.")
    	 return
     } 
     
       try {
         println("current dir " + getCurrentDir)
         val flag = args(0)
         println("flag: " + flag)
         val filename = args(1) // take first argument - file name of the source
         
         //TODO make global
         println("initiating controller")
         val controller = new info.kwarc.mmt.api.frontend.Controller
//         val uri = new utils.URI(None,None,List(hetstest + "?" + filename))
//         controller.handleLine("archive mmt source-structure")                 
//         controller.handleLine("archive add /home/aivaras/TPTP/MMT/urtheories")
//         controller.handleLine("archive add /home/aivaras/TPTP/MMT/test")
         controller.handleLine("file /home/aivaras/TPTP/test/hets.msl")
         
         val fl = File(new java.io.File(filename))
         if (!(new java.io.File(filename).exists())) {
           println("File " + filename + " does not exist!")
           return
         }

         val argl = args.toList
         
         // =====================================================
         if (argl contains "-newLogic") {
           println("reading new logic")
           println("..from file " + filename)

           // parse mmt doc
           val source = scala.io.Source.fromFile(new java.io.File(filename),"UTF-8")       
           val (doc, err) = controller.read(File(new java.io.File(filename)), None) 
             //controller.textReader.readDocument(source, DPath(uri))(controller.termParser.apply(_))
           source.close()
           if (!err.isEmpty) {
        	   println("errors while reading " + filename + " encountered:")
        	   err map println
        	   return
           }
           // assume theory name is the same as file name
           val thname = filename.substring(filename.lastIndexOf("/")+1, filename.lastIndexOf("."))

           
           // ----------- RUN ----------------------------         
         
//           println(controller.library.toString)
           val theo =  controller.localLookup.getTheory(DPath(utils.URI(hetstest)) ? thname) match {
         		case d : DeclaredTheory => d
         		case _ => throw TheoryLookupError("attempted retrieving not a DeclaredTheory")
           }
           println(theo)
           val tls = new Theory2LogicSyntax()
           println("translating " + thname + " to logic syntax")
           val ls = tls.translateTheory(theo)
           println(ls)
           println("compiling pseudo-code")
         
           val dir : String = "/home/aivaras/Hets-src/"
         
       	   val lw = new LogicWriter(ls)
       	   println("writing to files in " + dir + thname)
         
           lw.compile(ls, thname, dir)
         
                  
           println("new logic read and compiled")
           controller.handleLine("exit")
         }
         // ========================================================
         else if (argl contains "-readSpec") {        	
           // parse logic spec
           println("reading logic specification")
           val source = scala.io.Source.fromFile(new java.io.File(filename),"UTF-8")       
           val (doc, err) = controller.read(File(new java.io.File(filename)), None) 
             //controller.textReader.readDocument(source, DPath(uri))(controller.termParser.apply(_))
           source.close()           
           if (!err.isEmpty) {
        	   println("errors while reading " + filename + " encountered:")
        	   err map println
        	   return
           } else {
             println("test")
             println("no errors while reading")
             println(doc.toString())
             val th = doc.getModulesResolved(controller.localLookup) foreach {
             	case t:DeclaredTheory => t
             }
             println(th.toString())
           }
           println("..from file " + filename)
           
         } else {
           
         }
         
         
       } // <------------ end of main
      catch {
        case e : java.lang.ArrayIndexOutOfBoundsException => println("Error: array index out of bounds")
        													e.printStackTrace()
        													
        case e : java.lang.OutOfMemoryError => println("ran out of memory!") 
        										e.printStackTrace()
//
//        case e : FileNotFoundException => println("no such file: " + args(0) + ", check spelling")
//        case e : IOException => println("IO exception")
        case e : Throwable => println("unknown error:")
        				throw e
        				e.printStackTrace()
      }
       
     
     
   }
   
   def getCurrentDir : String = new java.io.File( "." ).getCanonicalPath()
}