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
import backend._
import utils.File._ //maybe the stuff here is useful

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


object SpecTest {
  
   def main(args: Array[String]) {
     
     if (args.length == 0) {
    	 println("No arguments given. Expecting 'flag filepath' [-outDir filepath] arguments.\nTerminating.")
    	 return
     } 
     try {      
    	 val argl = args.toList
    	 val controller = new frontend.Controller
         controller.handleLine("file startup.msl")
//    	 if ( argl.length == 1) {
//           controller.handleLine(argl.head)
//         }
    	 
         val flag = args(0)
         println("flag: " + flag)
         
         val filename = args(1) // take first argument - file name of the source
         var outDir = "../../../test/source/hets-test"
         if (args.contains("-outDir")) { outDir = args(args.length - 1) }                    
         
         //TODO make global
         
         
         val fl = File(new java.io.File(filename))
         if (!(new java.io.File(filename).exists())) {
           println("File " + filename + " does not exist!")
           return
         }

         
         
         if (argl contains "-readSpec") {        	
           // parse logic spec
           println("reading logic specification")
           println("\t\t..from file " + filename)
           val (doc, err) = controller.read(fl, None)
           if (!err.isEmpty) {
        	   println("errors while reading " + filename + " encountered:")
        	   err map println
        	   return
           } else {
             println("no errors while reading")
             val q = {doc.getModulesResolved(controller.localLookup) mapPartial {
               case t:DeclaredTheory => Some(t)
               case _ => None
             } }.head   
             val xmlExp = new Exporter(controller) // export to XML
             xmlExp.compile(File(outDir),q)
           }
         } else if (argl contains "-t") {
           println(".... test test ....")
           val (doc, err) = controller.read(fl,None)
           if (!err.isEmpty) {
        	   println("errors while reading " + filename + " encountered:")
        	   err map println
        	   return
           } else {
             println("spec test")
             println("no errors while reading")
             println(doc.toString())
           }
           val th = doc.getModulesResolved(controller.localLookup) foreach {
             case t:DeclaredTheory => t
           }
           println(th.toString())
         }
           
       } // <------------ end of main
       
      catch {
        case e : java.lang.ArrayIndexOutOfBoundsException => println("Error: array index out of bounds")
        													e.printStackTrace()
        case e : Throwable => println("unknown error at SpecTest:"); throw e
      }
       
     
     
   }
}