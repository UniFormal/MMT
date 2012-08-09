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
//import utils.MyList

import scala.sys.process._

//TODO 
/*	should be able to read in twelf files
 * twelf++ (typecheck and pattern match) the read files
 * 
 * read in pattern instance specification file (we have a logic + patterns already)
 * check the specifications, give sensible error messages if does not pass
 * 
 */

object Main {
  
   val base = "http://latin.omdoc.org/logics/syntax"
  
   def main(args: Array[String]) {
     
     if (args.length == 0) {
    	 println("No arguments given. Expecting filepath argument.\nTerminating.")
    	 return
     } 
     
       try {         
         val filename = args(0) // take first argument - file name of the source
         val controller = new frontend.Controller
         val uri = new utils.URI(None,None,List(base + "?" + filename))
         
         val source = scala.io.Source.fromFile(new java.io.File(filename),"UTF-8")       
         val (doc, err) = controller.textReader.readDocument(source, DPath(uri),"")
         source.close()
//         println("doc: ")      
         val fileout = "/home/aivaras/Desktop/Hets/hets-mmt-output.txt"
           val output = controller.library.toString()
           println(output)
//         "echo" + " " + output #> new java.io.File(fileout) ! 
//         val items = doc.getItems
//         println(controller.globalLookup.getTheory(DPath(uri) ? "Logic") )
//         println(controller.library.toString())
//         val patt = controller.localLookup.getPattern(controller.library.getAllPaths.head ? "dqq" )
//         println("printing patter:")
//         println(patt) 
//         val t = controller.get(controller.library.getAllPaths.head)
//         println(t)
//         
////         val pc = new PatternChecker(controller)
////         val home = OMMOD(controller.library.getAllPaths.head)
////         println(pc.getPatterns(home)(2))
//         
//         println(t.components.map(x => x.getClass()))
//         val comps = t.components.filter(x => x match {
//           case x : Constant => true
//           case _ => false
//         })                 
//         comps.map(x => {println(x.getClass()); println(x)})
//         
//         val translator = new Translator(controller)
//         val q = translator.translate(comps.head)
//         val res = comps.map(x => {
//           val q =  translator.translate(x)
//           q
//         })
//         res.map(x => {println(x.getClass()); println(x)})
//         
         
         if (! err.isEmpty) {
                    println("errors occured: ")
                    println(err.toString())
                    return
         }

         

//         translator.translate(controller.get(controller.library.getAllPaths.head))
         
         //TODO give .hs data structures
         // via info.kwarc.mmt.lf.compile ?
         // translate to data structures?
//         val exp  = Haskell.exp(ID("name"))
//         println(exp)
         
       
       } // <------------ end of main
      catch {
        case e : java.lang.ArrayIndexOutOfBoundsException => println("Error: array index out of bounds")
        													e.printStackTrace()
        													
        case e : java.lang.OutOfMemoryError => println("ran out of memory!") 
        										e.printStackTrace()

        case e : FileNotFoundException => println("no such file: " + args(0) + ", check spelling")
        case e : IOException => println("IO exception")
//        case e => println("unknown error:")
//        				throw e
//        				e.printStackTrace()
      }
       
     
     
   }
}