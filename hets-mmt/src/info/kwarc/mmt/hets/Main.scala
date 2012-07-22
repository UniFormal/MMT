package info.kwarc.mmt.hets


import info.kwarc.mmt.api._
import utils.File._ //maybe the stuff here is useful
import backend.TextReader // the parser

import scala.util.control.Exception._

// for exception handling
import java.io.FileNotFoundException
import java.io.IOException


//TODO 
/*	should be able to read in twelf files
 * twelf++ (typecheck and pattern match) the read files
 * 
 * read in pattern instance specification file (we have a logic + patterns already)
 * check the specifications, give sensible error messages if does not pass
 * 
 */

object Main {
  
   def main(args: Array[String]) {
     
     if (args.length == 0) {
    	 println("No arguments given. \n Terminating.")
     } else {
       try {         
         val filename = args(0) // take first argument - file name of the source
         val controller = new frontend.Controller
         val uri = new utils.URI(None,None,List(filename))
         
         val source = scala.io.Source.fromFile(new java.io.File(filename),"UTF-8")       
         val (doc, err) = controller.textReader.readDocument(source, DPath(uri))
         source.close()
         println("doc: ")
         println(controller.globalLookup.getTheory(DPath(uri) ? "Base") )
         //TODO 
         /* for starters: give document back to hets
          * give data structures? back to hets?
          */
         if (! err.isEmpty) {
                    println("errors occured: ")
                    println(err.toString())
         }
         
       
       
       }
      catch {
        case e : java.lang.ArrayIndexOutOfBoundsException => println("Error: array index out of bounds")
        													e.printStackTrace()
        													
        case e : java.lang.OutOfMemoryError => println("ran out of memory!") 
        										e.printStackTrace()

        case e : FileNotFoundException => println("no such file: " + args(0) + ", check spelling")
        case e : IOException => println("IO exception")
        case e => println("unknown error:")
        				e.printStackTrace()
      }
       
     }
     
   }
}