/**
 * This is a logic specification file reading wrapper:
 * Main -> filename.hets -> SpecReader(filename) -> mmt.backend.TextReader -> Logic in internal MMT language
 * 
 * created by Aivaras:
 * a.jakubauskas@jacobs-university.de
*/
package info.kwarc.mmt.hets

import info.kwarc.mmt.api._
import utils.File
import info.kwarc.mmt.api.backend._
import frontend._


/*	SpecReader - logic instance specification reader
 * reads and parses a logic spec. in MMT format
 * 
 *  should be a TextReader + %spec declaration reader
 */
//TODO implementation
class SpecReader {

/** ------------------------------------- private vars -------------------- */  
  /** array containing all the lines in the file */
  private var lines : Array[String] = null

  /** all the lines in a single string, with " " instead of newline. */
  private var flat  : String = ""  
      
  // ??
  private var file : File = new File(new java.io.File("")) 
  
  private var filename : String = ""
    
  
  
  /** points to a file */  
  def apply(fname : String) = {
    file = new File (new java.io.File(fname)) 
    filename = fname
  }
   
  /** reads the specification file */
  def read(controller : Controller, namespaceURI : utils.URI) : (documents.Document,scala.collection.mutable.LinkedList[SourceError]) = {    
//    val source : scala.io.Source = scala.io.Source.fromFile(new java.io.File(filename),"UTF-8")  
//	lines = source.getLines.toArray
    
    val source = scala.io.Source.fromFile(new java.io.File(filename),"UTF-8")
    val (doc, err) = controller.textReader.readDocument(source, DPath(namespaceURI))(controller.termParser.apply(_))
    source.close()
    (doc,err)
  }
  
  def read(fn : String) : Unit = {
    filename = fn
//    this.read()
  }
  
}

object SpecTest {
  
	val latinbase = "http://latin.omdoc.org/logics/syntax" // latin / logic atlas
	val foundational = "http://cds.omdoc.org/foundational" // MMT / theories
  
	def main(args : Array[String]) {
	  val flag = args(0)
	  val filename = args(1)
	  val uri = new utils.URI(None,None,List(foundational + "?" + filename))
	  val path = "/home/aivaras/TPTP/MMT/theories/source/pl_spec.mmt"
	  val spc = new SpecReader()
	  val cont = new Controller
	  spc(path)
	  val (doc,err) = spc.read(cont,uri)
	  println(err)
	  
	  println("\ndone")
	}
  
  
} 