package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import frontend._
import utils._
import parser._

import scala.collection.mutable.HashSet

/** a Compiler that uses the generic MMT functions to parse and check a file
 *  
 *  the produced OMDoc is already validated
 */
class MMTCompiler extends archives.Importer {
  val key = "mmt-omdoc"

  def includeFile(n: String) : Boolean = n.endsWith(".mmt") || n.endsWith(".elf")
    
  def buildOne(bf: archives.BuildFile, seCont: documents.Document => Unit) {
    val (doc, strucErrors) = controller.read(bf.inFile, Some(bf.dpath))
    val invErrors = controller.checker(doc)
    bf.errors = strucErrors ::: invErrors
    seCont(doc)
  }
}
