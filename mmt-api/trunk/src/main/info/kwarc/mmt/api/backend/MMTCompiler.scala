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
class MMTCompiler extends Compiler {
  val key = "mmt-omdoc"

  def includeFile(n: String) : Boolean = n.endsWith(".mmt") || n.endsWith(".elf")
    
  def buildOne(bf: archives.BuiltFile) = {
    val (doc, errorList) = controller.read(bf.inFile, Some(bf.dpath))
    bf.errors = errorList
    val invErrors = controller.checker(doc)
    val outFile = bf.outFile.setExtension("omdoc")
    File.write(outFile, doc.toNodeResolved(controller.localLookup).toString)
  }
}
