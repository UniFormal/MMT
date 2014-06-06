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
    
  def importDocument(bf: archives.BuildTask, seCont: documents.Document => Unit) {
    val doc = controller.read(bf.inFile, Some(bf.narrationDPath))(bf.errorCont)
    val invErrors = controller.checker(doc)(bf.errorCont, checking.RelationHandler.ignore)
    seCont(doc)
  }
}
