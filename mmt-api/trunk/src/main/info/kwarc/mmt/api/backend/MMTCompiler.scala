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
  
  override def parallel = false
     
  def includeFile(n: String) : Boolean = n.endsWith(".mmt")// || n.endsWith(".elf")
    
  def importDocument(bf: archives.BuildTask, seCont: documents.Document => Unit) {
     val name = File(bf.inPath.last).setExtension("omdoc").segments.last // set extension to omdoc (hacky)
     val dpath = DPath(bf.base / bf.inPath.init / name) // bf.narrationDPath except for extension
     val doc = controller.read(bf.inFile, Some(dpath))(bf.errorCont)
     controller.checker(doc)(bf.errorCont, checking.RelationHandler.ignore)
     seCont(doc)
  }
}
