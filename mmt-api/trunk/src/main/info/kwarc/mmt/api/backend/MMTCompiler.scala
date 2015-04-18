package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._
import frontend._
import utils._
import checking._

/* obsolete use the interpreter
/** a Compiler that uses the generic MMT functions to parse and check a file
 *  
 *  the produced OMDoc is already validated
 */
class MMTCompiler extends archives.Importer {
  val key = "mmt-omdoc"
  
  override def parallel = false
     
  def includeFile(n: String) : Boolean = n.endsWith(".mmt")
    
  def importDocument(bf: archives.BuildTask, index: documents.Document => Unit) {
     val inPathOMDoc = archives.ArchivePath(bf.inPath).setExtension("omdoc").segments
     val dpath = DPath(bf.base / inPathOMDoc) // bf.narrationDPath except for extension
     val ps = new parser.ParsingStream(bf.base / bf.inPath, dpath, bf.archive.namespaceMap, "mmt", File.Reader(bf.inFile))
     val doc = controller.read(ps, false)(bf.errorCont)
     controller.checker(doc)(new CheckingEnvironment(bf.errorCont, RelationHandler.ignore))
     index(doc)
  }
}
*/