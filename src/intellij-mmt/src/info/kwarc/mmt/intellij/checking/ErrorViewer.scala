package info.kwarc.mmt.intellij.checking

import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.documents.{Document, MRef}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.File

import scala.collection.mutable

class ErrorViewer(controller : Controller) {
  private val docs : mutable.HashMap[File,List[Path]] = mutable.HashMap.empty
  def finish(file : File, doc : Document) = {
    val ls = doc.path :: doc.getDeclarations.collect {
      case r : MRef => r.target
    }
    docs.update(file,ls)
  }
  def clearFile(file : String) = docs.get(File(file)).foreach(_.foreach(controller.delete))
}