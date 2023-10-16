package info.kwarc.mmt.odk.GAP
import info.kwarc.mmt.api._
import archives._
import frontend._
import documents._
import info.kwarc.mmt.api.ontology.{RelationalElement, ULOStatement}

class GAPImporter extends Importer {
  val key = "gap-omdoc"
  def inExts = List("xml", "json")
  override def inDim = RedirectableDimension("gap")

  // val docImporter = new GAPDocImporter
  val sysImporter = new GAPJSONImporter
  override val logPrefix = "gapimporter"

  override def init(controller : Controller): Unit = {
    this.controller = controller
    report = controller.report
    // initOther(docImporter)
    initOther(sysImporter)
  }

  def importDocument(bt : BuildTask, index : Document => Unit,rel:ULOStatement => Unit) : BuildResult = {
    try {
      val ext = bt.inFile.getExtension.get //should always be set considering inExts above
      if (sysImporter.inExts.contains(ext)) {
        sysImporter.importDocument(bt, index,rel)
      } /* else if (docImporter.inExts.contains(ext)) {
        docImporter.importDocument(bt, index)
      } else {
        throw new ImplementationError("unexpected extension " + ext)
      } */ else BuildResult.empty
    } catch {
      case e : Exception => throw new ImplementationError("unexpected error in Gap Importer: " + e.getMessage)
    }
  }

}
