package info.kwarc.mmt.api.ontology
import info.kwarc.mmt.api._
import utils._
import documents._
import modules._
import archives._
import java.util.ResourceBundle.Control
import info.kwarc.mmt.api.frontend.Controller

class StatisticsExporter extends Exporter {
  def key = "statistics"

  override def outExt = "json"

  /**
    * Get the statistic for the document convert it to json and write it to the respective file in export
    * @param doc the document to make the statistic for
    * @param bf the build task
    */
  def exportDocument(doc: Document, bf: BuildTask) {
    val rep = controller.report
    val rs = controller.depstore
    log("[  -> statistics]     "+doc.path.toPath+" "+bf.outFile.toString())
    // Ugly workaround
    // TODO: Fix the problem:
    // (the output of typedRelationalExtractor is written to content, but the statistic is made only from relational/narration
    val stat = rs.makeStatistics(doc.path, controller)
    val jsar  = stat.entries.map {case (dec, n) => JSONObject(dec.getDescription -> JSONInt(n))}
    val j = JSONArray.fromList(jsar)
    rh(j.toString)
  }

  def exportTheory(thy: DeclaredTheory, bf: BuildTask) {
    /* //Maybe useful later, to extend functionality for web interface
    val rep = controller.report
    val rs = controller.depstore
    val stat = rs.makeStatistics(thy.path, controller)
    val jsar  = stat.entries.map {case (dec, n) => JSONObject(dec.getDescription -> JSONInt(n))}
    val j = JSONArray.fromList(jsar)
    rh(j.toString)*/
  }
  def exportView(view: DeclaredView, bf: BuildTask)  {}
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {}

}
