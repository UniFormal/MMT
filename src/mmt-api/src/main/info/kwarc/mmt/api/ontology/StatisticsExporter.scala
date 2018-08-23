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
    * @param doc the document to make the statistics for
    * @param bf the build task
    */  
  def exportDocument(doc: Document, bf: BuildTask) {
    val rep = controller.report
    val rs = controller.depstore
    log("[  -> statistics]     "+doc.path.toPath+" "+bf.outFile.toString())
    // Ugly workaround
    // TODO: Fix the problem:
    // (the output of typedRelationalExtractor is written to content, but the statistic is made only from relational/narration
    val stat = rs.makeStatistics(doc.path)
    def jstatmap(substat:List[(StatisticEntries, Int)]) = JSONArray.fromList(substat map {case (dec, n) => JSONObject(dec.getKey -> JSONInt(n))})
    val jstat = stat.entries map {case (pre, substat) => JSONObject(pre.getKey -> jstatmap(substat))}
    val j = JSONArray.fromList(jstat)
    rh(j.toString)
  }

  /**
    * Get the statistic for the theory convert it to json and write it to the respective file in export
    * @param doc the theory to make the statistics for
    * @param bf the build task
    */
  def exportTheory(thy: DeclaredTheory, bf: BuildTask) {
    val rep = controller.report
    val rs = controller.depstore
    val stat = rs.makeStatistics(thy.path)
    def jstatmap(substat:List[(StatisticEntries, Int)]) = JSONArray.fromList(substat map {case (dec, n) => JSONObject(dec.getKey -> JSONInt(n))})
    val jstat = stat.entries map {case (pre, substat) => JSONObject(pre.getKey -> jstatmap(substat))}
    val j = JSONArray.fromList(jstat)
    rh(j.toString)
  }
  def exportView(view: DeclaredView, bf: BuildTask)  {}
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {}

}
