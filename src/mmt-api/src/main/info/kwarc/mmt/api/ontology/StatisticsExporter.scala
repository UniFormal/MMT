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
  def exportDocument(doc: Document, bf: BuildTask): Unit = {
    val rep = controller.report
    val rs = controller.depstore
    log("[  -> statistics]     "+doc.path.toPath+" "+bf.outFile.toString())
    rh(rs.makeStatistics(doc.path).toJSON.toString)
    var root = bf.archive.rootString
    val filename : String = (root+"/export/statistics/description_of_statistics_keys.json")
    log(filename)
    val usageFile = File(filename)
    utils.File.write(usageFile, rs.statDescription.toString())
  }

  /**
    * Get the statistic for the theory convert it to json and write it to the respective file in export
    * @param doc the theory to make the statistics for
    * @param bf the build task
    */
  def exportTheory(thy: Theory, bf: BuildTask): Unit = {
    val rep = controller.report
    val rs = controller.depstore
    rh(rs.makeStatistics(thy.path).toJSON.toString)
  }
  def exportView(view: View, bf: BuildTask): Unit =  {}
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]): Unit = {}
}
