package info.kwarc.mmt.api.archives.lmh.MathHub

import info.kwarc.mmt.api.utils.{JSON, JSONArray, JSONObject, URI}

import scala.collection.mutable.ListBuffer
import scala.util.Try

/** implements local and remote listing functionality in MathHub */
trait MathHubLister {
  self: MathHub =>

  /** find all the archives known to the controller */
  def entries_ : List[MathHubEntry] = controller.backend.getArchives.flatMap(a => getEntry(a.root))

  /** tries to get some json form a given URL */
  private def get_json(url: URI) : Option[JSON] = {
    log(s"fetching $url")
    val attempt = Try(io.Source.fromURL(url.toString))
    if (attempt.isFailure) None else Some(attempt.get.toBuffer.mkString).map(JSON.parse)
  }

  private def getAvailablePage(page : Int) : List[String] = {
    get_json(api_(page)).map(a => a.asInstanceOf[JSONArray].values.map( e => {
      e.asInstanceOf[JSONObject].getAsString("path_with_namespace")
    }).toList).getOrElse(Nil)
  }

  /** return a list of available pages, at most 10 */
  protected def available_() : List[String] = Try({
    val buffer = new ListBuffer[String]

    var i = 1

    while(i <= 10 && {
      val newEntries = getAvailablePage(i)
      buffer ++= newEntries

      i += 1

      newEntries.nonEmpty
    }) {}

    buffer.toList.distinct.sorted
  }).getOrElse(Nil)
}
