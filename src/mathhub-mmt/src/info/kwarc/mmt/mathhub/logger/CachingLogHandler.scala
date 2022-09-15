package info.kwarc.mmt.mathhub.logger

import info.kwarc.mmt.api.frontend.ReportHandler
import info.kwarc.mmt.api.utils._

import scala.collection.mutable

import java.util.UUID

/** represents a report handler that caches all messages */
class CachingReportHandler(override val id: String, val maxSize: Int = 10000) extends ReportHandler(id) {
  private val theCache = new mutable.Queue[LogEntry]()

  /** caches a new log entry*/
  override def apply(ind: Int, caller: => String, group: String, msgParts: List[String]): Unit = {
    val entry = LogEntry(
      UUID.randomUUID().toString,
      System.currentTimeMillis() / 1000L,
      ind,
      caller,
      group,
      msgParts
    )

    // only enqueue one element at the same time
    synchronized {
      theCache.enqueue(entry)
      while(theCache.length > maxSize){
        theCache.dequeue()
      }
    }
  }

  /** return all log entries */
  def all: List[LogEntry] = theCache.toList

  /** return all log entries after a given uuid */
  def after(uuid: String): List[LogEntry] = {
    val index = theCache.indexWhere(_.uuid == uuid)
    if(index > -1) {
      theCache.drop(index + 1).toList
    } else {
      theCache.toList
    }
  }
}

/**
  * a single log entry
  *
  * @param uuid The UUID of this entry
  * @param time Time when the log entry was added
  * @param indent Indent level of the log entry
  * @param caller The name of the caller of the function
  * @param prefix prefix of the log entry
  * @param parts \n separated parts of the message
  */
case class LogEntry(uuid: String, time: Long, indent: Int, caller: String, prefix: String, parts: List[String]) {
  def toJSON: JSON = JSONObject(
    "uuid" -> JSONString(uuid),
    "time" -> JSONInt(time),

    "caller" -> JSONString(caller),
    "indent" -> JSONInt(indent),

    "prefix" -> JSONString(prefix),
    "parts" -> JSONArray(parts.map(JSONString):_*)
  )
}