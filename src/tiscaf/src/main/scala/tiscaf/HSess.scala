/** *****************************************************************************
 *  This file is part of tiscaf.
 *
 *  tiscaf is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  Foobar is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with tiscaf.  If not, see <http://www.gnu.org/licenses/>.
 *  ****************************************************************************
 */
package tiscaf

import scala.collection.{ mutable => mute }

import sync.Sync

import scala.collection.JavaConverters._

// backend map is SynchronizedMap
final private class HSess(data: HTalkData, resp: HResponse) extends mute.Map[Any, Any] {

  // scala.collection.mutable.Map
  def get(key: Any): Option[Any] = sidMap._2.get(key)
  def iterator: Iterator[(Any, Any)] = sidMap._2.iterator
  def +=(kv: (Any, Any)): this.type = { sidMap._2 += kv; this }
  def -=(key: Any): this.type = { sidMap._2 -= key; this }
  override def size: Int = sidMap._2.size
  override def clear: Unit = sidMap._2.clear
  override def foreach[U](f: Tuple2[Any, Any] => U): Unit = sidMap._2.foreach { f(_) }

  // delegating here from HTalk user API
  def sid = sidMap._1
  def isValid = HSessMonitor.isValid(sidMap._1)
  def invalidate = HSessMonitor.invalidate(sidMap._1)

  // SPI
  def writeCookie: Unit = if (data.app.tracking == HTracking.Cookie)
    resp.setHeader("Set-Cookie", data.app.cookieKey + "=" + sid + "; path=/")

  def restamp = HSessMonitor.restamp(sidMap._1)

  //-------------------- internals ---------------------------------------------

  private lazy val sidMap: (String, mute.Map[Any, Any]) = extractSid match {
    case None => HSessMonitor.create(data.app)
    case Some(aSid) => HSessMonitor.bag(aSid) match { // extracted sid may be invalidated already
      case None      => HSessMonitor.create(data.app)
      case Some(bag) => (aSid, bag)
    }
  }

  // uri has higher priority rather cookie
  private def extractSid: Option[String] = data.app.tracking match {
    case HTracking.Uri    => extractSidFromUri
    case HTracking.Cookie => extractSidFromCookie
    case _                => None
  }

  private def extractSidFromUri: Option[String] = data.header.uriExt.flatMap { x =>
    val parts = x.split("=").map(_.trim)
    if (parts.length == 2 && parts(0) == data.app.sidKey && HSessMonitor.sidSyntaxIsValid(parts(1))) Some(parts(1))
    else None
  }

  private def extractSidFromCookie: Option[String] = data.header.header("cookie").flatMap { x =>
    val sids = for {
      pair <- x.split(";")
      parts = pair.split("=").map(_.trim)
      if (parts.size == 2) && (parts(0) == data.app.cookieKey) && HSessMonitor.sidSyntaxIsValid(parts(1))
    } yield parts(1).trim

    if (sids.length > 0) Some(sids(0)) else None // browser can keep few cookies with the same NAME in
    // NAME=VALUE pairs; take first one
  }

}

private object HSessMonitor {

  def create(app: HApp): (String, mute.Map[Any, Any]) = {
    val aSid = newSid
    val newSess = Sess(now, app, new java.util.concurrent.ConcurrentHashMap[Any, Any].asScala)
    bags(aSid) = newSess
    count.inc(app)
    (aSid, newSess.bag)
  }

  def bag(sid: String): Option[mute.Map[Any, Any]] = for (b <- bags.get(sid)) yield b.bag

  def restamp(sid: String): Unit = for (b <- bags.get(sid)) yield bags(sid) = b.restamp

  def sidSyntaxIsValid(sid: String): Boolean = if (idLength != sid.length) false else sid.toSet subsetOf symbols.toSet
  def isValid(sid: String): Boolean = bags.get(sid).isDefined

  def invalidate(sid: String): Unit = bags.get(sid) match {
    case None =>
    case Some(sess) =>
      count.dec(sess.app)
      Sync.spawn {
        sess.app.onSessionInvalidate(sid, sess.bag.toMap)
      }
      bags -= sid
  }

  object count {

    def inc(app: HApp): Unit = withCounts(app) { a => counts(a) = counts(a) + 1 }
    def dec(app: HApp): Unit = withCounts(app) { a => counts(a) = counts(a) - 1 }
    def get(app: HApp): Int = withCounts(app) { a => counts(a) }

    private val counts = new mute.HashMap[HApp, Int]
    private def withCounts[T](app: HApp)(f: HApp => T): T = counts.synchronized {
      if (counts.get(app).isEmpty) counts(app) = 0
      f(app)
    }
  }

  // internals
  private case class Sess(val stamp: Long, val app: HApp, val bag: mute.Map[Any, Any]) {
    def restamp = Sess(now, app, bag)
  }

  private val bags = new java.util.concurrent.ConcurrentHashMap[String, Sess].asScala

  private val alpha = "abcdefghijklmnopqrstuvwxyz"
  private val symbols = alpha + alpha.toUpperCase + "0123456789"
  private val symLength = symbols.length
  private val idLength = 16
  private val random = new java.security.SecureRandom

  private def newSid: String = {
    val buf = new StringBuilder
    (1 to idLength).foreach(_ => buf.append(symbols.charAt(random.nextInt(symLength))))
    buf.toString
  }

  private def now = System.currentTimeMillis

  private def cleanExpired: Unit = bags.synchronized {
    for (sid <- bags.keySet) {
      val sess = bags(sid)
      if (sess.stamp < now - (sess.app.sessionTimeoutMinutes * 60000L)) invalidate(sid)
    }
  }

  // starting cleaning daemon
  new java.util.Timer(true).scheduleAtFixedRate(
    new java.util.TimerTask { def run { cleanExpired } },
    60000, 60000) // every minute
}
