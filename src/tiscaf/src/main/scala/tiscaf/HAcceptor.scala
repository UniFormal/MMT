/*******************************************************************************
 * This file is part of tiscaf.
 * 
 * tiscaf is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Foobar is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with tiscaf.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package tiscaf

import scala.collection.{ mutable => mute }

import scala.concurrent._

private object HReqState extends Enumeration {
  val WaitsForHeader, WaitsForData, WaitsForPart, WaitsForOctets, IsInvalid, IsReady = Value
}

private class HConnData {
  self =>

  import scala.collection.{ mutable => mute }
  var reqState: HReqState.Value = HReqState.WaitsForHeader
  var tail: Array[Byte] = new Array[Byte](0)
  var acceptedTotalLength: Long = 0L
  var header: Option[HReqHeader] = None
  var parMap: mute.Map[String, Seq[String]] = new mute.HashMap[String, Seq[String]]()
  var appLet: Option[(HApp, HLet)] = None
  var octetStream: Option[Array[Byte]] = None
  var parts: Option[HPartData] = None
  var headers: Map[String, String] = Map()

  def toTalkData(aWriter: HWriter): HTalkData = new HTalkData {
    def header = self.header.get
    def parMap = Map[String, Seq[String]]() ++ self.parMap
    def app = self.appLet.get._1
    def octets = self.octetStream
    def writer = aWriter
    def headers = self.headers
  }

  def reset: Unit = {
    reqState = HReqState.WaitsForHeader
    tail = new Array[Byte](0)
    acceptedTotalLength = 0L
    header = None
    parMap.clear
    appLet = None
    octetStream = None
    parts = None
    headers = Map()
  }
}

private trait HTalkData {
  def header: HReqHeader
  def parMap: Map[String, Seq[String]]
  def app: HApp
  def octets: Option[Array[Byte]]
  def writer: HWriter
  def headers: Map[String, String]

  def aliveReq = try { header.isPersistent } catch { case _: Exception => false }
}

private class HAcceptor(
    val writer: HWriter,
    apps: Seq[HApp],
    connectionTimeout: Int,
    logger: HLoggable,
    maxPostDataLength: Int,
    headers: Map[String, String])(
      implicit executionContext: ExecutionContext) extends HLoggable {

  val in = new HConnData

  // set default headers
  in.headers = headers

  // HLoggable API
  def error(msg: String, t: Throwable) = logger.error(msg, t)
  def warning(msg: String) = logger.warning(msg)
  def info(msg: String) = logger.info(msg)

  def accept(bytes: Array[Byte]): Unit = {
    in.tail = in.tail ++ bytes

    in.reqState match {
      case HReqState.WaitsForHeader => inHeader
      case HReqState.WaitsForData   => inData
      case HReqState.WaitsForPart   => inParts
      case HReqState.WaitsForOctets => inOctets
      case x                        => // don't change
    }
  }

  private def maxHeaderLength = 8192 // ANLI to config?

  private def inHeader: Unit = {
    val till = in.tail.length - 4
    def findEol(start: Int): Option[Int] = if (till < start) None else (start to till).find { i =>
      in.tail(i) == 13 &&
        in.tail(i + 1) == 10 &&
        in.tail(i + 2) == 13 &&
        in.tail(i + 3) == 10
    }
    // at least lines with method is expected: 'GET / HTTP/1.1' length is 14
    findEol(14) match {
      case None => in.reqState = HReqState.WaitsForHeader
      case Some(shift) =>
        if (shift >= maxHeaderLength) {
          warning("Header too long")
          in.reqState = HReqState.IsInvalid
        } else {
          in.header = Some(new HReqHeader(new String(in.tail.take(shift), "ISO-8859-1") split ("\r\n")))
          in.tail = in.tail.slice(shift + 4, in.tail.length)

          in.header.get.reqType match {
            case HReqType.Get        => parseParams(in.header.get.query); in.reqState = HReqState.IsReady
            case HReqType.Delete     => parseParams(in.header.get.query); in.reqState = HReqState.IsReady
            case HReqType.Options    => parseParams(in.header.get.query); in.reqState = HReqState.IsReady
            case HReqType.Head       => parseParams(in.header.get.query); in.reqState = HReqState.IsReady
            case HReqType.PostData   => parseParams(in.header.get.query); inData
            case HReqType.PostOctets => parseParams(in.header.get.query); inOctets
            case HReqType.PostMulti  => parseParams(in.header.get.query); inParts
            case HReqType.Put        => parseParams(in.header.get.query); inOctets
            case HReqType.Patch      => parseParams(in.header.get.query); inOctets
            case HReqType.Invalid    => in.reqState = HReqState.IsInvalid
          }
      }
    }
  }

  //post, form data
  private def inData: Unit = {
    val length = in.header.get.contentLength.get.toInt
    if (length > maxPostDataLength) {
      warning("Post data is too big, you may want to increase `HServer.maxPostDataLength`")
      in.reqState = HReqState.IsInvalid
    } else if (length > in.tail.length) {
      in.reqState = HReqState.WaitsForData
    } else {
      in.reqState = HReqState.IsReady
      parseParams(new String(in.tail.take(length), "ISO-8859-1"))
    }
  }

  // post, unknown content type, falling back to octet mode, put, patch
  private def inOctets: Unit = {
    val contentLength = in.header.get.contentLength.get.toInt
    if (contentLength + in.tail.length > maxPostDataLength) {
      warning("Request data is too big, you may want to increase `HServer.maxPostDataLength`")
      in.reqState = HReqState.IsInvalid
    } else {
      in.octetStream match {
        case None         => in.octetStream = Some(in.tail.take(in.tail.length))
        case Some(octets) => in.octetStream = Some(octets ++ in.tail.take(in.tail.length))
      }
      in.tail = new Array[Byte](0)
      if (in.octetStream.get.length == contentLength) in.reqState = HReqState.IsReady
      else in.reqState = HReqState.WaitsForOctets
    }
  }

  //---------------- post, multipart
  private def inParts: Unit = {
    resolveAppLet
    in.appLet.get._2.partsAcceptor(in.header.get) match {
      case None =>
        warning("HLet does not define any parts acceptor, and thus cannot accept multipart post request")
        in.reqState = HReqState.IsInvalid
      case Some(acceptor) =>
        if (in.parts.isEmpty) {
          // at this point tail must have at least --boundary\r\n... or wait for bytes further
          val header = in.header.get
          def minSize = HParts.toBytes("--" + in.header.get.boundary.get + "\r\n").length
          if (in.tail.length >= minSize)
            in.parts = Some(new HPartData(acceptor, in.header.get.boundary.get))
        }

        if (in.parts.isEmpty) in.reqState = HReqState.WaitsForPart
        else {
          in.parts.get.tail = in.parts.get.tail ++ in.tail
          in.tail = new Array[Byte](0) // HMulti consumes all the tail
          HMulti.process(in.parts.get) match {
            case HReqState.IsInvalid =>
              warning("Unable to process parts")
              in.reqState = HReqState.IsInvalid
              in.parts = None
            case x => // HReqState.IsReady or HReqState.WaitsForPart
              if (in.acceptedTotalLength + in.tail.length > in.header.get.contentLength.get) {
                warning("Total read parts length was greater than header specified Content-Length")
                in.reqState = HReqState.IsInvalid
              } else {
                in.reqState = x
              }
          }
        }
    }
  }

  // used both for query string and post data
  private def parseParams(s: String): Unit = {
    def newPairs = s.split("&")
      .filter(_.length >= 2)
      .map(_.split("="))
      .filter(is => is.length > 0 && is.size > 0)
      .map { p => if (p.size == 1) (p(0), "") else (p(0), p(1)) }

    for (newPair <- newPairs) yield in.parMap.get(newPair._1) match {
      case None         => in.parMap(newPair._1) = Seq(newPair._2)
      case Some(oldSeq) => in.parMap(newPair._1) = oldSeq :+ newPair._2
    }
  }

  def talk: Future[PeerWant.Value] = {
    val (app, thelet) = in.appLet.get
    val tk = new HTalk(in.toTalkData(writer))

    val f = if ((app.tracking != HTracking.NotAllowed) &&
      (HSessMonitor.count.get(app) >= app.maxSessionsCount)) {
      warning("Too many open sessions. You may want to increase `HApp.maxSessionsCount`")
      try {
        new tiscaf.let.ErrLet(HStatus.ServiceUnavailable, "too many sessions") aact (tk)
      } catch {
        case e: Exception =>
          Future.successful {
            error("A problem occurred while notifying client", e)
          }
      }
    } else {
      if (app.keepAlive && in.header.get.isPersistent) {
        tk.setHeader("Connection", "keep-alive").setHeader("Keep-Alive", "timeout=" + connectionTimeout + ", max=100")
      } else {
        tk.setHeader("Connection", "close").removeHeader("Keep-Alive")
      }

      thelet aact (tk) recoverWith {
        case e: Exception =>
          error("An error occurred while executing the HLet", e) // reporting HLet errors
          try {
            new let.ErrLet(HStatus.InternalServerError) aact (tk)
          } // connection can be closed here...
          catch {
            case _: Exception => // ...and "header is already sent" will be arised; don't report it.
              Future.successful(())
          }
      }
    }

    f map (_ => tk.close)

  }

  def resolveAppLet: Unit = in.appLet match {
    case None =>
      val req = HReqData(in.toTalkData(writer))
      in.appLet = Some(HResolver.resolve(apps, req))
    case _ =>
  }
}

