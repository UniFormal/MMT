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

trait HPartDescriptor {
  def header(key : String) : Option[String]
  def headerKeys : scala.collection.Set[String]
  override def toString = (for (k <- headerKeys) yield { k + " -> " + header(k).get }).mkString(", ")
}

// any returned 'false' disposes the connection, declineAll will not be 
// called - i.e. acceptor must do all cleanup work itself at 'false'

/** Accepts uploaded files with POST multiparts method. */
abstract class HPartsAcceptor(reqInfo : HReqHeaderData) {

  // to implement (callbacks)
  /** Opens the upload. Returns `false` to decline upload.
   *  If upload is not allowed, the connection is closed.
   */
  def open(desc : HPartDescriptor) : Boolean // new part starts with it...

  /** Accepts some bytes. Returns `false` to avoid more bytes to be accepted. */
  def accept(bytes : Array[Byte]) : Boolean // ... takes bytes (multiple calls!)...

  /** Closes the upload of the file.  */
  def close : Unit // ... and ends with this file...

  /** Called when all parts are declined. */
  def declineAll : Unit // ... or this one apeals to abort all parts

  // to override
  /** Header encoding. */
  def headerEncoding : String = "UTF-8"
}

final private class HPart(acceptor : HPartsAcceptor) {

  def takeHeader(strings : Seq[String]) : Boolean =
    if (strings.size == 0) false else {
      val header = strings.map(s => splitString(s)).filter(_.isDefined).map(_.get)
      if (header.size == 0) false else { // 'content-disposition' is expected at least
        desc.he ++= header
        acceptor.open(desc)
      }
    }

  def takeBytes(bytes : Array[Byte]) : Boolean = acceptor.accept(bytes)
  def close : Unit = acceptor.close
  def declineAll : Unit = acceptor.declineAll

  private def splitString(s : String) : Option[(String, String)] = {
    val idx = s.indexOf(':')
    if (idx < 1 && s.length > idx + 1) None else {
      val name = s.substring(0, idx).trim.toLowerCase
      val value = s.substring(idx + 1, s.length).trim
      Some((name, value))
    }
  }

  // the only implementation
  private val desc = new HPartDescriptor {
    val he = new scala.collection.mutable.HashMap[String, String]
    def header(key : String) = he.get(key.toLowerCase)
    def headerKeys = he.keySet
  }
}

/* ==== stream structure after request header's CLRF CLRF:
part0
...
partN
CLRF -- bdr -- CLRF // stream end marker
==== partX structure:
partHeader
bytes
==== partHeader structure
-- bdr CLRF 
header0 CLRF
... 
headerN CLRF 
CLRF
=== */

private object HPartState extends Enumeration {
  val AtPrefix, AtHeader, WaitsHeaderBytes, InData, WaitsDataBytes, IsReady, IsInvalid = Value
}

private class HPartData(val anAcceptor : HPartsAcceptor, val aBdrString : String) {
  var state : HPartState.Value = HPartState.AtPrefix
  var acceptor : HPartsAcceptor = anAcceptor
  var tail : Array[Byte] = HParts.toBytes("\r\n")
  var part : Option[HPart] = None
  var bdrString = aBdrString

  def resetPart : Unit = for (p <- part) { p.close; part = None }
}

// this object is created  for each multipart request

private object HMulti {

  // boundary aware constants
  private def prefix(bdr : String) = HParts.toBytes("\r\n--" + bdr)
  private def partStart(bdr : String) = HParts.toBytes("\r\n--" + bdr + "\r\n")
  private def endMark(bdr : String) = HParts.toBytes("\r\n--" + bdr + "--\r\n")

  // the only SPI
  def process(in : HPartData) : HReqState.Value = {

    // main loop: eat bytes as long as possible
    @scala.annotation.tailrec
    def doProcess : HPartState.Value = in.state match {
      case HPartState.AtPrefix => in.state = atPrefix(in); doProcess
      case HPartState.AtHeader => in.state = atHead(in); doProcess
      case HPartState.InData   => in.state = inData(in); doProcess
      case x                   => x
    }

    in.state = doProcess // till WaitsHeaderBytes, WaitsDataBytes, IsInvalid or IsReady is got

    // inform HAcceptor what to do further
    in.state match {
      case HPartState.WaitsHeaderBytes => in.state = HPartState.AtHeader; HReqState.WaitsForPart
      case HPartState.WaitsDataBytes   => in.state = HPartState.InData; HReqState.WaitsForPart
      case HPartState.IsInvalid        => for (p <- in.part) p.declineAll; in.part = None; HReqState.IsInvalid
      case HPartState.IsReady          => in.resetPart; HReqState.IsReady
    }
  }

  //------------------ internals --------------------

  private def atPrefix(in : HPartData) : HPartState.Value = {
    in.resetPart // for not-first part
    if (HParts.startsWith(in.tail, endMark(in.bdrString))) HPartState.IsReady
    else if (HParts.startsWith(in.tail, partStart(in.bdrString))) HPartState.AtHeader
    else HPartState.IsInvalid
  }

  private def atHead(in : HPartData) : HPartState.Value = {
    val till = in.tail.length - 4

    def findEol(idx : Int) : Option[Int] = (0 to till).find { idx =>
      in.tail(idx) == 13 &&
        in.tail(idx + 1) == 10 &&
        in.tail(idx + 2) == 13 &&
        in.tail(idx + 3) == 10
    }

    if (till < 0) HPartState.WaitsHeaderBytes else findEol(12) match {
      case None        => HPartState.WaitsHeaderBytes
      case Some(shift) => acceptHeader(in, shift)
    }
  }

  private def acceptHeader(in : HPartData, shift : Int) : HPartState.Value =
    if (shift >= HParts.maxHeaderLength) HPartState.IsInvalid else {
      val ar = in.tail.take(shift)
      in.tail = in.tail.slice(shift + 4, in.tail.length)
      def strings = HParts.asString(ar, in.acceptor.headerEncoding)
        .split("\r\n").map(_.trim).filter(_.length != 0).toSeq.tail
      in.part = Some(new HPart(in.acceptor))
      if (in.part.get.takeHeader(strings)) HPartState.InData
      else HPartState.IsInvalid
    }

  private def inData(in : HPartData) : HPartState.Value = {
    val eatTill = HParts.keepTailPosition(in.tail, prefix(in.bdrString))

    if (eatTill == 0) {
      if (in.tail.length >= prefix(in.bdrString).length) HPartState.AtPrefix
      else HPartState.WaitsDataBytes
    } else { // in data
      if (in.part.get.takeBytes(in.tail.take(eatTill))) {
        in.tail = in.tail.slice(eatTill, in.tail.length)
        if (in.tail.length >= prefix(in.bdrString).length) HPartState.AtPrefix
        else HPartState.WaitsDataBytes
      } else HPartState.IsInvalid
    }
  }
}

// mostly bytes processing oriented commands
private object HParts {

  def maxHeaderLength = 1024 // I don' see reasons to present it in configuration

  def toBytes(s : String) = s.getBytes("ISO-8859-1")
  def asString(a : Array[Byte], enc : String) : String = new String(a, enc)

  def keepTailPosition(in : Array[Byte], pattern : Array[Byte]) : Int = {

    val inLength = in.length
    val patLength = pattern.length
    val delta = inLength - patLength

    // tail length > pattern length
    def findFullMatch : Option[Int] = {

      val patRange = 0 until patLength

      @scala.annotation.tailrec
      def tryOffset(idx : Int) : Option[Int] = if (idx > delta) None else {
        patRange.find { i => pattern(i) != in(idx + i) } match {
          case None => Some(idx)
          case _    => tryOffset(idx + 1)
        }
      }
      tryOffset(0)
    }

    // tail length <= pattern length
    def findPartialMatch(tail : Array[Byte]) : Int = {
      val tailLength = tail.length

      @scala.annotation.tailrec
      def tryOffset(idx : Int) : Int = if (idx == tailLength) idx else {
        (0 until (tailLength - idx)).find { i => tail(idx + i) != pattern(i) } match {
          case None => idx
          case _    => tryOffset(idx + 1)
        }
      }
      tryOffset(0)
    }

    if (delta > 0) findFullMatch match {
      case Some(idx) => idx
      case None =>
        val tail = in.slice(delta + 1, inLength)
        findPartialMatch(tail) + delta + 1
    }
    else findPartialMatch(in)
  }

  def hasFragment(in : Array[Byte], offset : Int, pattern : Array[Byte]) : Boolean = {
    val patLength = pattern.length
    if (offset + patLength > in.length) false
    else (0 until patLength).find { i => pattern(i) != in(offset + i) }.isEmpty
  }

  def startsWith(in : Array[Byte], pattern : Array[Byte]) : Boolean = hasFragment(in, 0, pattern)

}
