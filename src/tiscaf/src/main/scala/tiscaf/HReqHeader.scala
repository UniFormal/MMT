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

/** Contains the request header data:
 *   - request method,
 *   - host, port, ...
 *   - sent headers.
 */
trait HReqHeaderData {
  def reqType : HReqType.Value

  def host : Option[String]
  def port : Option[String]

  def uriPath : String
  def uriExt : Option[String]
  def query : String

  def header(key : String) : Option[String]
  def headerKeys : scala.collection.Set[String]

  def isPersistent : Boolean

  def contentLength : Option[Long]
  def boundary : Option[String]
}

/** Request method type. */
object HReqType extends Enumeration {
  val Invalid = Value("Invalid")
  val Get = Value("GET")
  val PostData = Value("POST/application/x-www-form-urlencoded")
  val PostOctets = Value("POST/application/octet-stream")
  val PostMulti = Value("POST/multipart/form-data")
  val Delete = Value("DELETE")
  val Options = Value("OPTIONS")
  val Head = Value("HEAD")
}

private class HAddress(val uriPath : String, val uriExt : Option[String], val query : String)
private class HHostPort(val host : Option[String], val port : Option[String])

private class HReqHeader(streamStrings : Seq[String]) extends HReqHeaderData {

  lazy val reqType = if (strings.isEmpty) HReqType.Invalid else parseMethod(strings.head)

  lazy val host = hostPort.host
  lazy val port = hostPort.port

  // these three methods can rise exception but will not as far as can be called
  // when request header is valid
  lazy val uriPath : String = address.get.uriPath
  lazy val uriExt : Option[String] = address.get.uriExt
  lazy val query : String = address.get.query

  def header(key : String) : Option[String] = pairs.get(key.toLowerCase)
  lazy val headerKeys : scala.collection.Set[String] = pairs.keySet

  lazy val isPersistent = parsePersistence

  lazy val contentLength : Option[Long] = try { Some(pairs("content-length").toLong) } catch { case _: Exception => None }
  lazy val boundary = parseBoundary

  /*
  def toText : String = reqType match {
    case HReqType.Invalid => "Invalid header"
    case _ =>
      "**Header strings: \n" + strings +
      "\n request method: " + reqType +
      "\n host: " + host +
      "\n port: " + port +
      "\n reqType: " + reqType +
      "\n uriPath: " + uriPath +
      "\n uriExt: " + uriExt +
      "\n query: " + query +
      "\n headerKeys: " + headerKeys +
      "\n contentLength: " + contentLength +
      "\n boundary: " + boundary + "\n"
  }
  */

  //--------------------------
  private lazy val strings = unwrapStrings
  private lazy val pairs = fillInPairs
  private lazy val address : Option[HAddress] = if (strings.isEmpty) None else {
    val parts = strings.head.split(" ")
    if (parts.size != 3) None else Some(parseAddress(parts(1)))
  }
  private lazy val hostPort = parseHostPort

  private def unwrapStrings : Seq[String] = {
    @scala.annotation.tailrec
    def step(acc : List[String], from : Seq[String]) : Seq[String] = if (from.isEmpty) acc else {
      val raw = from.head
      if (raw.startsWith("\t") || raw.startsWith(" ")) {
        if (acc.isEmpty) step(raw.trim :: acc, from.tail)
        else step((acc.head + " " + raw.trim) :: acc.tail, from.tail)
      } else step(raw.trim :: acc, from.tail)

    }
    step(Nil, streamStrings).reverse
  }

  private def parseHostPort : HHostPort = pairs.get("host") match {
    case None => new HHostPort(None, None)
    case Some(h) =>
      val hostParts = h.split(":")
      new HHostPort(Some(hostParts(0)), { if (hostParts.length == 1) None else Some(hostParts(1)) })
  }

  private def parseMethod(s : String) : HReqType.Value = {
    val parts = s.split(" ")
    if (parts.length != 3 /* method uri protocol */ ) HReqType.Invalid else {
      parts(0).trim match {
        case "GET"     => HReqType.Get
        case "POST"    => parsePostMethod
        case "DELETE"  => HReqType.Delete
        case "OPTIONS" => HReqType.Options
        case "HEAD"    => HReqType.Head
        case _         => HReqType.Invalid
      }
    }
  }

  private def parseAddress(s : String) : HAddress = {
    val parts = s.split("\\?", 2)
    val query = if (parts.length == 2) parts(1) else ""
    val uri = {
      val full = parts(0).replace('\\', '/')
      if (full.startsWith("/")) full.substring(1) else full
    }
    val extParts = uri.split(";")
    if (extParts.length == 2 && extParts(1).trim.size > 0)
      new HAddress(extParts(0), Some(extParts(1).trim), query)
    else
      new HAddress(extParts(0), None, query)
  }

  private def parsePostMethod : HReqType.Value = contentLength match {
    case None => HReqType.Invalid
    case Some(le) => pairs.get("content-type") match {
      case None => HReqType.Invalid
      case Some(contType) => contType.toLowerCase match {
        case da if (da.contains("application/x-www-form-urlencoded")) =>
          HReqType.PostData
        case mu if (mu.contains("multipart/form-data")) =>
          contType.split(";").map(_.trim).find(_.toLowerCase.startsWith("boundary")) match {
            case None => HReqType.Invalid
            case Some(bnd) => try {
              val aBoundary = Some(bnd.split("=")(1).trim) // to rise an exception
              HReqType.PostMulti
            } catch { case _: Exception => HReqType.Invalid }
          }
        case _ => HReqType.PostOctets // falling back to 
      }
    }
  }

  private def parseBoundary : Option[String] = reqType match {
    case HReqType.PostMulti => pairs.get("content-type") match {
      case None => None
      case Some(contType) => contType.split(";").map(_.trim).find(_.toLowerCase.startsWith("boundary")) match {
        case None      => None
        case Some(bnd) => try { Some(bnd.split("=")(1).trim) } catch { case _: Exception => None }
      }
    }
    case _ => None
  }

  private def fillInPairs : Map[String, String] = {
    @scala.annotation.tailrec
    def step(acc : Map[String, String], from : Seq[String]) : Map[String, String] = if (from.isEmpty) acc else {
      val s = from.head
      val idx = s.indexOf(":")
      if (idx < 0) step(acc + Pair(s.trim.toLowerCase, ""), from.tail)
      else step(acc + Pair(s.substring(0, idx).trim.toLowerCase, s.substring(idx + 1).trim), from.tail)
    }
    step(Map(), strings.tail) // skip first HTTP string
  }

  private def parsePersistence : Boolean = pairs.get("connection") match {
    case None      => false
    case Some(con) => con.toLowerCase.contains("keep-alive")
  }
}

