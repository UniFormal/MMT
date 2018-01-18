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

/** Request data contains following elements:
 *  - sent headers,
 *  - request method, host, port, ...
 *  - request parameters
 *  - sent bytes (in case of a POST/application/octet-stream request).
 */
trait HReqData {
  // common
  def method : HReqType.Value
  def host : Option[String]
  def port : Option[String]
  def uriPath : String
  def uriExt : Option[String]
  def query : String
  def remoteIp : String
  def contentLength : Option[Long]
  def contentEncoding : String

  // header
  def header(key : String) : Option[String]
  def headerKeys : scala.collection.Set[String]

  // parameters
  def paramsKeys : Set[String]
  def params(key : String) : Seq[String]
  def param(key : String) : Option[String]
  def softParam(key : String) : String

  def asQuery(ignore : Set[String] = Set()) : String

  // param(key) helpers
  def asByte(key : String) : Option[Byte]
  def asShort(key : String) : Option[Short]
  def asInt(key : String) : Option[Int]
  def asLong(key : String) : Option[Long]
  def asFloat(key : String) : Option[Float]
  def asDouble(key : String) : Option[Double]

  // POST/application/octet-stream
  // PUT
  // PATCH
  def octets : Option[Array[Byte]]
}

object HReqData {

  def apply(data : HTalkData) = new HReqData {
    // common
    def method : HReqType.Value = data.header.reqType
    def host : Option[String] = data.header.host
    def port : Option[String] = data.header.port
    def uriPath : String = decode(data.header.uriPath)
    def uriExt : Option[String] = data.header.uriExt // no decoding needed
    def query : String = decode(data.header.query)
    def remoteIp : String = data.writer.remoteIp
    def contentLength : Option[Long] = data.header.contentLength
    def contentEncoding : String = data.header.contentEncoding

    // header
    def header(key : String) : Option[String] = data.header.header(key)
    def headerKeys : scala.collection.Set[String] = data.header.headerKeys

    // parameters
    def paramsKeys : Set[String] = data.parMap.keySet.map(decode)
    def params(key : String) : Seq[String] = data.parMap.getOrElse(encode(key), Nil).map(decode)
    def param(key : String) : Option[String] = params(key) match {
      case Seq(x, _*) => Some(x)
      case _          => None
    }
    def softParam(key : String) : String = param(key).getOrElse("")

    def asQuery(ignore : Set[String] = Set()) : String = {
      def paramQuery(key : String) = params(key).map(v => { encode(key) + "=" + encode(v) }).mkString("&")
      paramsKeys.diff(ignore).map(paramQuery).mkString("&")
    }

    // param(key) helpers
    def asByte(key : String) : Option[Byte] = try { Some(param(key).get.toByte) } catch { case _: Exception => None }
    def asShort(key : String) : Option[Short] = try { Some(param(key).get.toShort) } catch { case _: Exception => None }
    def asInt(key : String) : Option[Int] = try { Some(param(key).get.toInt) } catch { case _: Exception => None }
    def asLong(key : String) : Option[Long] = try { Some(param(key).get.toLong) } catch { case _: Exception => None }
    def asFloat(key : String) : Option[Float] = try { Some(param(key).get.toFloat) } catch { case _: Exception => None }
    def asDouble(key : String) : Option[Double] = try { Some(param(key).get.toDouble) } catch { case _: Exception => None }

    // POST/application/octet-stream
    // PUT
    // PATCH
    def octets : Option[Array[Byte]] = data.octets

    private def encode(s : String) = try { java.net.URLEncoder.encode(s, data.app.encoding) } catch { case _: Exception => s }
    private def decode(s : String) = try { java.net.URLDecoder.decode(s, data.app.encoding) } catch { case _: Exception => s }
  }

}
