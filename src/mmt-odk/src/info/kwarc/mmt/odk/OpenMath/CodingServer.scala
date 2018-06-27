package info.kwarc.mmt.odk.OpenMath

import info.kwarc.mmt.api.web._
import info.kwarc.mmt.odk.OpenMath.Coding.{OMJSONCoding, OMXMLCoding}

class CodingServer extends ServerExtension("omcoding") {
  def apply(request: ServerRequest): ServerResponse = request.pathForExtension match {
    case List("convert") => doEncodingSafe(request)
  }

  private def doEncodingSafe(request: ServerRequest) : ServerResponse = {
    try {
      val fromEncoding = request.parsedQuery.string("from", throw new Exception("missing 'from' parameter"))
      val toEncoding = request.parsedQuery.string("to", throw new Exception("missing 'to' parameter"))

      val term = doFrom(fromEncoding, request.body)
      doTo(toEncoding, term)
    } catch {
      case e: Exception => doError(e)
    }
  }

  lazy val xmlCoding = new OMXMLCoding()
  lazy val jsonCoding = new OMJSONCoding()

  private def doFrom(fromEncoding: String, body: Body): OMAny = fromEncoding match {
    case "xml" => xmlCoding.decode(body.asXML)
    case "json" => jsonCoding.decode(body.asJSON)
    case _ => throw new Exception("unknown encoding in 'from' parameter, needs to be one of 'xml', 'json'")
  }
  private def doTo(toEncoding: String, term: OMAny): ServerResponse = toEncoding match {
    case "xml" => ServerResponse.fromXML(xmlCoding.encode(term))
    case "json" => ServerResponse.fromJSON(jsonCoding.encode(term))
    case _ => throw new Exception("unknown encoding in 'to' parameter, needs to be one of 'xml', 'json'")
  }

  private def doError(exp: Exception): ServerResponse = {
    val exception = ServerError(exp.getMessage).setCausedBy(exp)
    ServerResponse.fromXML(exception.toNode, ServerResponse.statusCodeBadRequest)
  }
}