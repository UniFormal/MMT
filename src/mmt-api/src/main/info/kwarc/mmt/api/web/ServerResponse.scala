package info.kwarc.mmt.api.web

import info.kwarc.mmt.api.Error
import info.kwarc.mmt.api.utils.{JSON, xml}
import tiscaf.{HLet, HSimpleLet, HStatus, HTalk}

import scala.collection.mutable

/** represents a response sent by the server */
class ServerResponse(var statusCode: ResponseStatusCode, val headers : mutable.Map[String, String], var body : Array[Byte]) {

  /** sets the content type of this serverResponse */
  def setContentType(tp : String): Unit =  {
    headers("Content-Type") = s"$tp; charset=utf8"
  }

  /** sets the response text of this ServerResponse */
  def setText(text : String): Unit = {
    body = text.getBytes("utf-8")
  }

  /** sets a CORS header matching to a request */
  def setMatchingCORS(request : ServerRequest): Unit = {
    val origin = request.headers.getOrElse("Origin", "*")
    headers("Access-Control-Allow-Origin") = origin
    headers("Access-Control-Allow-Methods") = "POST, GET, OPTIONS"
    headers("Access-Control-Max-Age") = "1000"
    headers("Access-Control-Allow-Headers") = "origin, x-csrf-token, content-type, content-length, accept"
  }
}

object ServerResponse {

  /** creates a new (empty) server Response */
  def empty() : ServerResponse = new ServerResponse(OK, new mutable.HashMap(), new Array(0))

  /**
    * A textual response that contains a status code and other things
    * @param text the message that is sent in the HTTP body
    * @param tp The content type of the response
    * @param statusCode the statusCode of the response
    */
  def apply(text : String, tp : String, statusCode : ResponseStatusCode) : ServerResponse = {
    val resp = empty()
    resp.statusCode = statusCode
    resp.setContentType(tp)
    resp.setText(text)
    resp
  }

  /**
    * a response that sends text back to the client
    * @param text the message that is sent in the HTTP body
    * @param tp The content type of the response
    */
  def TypedTextResponse(text: String, tp: String): ServerResponse = apply(text, tp, OK)

  /**
    * a response that sends text back to the client
    * @param text the message that is sent in the HTTP body
    * @param tp The content type of the response
    * @param statusCode the statusCode of the response
    */

  def TextResponse(text: String, tp: String = "plain", statusCode: ResponseStatusCode = OK): ServerResponse = apply(text, "text/" + tp, statusCode)

  /**
    * a response that sends xml back to the client
    *
    * @param s      the XML message that is sent in the HTTP body
    * @param statusCode the statusCode of the response
    */
  def XmlResponse(s: String, statusCode: ResponseStatusCode): ServerResponse = TextResponse(s, "xml", statusCode)

  /**
    * a response that sends xml back to the client
    * @param s the XML message that is sent in the HTTP body
    */
  def XmlResponse(s: String): ServerResponse = XmlResponse(s, OK)

  /**
    * a response that sends xml back to the client
    *
    * @param node XML Node to send back to the client
    * @param statusCode the statusCode of the response
    */
  def XMLResponse(node: scala.xml.Node, statusCode: ResponseStatusCode): ServerResponse = TextResponse(node.toString, "xml", statusCode)

  /**
    * An XML response that the server sends back to the browser
    *
    * @param node the XML message that is sent in the HTTP body
    */
  def XmlResponse(node: scala.xml.Node): ServerResponse = XMLResponse(node, OK)

  /**
    * a response that sends json back to the client
    *
    * @param json the json message that is sent in the HTTP body
    */
  def JsonResponse(json: JSON): ServerResponse = JsonResponse(json, OK)

  /**
    * a response that sends json back to the client
    *
    * @param json the json message that is sent in the HTTP body
    * @param statusCode the statusCode of the response
    */
  def JsonResponse(json: JSON, statusCode: ResponseStatusCode): ServerResponse = apply(json.toString, "application/json", statusCode)

  /** builds an error response from an error */
  def errorResponse(error: Error, req: ServerRequest): ServerResponse = {
    val responseType = req.accept(List("text/plain", "text/xml", "text/html"))

    if (responseType == "text/html") {
      plainErrorResponse(error)
    } else if (responseType == "text/xml") {
      xmlErrorResponse(error)
    // fall back to an html error response
    } else {
      htmlErrorResponse(error)
    }

  }

  /** builds a smart error message from an error */
  def errorResponse(msg: String, req: ServerRequest): ServerResponse = errorResponse(ServerError(msg), req)

  /** an error response in plain text format */
  def plainErrorResponse(error: Error): ServerResponse = TextResponse(error.toStringLong, statusCode=InternalServerError)

  /** an error response in html format */
  def htmlErrorResponse(error: Error): ServerResponse = TextResponse(s"""<div xmlns="${xml.namespace("html")}"><div>""" + error.toHTML + "</div></div>", "html", InternalServerError)

  /** an error response in xml format */
  def xmlErrorResponse(error: Error): ServerResponse = XMLResponse(error.toNode, InternalServerError)

  /** makes a ServerResonse act on a tiscaf server */
  def actOnTiscaf(response : ServerResponse, tk: HTalk) : HLet = {
    var tiscafRef = tk
      .setStatus(ResponseStatusCode.toTiscaf(response.statusCode))
      .setContentLength(response.body.size)
    response.headers.foreach(fv => {
      tiscafRef = tiscafRef.setHeader(fv._1, fv._2)
    })
    tiscafRef.write(response.body)
  }
}

/** represents a response code sent back by the server */
sealed abstract class ResponseStatusCode(val code : Int, val description : String)

object ResponseStatusCode {
  private val all = List[ResponseStatusCode](
    Continue, SwitchingProtocols, Processing, OK, Created, Accepted, NonAuthoritativeInformation, NoContent, ResetContent,
    PartialContent, MultiStatus, AlreadyReported, IMUsed, MultipleChoices, MovedPermanently, Found, SeeOther,
    NotModified, UseProxy, SwitchProxy, TemporaryRedirect, PermanentRedirect, BadRequest, Unauthorized, PaymentRequired,
    Forbidden, NotFound, MethodNotAllowed, NotAcceptable, ProxyAuthenticationRequired, RequestTimeout, Conflict, Gone,
    LengthRequired, PreconditionFailed, PayloadTooLarge, URITooLong, UnsupportedMediaType, RangeNotSatisfiable,
    ExpectationFailed, ImATeapot, MisdirectedRequest, UnprocessableEntity, Locked, FailedDependency, UpgradeRequired,
    PreconditionRequired, TooManyRequests, RequestHeaderFieldsTooLarge, UnavailableForLegalReasons, InternalServerError,
    NotImplemented, BadGateway, ServiceUnavailable, GatewayTimeout, HTTPVersionNotSupported, VariantAlsoNegotiates,
    InsufficientStorage, LoopDetected, NotExtended, NetworkAuthenticationRequired, RailgunError
  )

  /** returns the statusCode coressponding to a given int or a customCode */
  def apply(code : Int, description: String = "") : ResponseStatusCode  = all.find(_.code == code).getOrElse(CustomCode(code, description))

  /** turns an internal tiscaf response into a ResponseStatusCode */
  def apply(status : HStatus.Value) = {
    val (code, desc) = HStatus.strings(status)
    apply(code.toInt, desc)
  }

  /** turns this statusCode into a TISCAF Status Code */
  def toTiscaf(code : ResponseStatusCode) : HStatus.Value = {
    HStatus.strings.find(_._2._1 == code.code.toString()).map(_._1).get
  }
}

case object Continue extends ResponseStatusCode(100, "Continue")
case object SwitchingProtocols extends ResponseStatusCode(101, "Switching Protocols")
case object Processing extends ResponseStatusCode(102, "Processing")
case object OK extends ResponseStatusCode(200, "OK")
case object Created extends ResponseStatusCode(201, "Created")
case object Accepted extends ResponseStatusCode(202, "Accepted")
case object NonAuthoritativeInformation extends ResponseStatusCode(203, "Non-Authoritative Information")
case object NoContent extends ResponseStatusCode(204, "No Content")
case object ResetContent extends ResponseStatusCode(205, "Reset Content")
case object PartialContent extends ResponseStatusCode(206, "Partial Content")
case object MultiStatus extends ResponseStatusCode(207, "Multi-Status")
case object AlreadyReported extends ResponseStatusCode(208, "Already Reported")
case object IMUsed extends ResponseStatusCode(226, "IM Used")
case object MultipleChoices extends ResponseStatusCode(300, "Multiple Choices")
case object MovedPermanently extends ResponseStatusCode(301, "Moved Permanently")
case object Found extends ResponseStatusCode(302, "Found")
case object SeeOther extends ResponseStatusCode(303, "See Other")
case object NotModified extends ResponseStatusCode(304, "Not Modified")
case object UseProxy extends ResponseStatusCode(305, "Use Proxy")
case object SwitchProxy extends ResponseStatusCode(306, "Switch Proxy")
case object TemporaryRedirect extends ResponseStatusCode(307, "Temporary Redirect")
case object PermanentRedirect extends ResponseStatusCode(308, "Permanent Redirect")
case object BadRequest extends ResponseStatusCode(400, "Bad Request")
case object Unauthorized extends ResponseStatusCode(401, "Unauthorized")
case object PaymentRequired extends ResponseStatusCode(402, "Payment Required")
case object Forbidden extends ResponseStatusCode(403, "Forbidden")
case object NotFound extends ResponseStatusCode(404, "Not Found")
case object MethodNotAllowed extends ResponseStatusCode(405, "Method Not Allowed")
case object NotAcceptable extends ResponseStatusCode(406, "Not Acceptable")
case object ProxyAuthenticationRequired extends ResponseStatusCode(407, "Proxy Authentication Required")
case object RequestTimeout extends ResponseStatusCode(408, "Request Timeout")
case object Conflict extends ResponseStatusCode(409, "Conflict")
case object Gone extends ResponseStatusCode(410, "Gone")
case object LengthRequired extends ResponseStatusCode(411, "Length Required")
case object PreconditionFailed extends ResponseStatusCode(412, "Precondition Failed")
case object PayloadTooLarge extends ResponseStatusCode(413, "Payload Too Large")
case object URITooLong extends ResponseStatusCode(414, "URI Too Long")
case object UnsupportedMediaType extends ResponseStatusCode(415, "Unsupported Media Type")
case object RangeNotSatisfiable extends ResponseStatusCode(416, "Range Not Satisfiable")
case object ExpectationFailed extends ResponseStatusCode(417, "Expectation Failed")
case object ImATeapot extends ResponseStatusCode(418, "I'm a teapot")
case object MisdirectedRequest extends ResponseStatusCode(421, "Misdirected Request")
case object UnprocessableEntity extends ResponseStatusCode(422, "Unprocessable Entity")
case object Locked extends ResponseStatusCode(423, "Locked")
case object FailedDependency extends ResponseStatusCode(424, "Failed Dependency")
case object UpgradeRequired extends ResponseStatusCode(426, "Upgrade Required")
case object PreconditionRequired extends ResponseStatusCode(428, "Precondition Required")
case object TooManyRequests extends ResponseStatusCode(429, "Too Many Requests")
case object RequestHeaderFieldsTooLarge extends ResponseStatusCode(431, "Request Header Fields Too Large")
case object UnavailableForLegalReasons extends ResponseStatusCode(451, "Unavailable For Legal Reasons")
case object InternalServerError extends ResponseStatusCode(500, "Internal Server Error")
case object NotImplemented extends ResponseStatusCode(501, "Not Implemented")
case object BadGateway extends ResponseStatusCode(502, "Bad Gateway")
case object ServiceUnavailable extends ResponseStatusCode(503, "Service Unavailable")
case object GatewayTimeout extends ResponseStatusCode(504, "Gateway Time-out")
case object HTTPVersionNotSupported extends ResponseStatusCode(505, "HTTP Version Not Supported")
case object VariantAlsoNegotiates extends ResponseStatusCode(506, "Variant Also Negotiates")
case object InsufficientStorage extends ResponseStatusCode(507, "Insufficient Storage")
case object LoopDetected extends ResponseStatusCode(508, "Loop Detected")
case object NotExtended extends ResponseStatusCode(510, "Not Extended")
case object NetworkAuthenticationRequired extends ResponseStatusCode(511, "Network Authentication Required")
case object RailgunError extends ResponseStatusCode(527, "Railgun Error")

case class CustomCode(override val code : Int, override val description : String) extends ResponseStatusCode(code, description)