package info.kwarc.mmt.api.web

import java.io.InputStream

import info.kwarc.mmt.api._
import utils._

import scala.collection.mutable

/**
  * Mutable class that represents a response sent from the Server to the Client
  *
  * Defaults to UTF-8 content type for text responses.
  */
class ServerResponse {

  override def toString = outputStream match {
    case Left(a) => a.mkString
    case Right(s) => "(outputstream)"
  }

  /** status of the response */
  private var status : (Int, String) = (200, "OK")
  /** statusCode (HTTP response code) for this message */
  def statusCode : Int = status._1
  def statusCode_=(code : Int): Unit = {
    // set statusCode and matching text
    val newText = ServerResponse.statusCodes.getOrElse(code, "")
    status = (code, newText)
  }
  /** the status text (HTTP Status Text) for this response code */
  def statusText: String = status._2
  def statusText_=(txt : String): Unit = {
    status = (statusCode, txt)
  }

  /** headers to be sent in the response */
  var headers : mutable.Map[String, String] = new mutable.HashMap()
  /** the content type of the response */
  def contentType : String = headers.getOrElse("Content-Type", "").split(";").head
  /** sets the content type of this serverResponse */
  def contentType_=(tp : String): Unit =  {
    headers("Content-Type") = s"$tp; charset=utf8"
  }
  /** sets a CORS header matching to a request */
  def setCORSFor(request : ServerRequest): Unit = {
    val origin = request.headers.getOrElse("Origin", "*")
    headers("Access-Control-Allow-Origin") = origin
    headers("Access-Control-Allow-Methods") = "POST, GET, OPTIONS"
    headers("Access-Control-Max-Age") = "1000"
    headers("Access-Control-Allow-Headers") = "origin, x-csrf-token, content-type, content-length, accept"
  }

  // OUTPUT
  private var outputStream : Union[Array[Byte], InputStream] = Left(Array.empty)
  /** the ouput is either an array or read directly from an input stream */
  def output = outputStream
  /** sets the output to an array of bytes */
  def output_= (ary : Array[Byte]): Unit = {
    outputStream = Left(ary)
  }
  /** sets the output to text */
  def output_=(txt : String): Unit = {
    outputStream = Left(txt.getBytes("utf-8"))
  }
  /** sets the output to an InputStream */
  def output_=(io: InputStream): Unit = {
    outputStream = Right(io)
  }
}

/**
  * Factory methods for typical responses
  * As is the case with the companion class [[ServerResponse]], all text responses default to UTF-8
  * (via the Content-Type header).
  */
object ServerResponse {

  // typical responses
  def TextResponse(text: String, tp: String = "plain"): ServerResponse = apply(text, "text/" + tp)
  def HTMLResponse(text: String): ServerResponse = TextResponse(text, "html")
  def XmlResponse(text: String): ServerResponse = TextResponse(text, "xml")
  def XmlResponse(node: scala.xml.Node): ServerResponse = fromXML(node)
  def JsonResponse(content: JSON): ServerResponse = fromJSON(content)
  def FileResponse(file: utils.File): ServerResponse = {
    val in = new java.io.FileInputStream(file)
    anyDataResponse(in, file.getExtension)
  }
  /** builds an error response from an error message */
  def errorResponse(msg: String, format: String = "html"): ServerResponse = errorResponse(ServerError(msg), format)
  /** builds an error response from an error */
  // error responses intentionally do not use HTTP error codes or accept headers
  def errorResponse(error: Error, format: String): ServerResponse = {
    if (format == "html") {
      HTMLResponse(s"""<div xmlns="${utils.xml.namespace("html")}"><div>""" + error.toHTML + "</div></div>")
    } else {
      XmlResponse(error.toNode)
    }
  }

  /** an MMT resource
    * @param path path of File to be sent back
    */
  def ResourceResponse(path: String) : ServerResponse = {
    val io = Util.loadResource(path)
    if (io == null) {
      errorResponse(s"Path $path not found")
    } else {
      anyDataResponse(io, utils.File(path).getExtension)
    }
  }

  /** creates a response from a file etc
    *  @param extension file extension (only used to guess a content type)
    */
  def anyDataResponse(in: InputStream, extension: Option[String]): ServerResponse = {
    // start with an empty response
    val resp = new ServerResponse
    // set the content type, possibly using the file extension
    resp.contentType = extension.flatMap(e => contentTypes.get(e.toLowerCase)).getOrElse("text/plain")
    // set the output and return
    resp.output = in
    resp
  }


  // ***************** more sophisticated methods added by Tom (?), probably not needed

  /**
    * Convenience method to construct a standard response
    * @param content Textual message to be sent in the HTTP body
    * @param contentType contentType of the message to be returned
    * @param statusCode statusCode of the message
    */
  def apply(content : String, contentType : String, statusCode : Int = statusCodeOK) : ServerResponse = {
    val resp = new ServerResponse
    resp.statusCode = statusCode
    resp.contentType = contentType
    resp.output = content
    resp
  }

  /**
    * Convenience method to send back a text/ response back to the client.
    * @param content Textual message to be sent in the HTTP body
    * @param contentType Textual content type, will be returned as text/_ in the header
    * @param statusCode statusCode of the message
    */
  def fromText(content : String, contentType : String = "plain", statusCode : Int = statusCodeOK) : ServerResponse = apply(content, s"text/$contentType", statusCode)

  /**
    * Convenience method to send application/xml back to the client
    * @param content XML object to send back to the user
    * @param statusCode statusCode of the message
    */
  def fromXML(content: scala.xml.Node, statusCode : Int = statusCodeOK) : ServerResponse = apply(content.toString, "application/xml", statusCode)

  /**
    * Convenience method to send application/json back to the client
    * @param content JSON object to send back to the user
    * @param statusCode statusCode of the message
    */
  def fromJSON(content : JSON, statusCode : Int = statusCodeOK) : ServerResponse = apply(content.toString, "application/json", statusCode)

  // HELPERS

  private val contentTypes = Map(
    "html" -> "text/html",
    "htm" -> "text/html",
    "js" -> "application/x-javascript",
    "css" -> "text/css",
    "gif" -> "image/gif",
    "ico" -> "image/x-icon",
    "jpeg" -> "image/jpeg ",
    "jpg" -> "image/jpeg ",
    "json" -> "application/json",
    "png" -> "image/png",
    "pdf" -> "application/pdf",
    "zip" -> "application/zip",
    "xhtml" -> "application/xhtml+xml",
    "svg" -> "image/svg+xml",
    "txt" -> "text/plain",
    "scala" -> "text/plain",
    "xml" -> "application/xml",
    "xsl" -> "application/xml",
    "tgz" -> "application/x-gtar",
    "jar" -> "application/java-archive"
  )

  private val statusCodes = Map(
    100 -> "Continue",
    101 -> "Switching Protocols",
    102 -> "Processing",
    200 -> "OK",
    201 -> "Created",
    202 -> "Accepted",
    203 -> "Non-Authoritative Information",
    204 -> "No Content",
    205 -> "Reset Content",
    206 -> "Partial Content",
    207 -> "Multi-Status",
    208 -> "Already Reported",
    226 -> "IM Used",
    300 -> "Multiple Choices",
    301 -> "Moved Permanently",
    302 -> "Found",
    303 -> "See Other",
    304 -> "Not Modified",
    305 -> "Use Proxy",
    306 -> "Switch Proxy",
    307 -> "Temporary Redirect",
    308 -> "Permanent Redirect",
    400 -> "Bad Request",
    401 -> "Unauthorized",
    402 -> "Payment Required",
    403 -> "Forbidden",
    404 -> "Not Found",
    405 -> "Method Not Allowed",
    406 -> "Not Acceptable",
    407 -> "Proxy Authentication Required",
    408 -> "Request Timeout",
    409 -> "Conflict",
    410 -> "Gone",
    411 -> "Length Required",
    412 -> "Precondition Failed",
    413 -> "Payload Too Large",
    414 -> "URI Too Long",
    415 -> "Unsupported Media Type",
    416 -> "Range Not Satisfiable",
    417 -> "Expectation Failed",
    418 -> "I'm a teapot",
    421 -> "Misdirected Request",
    422 -> "Unprocessable Entity",
    423 -> "Locked",
    424 -> "Failed Dependency",
    426 -> "Upgrade Required",
    428 -> "Precondition Required",
    429 -> "Too Many Requests",
    431 -> "Request Header Fields Too Large",
    451 -> "Unavailable For Legal Reasons",
    500 -> "Internal Server Error",
    501 -> "Not Implemented",
    502 -> "Bad Gateway",
    503 -> "Service Unavailable",
    504 -> "Gateway Time-out",
    505 -> "HTTP Version Not Supported",
    506 -> "Variant Also Negotiates",
    507 -> "Insufficient Storage",
    508 -> "Loop Detected",
    510 -> "Not Extended",
    511 -> "Network Authentication Required",
    527 -> "Railgun Error"
  )

  val statusCodeOK : Int = 200
  val statusCodeMovedPermanently : Int = 301
  val statusCodeTemporaryRedirect : Int = 307
  val statusCodePermanentRedirect : Int = 308
  val statusCodeBadRequest : Int = 400
  val statusCodeForbidden : Int = 403
  val statusCodeNotFound : Int = 404
  val statusCodeInternalServerError : Int = 500
  val statusCodeNotImplemented : Int = 501
  val statusCodeBadGateway : Int = 502
  val statusCodeServiceUnavailable : Int = 503
}