package info.kwarc.mmt.api.web

import java.io.InputStream

import info.kwarc.mmt.api.{Error, utils}
import info.kwarc.mmt.api.utils.{JSON, xml}

import scala.collection.immutable.HashMap
import scala.collection.mutable

/**
  * Mutable class that represents a response sent from the Server to the Client
  */
class ServerResponse {

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
  def statusText : String = status._2

  def statusText_=(txt : String) : Unit = {
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

  private var outputStream : Either[Array[Byte], InputStream] = Left(Array.empty)

  /** the ouput is either an array or read directly from an input stream */
  def output : Either[Array[Byte], InputStream] = outputStream

  /** sets the output to an array of bytes */
  def output_= (ary : Array[Byte]): Unit = {
    outputStream = Left(ary)
  }

  /** sets the output to text */
  def output_=(txt : String) : Unit = {
    outputStream = Left(txt.getBytes("utf-8"))
  }

  /** sets the output to an InputStream */
  def output_=(io: InputStream) : Unit = {
    outputStream = Right(io)
  }
}

object ServerResponse {

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
    * Convenience method to construct an MMT-internal resource response. Additionally applies a map to the resource to
    * be returned.
    * @param path path of File to be sent back
    * @param map function to apply to the InputStream
    * @param request Original request sent by the client
    */
  def resource(path: String, map: InputStream => Either[Array[Byte], InputStream], request: ServerRequest) : ServerResponse = {
    //TODO: Make sure that the path is cleaned up

    val properPath = path.replace("//", "/")
    val io = Util.loadResource(properPath)

    if(io == null){
      return fromText(s"Path $path not found", statusCode = statusCodeNotFound)
    }

    // attempt to get the file extension
    val fileExtension = {
      val i = path.lastIndexOf(".")
      if (i > 0) path.substring(i + 1)
      else ""
    }

    // start with an empty response
    val resp = new ServerResponse

    // set the content type
    resp.contentType = contentTypes.getOrElse(fileExtension.toLowerCase(), "text/plain")

    // and get the output
    map(io) match {
      case Left(ary) =>
          resp.output = ary
      case Right(stream) =>
          resp.output = stream
    }

    // and return it
    resp
  }

  /**
    * Convenience method to construct an MMT-internal resource response being sent back to the client
    * @param path path of File to be sent back
    * @param request Original request sent by the client
    */
  def resource(path: String, request: ServerRequest) : ServerResponse = {
    resource(path, Right(_), request)
  }

  /**
    * Convenience method to send application/json back to the client
    * @param content JSON object to send back to the user
    * @param statusCode statusCode of the message
    */
  def fromJSON(content : JSON, statusCode : Int = statusCodeOK) : ServerResponse = apply(content.toString, "application/json", statusCode)


  /** builds an error response from an error */
  def errorResponse(error: Error, req: ServerRequest): ServerResponse = {
    // TODO: Check in a smart way if we should display it in a human readable form
    val responseType = req.accept(List("text/plain", "text/xml", "text/html"))

    if (responseType == "text/plain") {
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
  def plainErrorResponse(error: Error): ServerResponse = fromText(error.toStringLong, statusCode=statusCodeInternalServerError)

  /** an error response in html format */
  def htmlErrorResponse(error: Error): ServerResponse = fromText(s"""<div xmlns="${utils.xml.namespace("html")}"><div>""" + error.toHTML + "</div></div>", "html", statusCodeInternalServerError)

  /** an error response in xml format */
  def xmlErrorResponse(error: Error): ServerResponse = XMLResponse(error.toNode, statusCodeInternalServerError)

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


  // LEGACY methods
  // for backwards compatibility only
  // these will be removed soon(ish)

  @deprecated("use new ServerResponse instead")
  def empty() : ServerResponse = new ServerResponse

  @deprecated("use [[apply]] instead")
  def TypedTextResponse(text: String, tp: String): ServerResponse = apply(text, tp)

  @deprecated("use [[fromText]] instead")
  def TextResponse(text: String, tp: String = "plain", statusCode: Int = statusCodeOK): ServerResponse = apply(text, "text/" + tp, statusCode)

  @deprecated("for a textual xml response, use fromText(s, 'xml', statusCode) instead ")
  def XmlResponse(s: String, statusCode: Int): ServerResponse = fromText(s, "xml", statusCode)

  @deprecated("for a textual xml response, use fromText(s, 'xml') instead ")
  def XmlResponse(s: String): ServerResponse = fromText(s, "xml")

  @deprecated("use [[fromXML]] instead")
  def XMLResponse(node: scala.xml.Node, statusCode: Int): ServerResponse = fromXML(node, statusCode)

  @deprecated("use [[fromXML]] instead")
  def XmlResponse(node: scala.xml.Node): ServerResponse = fromXML(node)

  @deprecated("use [[fromJSON]] instead")
  def JsonResponse(content: JSON): ServerResponse = fromJSON(content)

  @deprecated("use [[fromJSON]] instead")
  def JsonResponse(content: JSON, statusCode: Int): ServerResponse = fromJSON(content, statusCode)
}