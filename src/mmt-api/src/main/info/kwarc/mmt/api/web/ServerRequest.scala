package info.kwarc.mmt.api.web

import info.kwarc.mmt.api.utils.JSON
import tiscaf.{HReqData, HReqType, HTalk}

import scala.xml.Node

/**
  * A request made to an extension
  *
  * @param path the PATH from above (excluding CONTEXT)
  * @param query the QUERY from above
  * @param body the body of the request
  * @param headers Headers of the request sent to the server
  * @param session Session Information
  */
case class ServerRequest(method: RequestMethod.Value, headers: Map[String, String], sessionID: Option[Session], pathStr: String, parsedQuery : WebQuery, body: Body) {

  /** components of the path */
  lazy val pathComponents : List[String] = pathStr.split("/").toList

  /** Name of the extension that should serve this request, if applicable */
  lazy val extensionName : Option[String] = pathComponents match {
    case h :: t if h.startsWith(":") => Some(h.substring(1))
    case _ => None
  }

  /** path that is served by an extension if applicable, else Nil */
  lazy val extensionPathComponents : List[String] = extensionName match {
    case Some(ext) => pathComponents.tail
    case None => Nil
  }

  /** the full path requested to this extension, excluding query string */
  lazy val extensionPath : String = extensionPathComponents.mkString("/")

  /** the full path requested from this server, including query string */
  lazy val requestPath = "/" + pathStr + "?" + queryString

  /** a raw version of the queryString */
  lazy val queryString : String = parsedQuery.asString

  // FOR BACKWARDS COMPATIBILITY
  // METHOD CALLS SHOULD USE OTHER METHODS INSTEAD
  @deprecated("Use [[extensionPathComponents]] instead")
  lazy val path = extensionPathComponents

  @deprecated("Use [[queryString]] instead")
  lazy val query : String = queryString

  @deprecated("use [[sessionID]] instead")
  lazy val session = sessionID.get
}

object ServerRequest {
  /** creates a new request object from an internal Tiscaf HReqData object */
  def apply(data : HReqData) : ServerRequest = {
    val method = RequestMethod(data.method)
    val headers = data.headerKeys.map(k => (k, data.header(k).get)).toMap[String, String]
    val sessionID = None
    val pathStr = data.uriPath
    val parsedQuery = WebQuery.parse(data.query)
    val body = new Body(None)
    ServerRequest(method, headers, sessionID, pathStr, parsedQuery, body)
  }
  /** adds information from interal Tiscaf HTalk object into a request */
  def apply(req : ServerRequest, tk: HTalk): ServerRequest = {
    req.copy(body=new Body(tk.req.octets), sessionID=Some(Session(tk.ses.id)))
  }
  def apply(tk: HTalk): ServerRequest = {
    apply(apply(tk.req), tk)
  }
}

/** Request types that can be made */
object RequestMethod extends Enumeration {
  val Get = Value("GET")
  val Post = Value("POST")
  val Delete = Value("DELETE")
  val Options = Value("OPTIONS")
  val Head = Value("HEAD")

  def apply(method: HReqType.Value): RequestMethod.Value = method match {
    case HReqType.Get => Get
    case HReqType.PostData => Post
    case HReqType.PostMulti => Post
    case HReqType.Delete => Delete
    case HReqType.Options => Options
    case HReqType.Head => Head
  }
}

/** straightforward abstraction of the current session */
case class Session(id: String)


/** the body of an HTTP request
  *
  * This class abstracts from tiscaf's HTalk internal.
  */
class Body(octets: Option[Array[Byte]]) {
  @deprecated("Pass octets directly")
  def this(tk: HTalk) = this(tk.req.octets)

  /** returns the body of a request as a string */
  def asString: String = {
    val bodyArray: Array[Byte] = octets.getOrElse(throw ServerError("no body found"))
    new String(bodyArray, "UTF-8")
  }

  /** body (if any) as a string */
  def asStringO = asString match {
    case "" => None
    case s => Some(s)
  }

  /** returns the body of a request as XML */
  def asXML: Node = {
    val bodyString = asString
    val bodyXML = try {
      scala.xml.XML.loadString(bodyString).head
    } catch {
      case _: Exception => throw ServerError("invalid XML")
    }
    scala.xml.Utility.trim(bodyXML)
  }

  def asJSON = {
    try {
      JSON.parse(asString)
    }
    catch {
      case e: JSON.JSONError =>
        throw ServerError("error in json body").setCausedBy(e)
    }
  }
}