package info.kwarc.mmt.api.web

import java.net.{URLDecoder, URLEncoder}

import info.kwarc.mmt.api.{ParseError, utils}
import info.kwarc.mmt.api.utils.JSON

import scala.collection.immutable.TreeMap
import scala.xml.Node

/**
  * Represents a request made to the MMT Server.
  *
  * @param method Method used to make request
  * @param headerData Request headers that were sent along with the request
  * @param sessionID sessionID that was
  * @param pathStr Path string requested on the server without trailing /s
  * @param queryString The query portion of th estruing
  * @param body
  */
case class ServerRequest(method: RequestMethod.Value, private val headerData: Map[String, String], sessionID: Option[Session], pathStr: String, queryString : String, body: Body) {
  lazy val headers : TreeMap[String, String] = new TreeMap[String, String]()(Ordering.by(_.toLowerCase)) ++ headerData

  /** a set of accepted content types */
  lazy val acceptedTypes = headers.getOrElse("Accept", "").split(",").map(_.split(";").head.trim).toList

  /** checks if the server accepts a specific content type */
  def accepts(content: String) : Boolean = {
    acceptedTypes.contains(content)
  }

  /** returns the content type that is shown first */
  def accept(types: List[String], default: => String = ""): String ={
    acceptedTypes.find(types.contains(_)).getOrElse(default)
  }

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
  lazy val requestPath : String = "/" + pathStr + "?" + queryString

  /** a decoded version of the query string */
  lazy val decodedQuery : String = URLDecoder.decode(queryString, "UTF-8")

  /** a parsed version of the query string */
  lazy val parsedQuery : WebQuery = WebQuery(queryString)

  // FOR BACKWARDS COMPATIBILITY
  // METHOD CALLS SHOULD USE OTHER METHODS INSTEAD
  @deprecated("Use [[pathComponents.tail]] instead")
  lazy val path = extensionPathComponents

  @deprecated("to be safe, use [[decodedQuery]], otherwise use [[queryString]]")
  lazy val query : String = queryString

  @deprecated("use [[sessionID]] instead")
  lazy val session = sessionID.get

  @deprecated("use [[headers.lift]] instead")
  def header(key : String) : Option[String] = headers.lift(key)
}

/** Request types that can be made */
object RequestMethod extends Enumeration {
  val Get = Value("GET")
  val Post = Value("POST")
  val Delete = Value("DELETE")
  val Options = Value("OPTIONS")
  val Head = Value("HEAD")

  def toString(method : RequestMethod.Value) : String = method match {
    case Get => "GET"
    case Post => "POST"
    case Delete => "DELETE"
    case Options => "OPTIONS"
    case Head => "HEAD"
  }
}

/** straightforward abstraction of the current session */
case class Session(id: String)


/** the body of an HTTP request
  *
  */
class Body(octets: Option[Array[Byte]]) {

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

  /** returns the body as a JSON object */
  def asJSON: JSON = {
    try {
      JSON.parse(asString)
    }
    catch {
      case e: JSON.JSONError =>
        throw ServerError("error in json body").setCausedBy(e)
    }
  }
}

/** straightforward abstraction for web style key-value queries; assumes input is already decoded and duplicate-less */
case class WebQuery(pairs: List[(String, String)]) {
  /** @return the value of the key, if present */
  def apply(key: String): Option[String] = pairs.find(_._1 == key).map(_._2)

  /** @return the string value of the key, default value if not present */
  def string(key: String, default: => String = ""): String = apply(key).getOrElse(default)

  /** @return the boolean value of the key, default value if not present */
  def boolean(key: String, default: => Boolean = false): Boolean = apply(key).getOrElse(default.toString).toLowerCase match {
    case "false" => false
    case "" | "true" => true
    case s => throw ParseError("boolean expected: " + s)
  }

  /** @return the integer value of the key, default value if not present */
  def int(key: String, default: => Int = 0) : Int = {
    val s = apply(key).getOrElse(default.toString)
    try {
      s.toInt
    }
    catch {
      case _: Exception => throw ParseError("integer expected: " + s)
    }
  }

  /** checks if this WebQuery contains a specific query */
  def contains(key : String) : Boolean = pairs.exists(_._1 == key)

  /** @return a string encoded web query object */
  def asString : String = {
    pairs.map(kv => {
      URLEncoder.encode(kv._1, "UTF-8") + "=" + URLEncoder.encode(kv._2, "UTF-8")
    }).mkString("&")
  }
}

object WebQuery {
  /**
    * Turns a query string into a WebQuery
    *
    * In general, these take the form of key=value&key=value2.
    * This method takes care of URLDecoding the strings.
    * In case of duplicate keys, the first item will take priority.
    *
    * @param query
    * @return
    */
  def apply(query: String): WebQuery = {
    val kvs = utils.stringToList(query, "&")
    val pairs = kvs map { s =>
      val i = s.indexOf("=")

      val (k, v) = if (i == -1 || i == s.length - 1) (s, "")
      else (s.substring(0, i), s.substring(i + 1))

      (URLDecoder.decode(k, "UTF-8"), URLDecoder.decode(v, "UTF-8"))
    }
    WebQuery(pairs)
  }

  /** Utility method to encode a list of key/value pairs into a QueryString */
  def encode(pairs : List[(String, String)]) : String = {
    WebQuery(pairs).asString
  }

  /** same as WebQuery.apply, for backwards compatibility */
  @deprecated("you should probably use [[ServerRequest.parsedQuery]], if you still need to parse manually use [[WebQuery.apply]]. Beware that the new method takes care of URLDecoding")
  def parse(query: String) : WebQuery = {
    val kvs = utils.stringToList(query, "&")
    val pairs = kvs map { s =>
      val i = s.indexOf("=")

      val (k, v) = if (i == -1 || i == s.length - 1) (s, "")
      else (s.substring(0, i), s.substring(i + 1))

      (k, v)
    }
    WebQuery(pairs)
  }
}