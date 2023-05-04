package info.kwarc.mmt.api.web

import java.net.URLEncoder
import info.kwarc.mmt.api.ParseError
import info.kwarc.mmt.api.utils._

import scala.collection.immutable
import scala.xml.Node

/**
  * Represents a request made to the MMT Server.
  *
  * @param method Method used to make request
  * @param headers Request headers that were sent along with the request
  * @param session sessionID that was
  * @param path the full path of the request (use pathForExtension when accessing the path in a [[ServerExtension]])
  * @param query the query portion of the URL
  * @param body
  */
case class ServerRequest(method: RequestMethod.Value, val headers: Map[String, String], session: Option[Session],
                         path: List[String], query: String, body: Body) {
  def toStringShort = RequestMethod.toString(method) + " " + pathString + "?" + query

  def header(key : String) : Option[String] = headers.get(key)
  def pathString = path.mkString("/")

  def extensionName = path match {
    case ext :: _ if ext.startsWith(":") => Some(ext.substring(1))
    case _ => None
  }
  def pathForExtension = path match {
    case ext :: tail if ext.startsWith(":") => tail
    case _ => path
  }

  /** a decoded version of the query string */
  lazy val decodedQuery = URI.decode(query)
  /** a parsed version of the query string */
  lazy val parsedQuery = WebQuery(query)

  /** a set of accepted content types */
  lazy val acceptedTypes = headers.getOrElse("Accept", "").split(",").map(_.split(";").head.trim).toList
  /** checks if the server accepts a specific content type */
  def accepts(content: String) : Boolean = {
    acceptedTypes.contains(content)
  }
  /** returns the content type that is shown first */
  def accept(types: List[String], default: => String = ""): String = {
    acceptedTypes.find(types.contains(_)).getOrElse(default)
  }
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
class Body(octets: Option[Array[Byte]], val params : immutable.Map[String,String] = immutable.Map.empty) {
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
    val kvs = stringToList(query, """&""")
    val pairs = kvs map { s =>
      val i = s.indexOf("=")

      val (k, v) = if (i == -1 || i == s.length - 1) (s, "")
      else (s.substring(0, i), s.substring(i + 1))

      (URI.decode(k), URI.decode(v))
    }
    WebQuery(pairs)
  }

  /** Utility method to encode a list of key/value pairs into a QueryString */
  def encode(pairs : List[(String, String)]) : String = {
    WebQuery(pairs).asString
  }
}
