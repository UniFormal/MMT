package info.kwarc.mmt.api.utils

import java.net

import info.kwarc.mmt.api._

/** Custom implementation of the URI RFC that's better than java.net.URI
  *
  * @param abs true if the path is absolute (ignored if scheme or authority are present) */
case class URI(scheme: Option[String],
               authority: Option[String],
               path: List[String] = Nil,
               private val abs: Boolean = false,
               query: Option[String] = None,
               fragment: Option[String] = None) {
  private def isIllegal = (!abs && path.nonEmpty && authority.isDefined) || (!abs && path.startsWith(List("")))
  if (isIllegal) throw ImplementationError("illegal URI: " + this)
  /** true if the path is absolute */
  def absolute: Boolean = abs

  /** drop authority, path, query, fragment, append authority and make path absolute */
  def colon(n: String): URI = URI(scheme, Some(n), abs = true)

  /** drop path, query, fragment, append (absolute) path of length 1 */
  def !/(n: String): URI = this !/ List(n)

  /** drop path, query, fragment, append (absolute) path */
  def !/(p: List[String]): URI = {
    URI(scheme, authority, p, abs = true, None, None)
  }

  /** drop query, fragment, append one segment to path */
  def /(n: String): URI = this / List(n)

  /** drop query, fragment, append to path
    * path stays relative/absolute; but URI(_, Some(_), Nil, false, _, _) / _ turns path absolute
    * trailing empty segment of this URI is dropped when appending
    */
  def /(p: List[String]): URI = {
    val mustBeAbs = p.nonEmpty && authority.isDefined
    URI(scheme, authority, pathNoTrailingSlash ::: p, absolute || mustBeAbs)
  }

  /** drops query and fragment, drop last path segment (if any) */
  def ^ : URI = URI(scheme, authority, if (path.isEmpty) Nil else path.init, absolute)

  /** drops query and fragment and path */
  def ^! : URI = URI(scheme, authority, abs = absolute)

  /** drop query, fragment, append query */
  def ?(q: String): URI = URI(scheme, authority, path, absolute, Some(q))

  /** drop fragment, append fragment */
  def ##(f: String): URI = URI(scheme, authority, path, absolute, query, Some(f))

  /** true iff this is a prefix of u */
  def <=(u: URI): Boolean = u.toString.startsWith(this.toString)

  /** makes a URI relative to this one */
  def relativize(u: URI): URI =
    if (scheme != u.scheme) u
    else if (authority != u.authority) URI(None, u.authority, u.path, u.absolute, u.query, u.fragment)
    else if (path != u.path) {
      if (absolute == u.absolute && path != Nil) {
        val max = path.length - 1
        var i = 0
        while (i <= max && u.path.startsWith(path.take(i + 1))) i += 1
        // now path.take(i) == u.path.take(i)
        URI(None, None, Range(0, max - i).toList.map(_ => "..") ::: u.path.drop(i), abs = false, u.query, u.fragment)
      } else
        URI(None, None, u.path, u.absolute, u.query, u.fragment)
    } else if (query != u.query) URI(None, None, Nil, abs = false, u.query, u.fragment)
    else if (fragment != u.fragment) URI(None, None, Nil, abs = false, None, u.fragment)
    else URI(None, None, Nil, abs = false, None, None)

  /**
   * changes the (file) extension of the last path segment (if any) to ext
   * extension is added if none exists
   */
  def setExtension(ext: String): URI =
    if (path.nonEmpty) {
      val oldLast = path.last
      val newLast = File(oldLast).setExtension(ext).segments.last
      copy(path = path.init ::: List(newLast))
    } else
      this

  /** parses a URI and resolves it against this */
  def resolve(s: String): URI = resolve(URI(s))

  private def merge(p: List[String], q: List[String]) = dodots(if (p.isEmpty) q else p.init ::: q)
  private def dodots(p : List[String]) = p.foldLeft[List[String]](Nil)((q,cur) =>
    if (cur == ".") q
    else if (cur == "..") if (q.isEmpty) Nil else q.tail
    else cur :: q
  ).reverse
  /** resolves a URI against this one (not using the java.net.URI resolution algorithm, which is buggy when u has no scheme, authority, path) */
  // URI RFC calls dodots in more situations during resolution, but it seems not crucial in practice
  def resolve(u: URI): URI = {
    if      (u.scheme.isDefined)    u
    else if (u.authority.isDefined) URI(scheme, u.authority, u.path,              u.absolute,                      u.query, u.fragment)
    else if (u.absolute)            URI(scheme, authority,   u.path,              u.absolute,                      u.query, u.fragment)
    else if (u.path.nonEmpty)       URI(scheme, authority,   merge(path, u.path), absolute || authority.isDefined, u.query, u.fragment)
    else if (u.query.isDefined)     URI(scheme, authority,   path,                absolute,                        u.query, u.fragment)
    else if (u.fragment.isDefined)  URI(scheme, authority,   path,                absolute,                        query,   u.fragment)
    else                            this
  }

  /** removes an empty trailing segment, which results from a trailing / */
  def pathNoTrailingSlash: List[String] = if (path.endsWith(List(""))) path.init else path

  /** returns the whole path as a string (/-separated, possibly with a leading /) */
  def pathAsString: String = {
    val tmp = path.mkString(if (absolute) "/" else "", "/", "")
    tmp.indexOf(";") match {
      case -1 => tmp
      case i => tmp.substring(0, i)
    }
  }

  /** convenience: the scheme or null */
  def schemeNull: String = scheme.orNull

  /** convenience: the authority or null */
  def authorityNull: String = authority.orNull

  /** converts MMT's implementation to Java's (buggy) implementation of URIs */
  def toJava: net.URI = new net.URI(schemeNull, authorityNull, pathAsString, query.orNull, fragment.orNull)

  private def mkS(p: String, s: Option[String]) = s.map(p+_).getOrElse("")
  override def toString: String = scheme.map(_+":").getOrElse("") + mkS("//", authority) + pathAsString + mkS("?",query) + mkS("#",fragment)
}

object URI {
  private val pattern = java.util.regex.Pattern.compile("^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?")

  // pattern taken from RFC 3986   /** transforms a Java URI into a URI */
  def apply(uri: java.net.URI): URI = URI(uri.toString)

  /** parses a URI (using the regular expression from the RFC) */
  def apply(s: String): URI = {
    val m = pattern.matcher(s)
    if (!m.matches)
      throw new net.URISyntaxException(s, "malformed URI reference")
    val scheme = Option(m.group(2))
    val authority = Option(m.group(4))
    val jpath = m.group(5)
    val (pathString, absolute) = {
      if (jpath.startsWith("/")) (jpath.substring(1), true)
      else (jpath, false)
    }
    var path = pathString.split("/", -1).toList
    if (path == List("")) //note: split returns at least List(""), never Nil
      path = Nil
    val query = Option(m.group(7))
    val fragment = Option(m.group(9))
    URI(scheme, authority, path, absolute, query, fragment)
  }

  /** returns a relative URI with scheme and authority only */
  def apply(s: String, a: String): URI = URI(Some(s), Some(a), abs = true)

  /** returns an absolute URI with scheme, authority, and path */
  def apply(s: String, a: String, p: List[String]): URI = URI(Some(s), Some(a), p, abs = true)

  /** returns a URI with scheme, authority, absolute path, and query */
  def apply(scheme: String, authority: String, path: List[String], query: String): URI =
    (URI(scheme, authority) / path) ? query

  /** returns a URI with a scheme only */
  def scheme(s: String): URI = URI(Some(s), None)

  def empty: URI = URI(None, None)

  /** the URI "file:" */
  val file = scheme("file")
  /** the URI "http:" */
  val http = scheme("http")
  /** the URI "https:" */
  val https = scheme("https")
  /** returns a URI with no scheme or authority and relative path */
  def relative(path: String*): URI = URI(None, None, path.toList, abs = false)

  implicit def toJava(u: URI): java.net.URI = u.toJava

  implicit def fromJava(u: java.net.URI): URI = apply(u)

  def get(uri: URI): java.io.InputStream = {
    val url = uri.toURL
    val conn = url.openConnection
    if (List("https","http") contains url.getProtocol) {
      val httpConn = conn.asInstanceOf[java.net.HttpURLConnection]
      val resp = httpConn.getResponseCode
      // setFollowRedirects does not actually follow redirects
      if (resp.toString.startsWith("30")) {
        val redirectURL = URI(conn.getHeaderField("Location"))
        return get(redirectURL)
      }
      if (resp.toString.startsWith("40")) {
        throw GeneralError("download of " + url + " failed")
      }
    }
    conn.getInputStream
  }

  /** URLDecoder.decode without second argument is deprecated and leads to crashes on more recent JDKs */
  def decode(url: String) = java.net.URLDecoder.decode(url, "UTF-8")
}
