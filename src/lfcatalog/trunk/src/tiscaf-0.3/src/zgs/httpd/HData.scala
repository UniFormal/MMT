package zgs.httpd

private trait HData // ant

object HStatus extends Enumeration {
  val
    /* 100 */
    Continue,
    SwitchingProtocol,
    /* 200*/
    OK,
    Created,
    Accepted,
    NotAuthoritativeInformation,
    NoContent,
    ResetContent,
    PartialContent,
    /* 300 */
    MultipleChoices,
    MovedPermanently,
    Found,
    SeeOther,
    NotModified,
    UseProxy,
    Unused306,
    TemporaryRedirect,
    /* 400 */
    BadRequest,
    Unauthorized,
    PaymentRequired,
    Forbidden,
    NotFound,
    MethodNotAllowed,
    NotAcceptable,
    ProxyAuthentificationRequired,
    RequestTimeout,
    Conflict,
    Gone,
    LengthRequired,
    PreconditionFailed,
    RequestEntityTooLarge,
    RequestUriTooLong,
    UnsupportedMediaType,
    RequestRangeNotSatisfiable,
    ExpectationFailed,
    UpgradeRequired,
    /* 500 */
    InternalServerError,
    NotImplemented,
    BadGateway,
    ServiceUnavailable,
    GatewayTimeout,
    VersionNotSupported = Value
  
  val strings = Map (
    Continue -> ("100", "Continue"),
    SwitchingProtocol -> ("101", "Switching Protocol"),
    /* 200*/
    OK -> ("200", "OK"),
    Created -> ("201", "Created"),
    Accepted -> ("202", "Accepted"),
    NotAuthoritativeInformation -> ("203", "Not Authoritative Information"),
    NoContent -> ("204", "No Content"),
    ResetContent -> ("205", "Reset Content"),
    PartialContent -> ("206", "Partial Content"),
    /* 300 */
    MultipleChoices -> ("300", "Multiple Choices"),
    MovedPermanently -> ("301", "Moved Permanently"),
    Found -> ("302", "Found"),
    SeeOther -> ("303", "See Other"),
    NotModified -> ("304", "Not Modified"),
    UseProxy -> ("305", "Use Proxy"),
    Unused306 -> ("306", "unused"),
    TemporaryRedirect -> ("307", "Temporary Redirect"),
    /* 400 */
    BadRequest -> ("400", "Bad Request"),
    Unauthorized -> ("401", "Unauthorized"),
    PaymentRequired -> ("402", "Payment Required"),
    Forbidden -> ("403", "Forbidden"),
    NotFound -> ("404", "Not Found"),
    MethodNotAllowed -> ("405", "Method Not Allowed"),
    NotAcceptable -> ("406", "Not Acceptable"),
    ProxyAuthentificationRequired -> ("407", "Proxy Authentification Required"),
    RequestTimeout -> ("408", "Request Timeout"),
    Conflict -> ("409", "Conflict"),
    Gone -> ("410", "Gone"),
    LengthRequired -> ("411", "Length Required"),
    PreconditionFailed -> ("412", "Precondition Failed"),
    RequestEntityTooLarge -> ("413", "Request Entity Too Large"),
    RequestUriTooLong -> ("414", "Request URI TooLong"),
    UnsupportedMediaType -> ("415", "Unsupported Media Type"),
    RequestRangeNotSatisfiable -> ("416", "Request Range Not Satisfiable"),
    ExpectationFailed -> ("417", "Expectation Failed"),
    UpgradeRequired -> ("426", "Upgrade Required"),
    /* 500 */
    InternalServerError -> ("500", "Internal Server Error"),
    NotImplemented -> ("501", "Not Implemented"),
    BadGateway -> ("502", "Bad Gateway"),
    ServiceUnavailable -> ("503", "Service Unavailable"),
    GatewayTimeout -> ("504", "Gateway Timeout"),
    VersionNotSupported -> ("505", "Version Not Supported")
  )
  
  def asString(v : Value, msg : String) = strings(v)._1 + " " + msg 
  def asString(v : Value) = { val s = strings(v); s._1 + " " + s._2 }
}


protected object HFields {
  
  // ordered header fields names
  val list = List(
    /*----- unordered -----*/
    "date", // general
    "server", // server
    /* general */
    //"cache-control",
    "connection",
    "keep-alive",
    //"pragma",
    //"trailer",
    "transfer-encoding",
    //"upgrade",
    "user-agent",
    //"warning",
    /*----- response -----*/
    "accept-ranges",
    //"age",
    //"authentication-info",
    //"etag",
    "location",
    //"meter",
    //"proxy-authenticate",
    //"retry-after",
    "set-cookie",
    //"vary",
    //"www-authenticate",
    /*----- entity -----*/
    //"allow",
    "content-encoding",
    "content-language",
    "content-length",
    "content-location",
    //"content-md5",
    "content-range",
    "content-type",
    //"expires",
    "last-modified"
  )
  
  def toCap(in : String) : String = in.split("-", 2).map(_.capitalize).mkString("-")
}


object HMime {
  
  val exts = Map(
    "html" -> "text/html",
    "htm" -> "text/html",
    "js" -> "application/x-javascript",
    "css" -> "text/css ",
    "shtml" -> "text/html",
    "gif" -> "image/gif",
    "ico" -> "image/x-icon",
    "jpeg" -> "image/jpeg ",
    "jpg" -> "image/jpeg ",
    "png" -> "image/png",
    "pdf" -> "application/pdf",
    "zip" -> "application/zip",
    "xhtml" -> "application/xhtml+xml",
    "xht" -> "application/xhtml+xml",
    "svg" -> "image/svg+xml",
    "svgz" -> "image/svg+xml",
    "tiff" -> "image/tiff",
    "tif" -> "image/tiff",
    "djvu" -> "image/vnd.djvu",
    "djv" -> "image/vnd.djvu",
    "bmp" -> "image/x-ms-bmp",
    "asc" -> "text/plain",
    "txt" -> "text/plain",
    "text" -> "text/plain",
    "diff" -> "text/plain",
    "scala" -> "text/plain",
    "xml" -> "application/xml",
    "xsl" -> "application/xml",
    "tgz" -> "application/x-gtar",
    "jar" -> "application/java-archive",
    "class" -> "application/java-vm",
    "flac" -> "application/x-flac",
    "ogg" -> "application/ogg",
    "wav" -> "audio/x-wav",
    "pgp" -> "application/pgp-signatur",
    "ps" -> "application/postscript",
    "eps" -> "application/postscript",
    "rar" -> "application/rar",
    "rdf" -> "application/rdf+xml",
    "rss" -> "application/rss+xml",
    "torrent" -> "application/x-bittorrent",
    "deb" -> "application/x-debian-package",
    "udeb" -> "application/x-debian-package",
    "dvi" -> "application/x-dvi",
    "gnumeric" -> "application/x-gnumeric",
    "iso" -> "application/x-iso9660-image",
    "jnlp" -> "application/x-java-jnlp-file",
    "latex" -> "application/x-latex",
    "rpm" -> "application/x-redhat-package-manager",
    "tar" -> "application/x-tar",
    "texinfo" -> "application/x-texinfo",
    "texi" -> "application/x-texinfo",
    "man" -> "application/x-troff-man",
    "h++" -> "text/x-c++hdr",
    "hpp" -> "text/x-c++hdr",
    "hxx" -> "text/x-c++hdr",
    "hh" -> "text/x-c++hdr",
    "c++" -> "text/x-c++src",
    "cpp" -> "text/x-c++src",
    "cxx" -> "text/x-c++src",
    "cc" -> "text/x-c++src",
    "h" -> "text/x-chdr",
    "hs" -> "text/x-haskell",
    "java" -> "text/x-java",
    "lhs" -> "text/x-literate-haskell",
    "pas" -> "text/x-pascal",
    "py" -> "text/x-python",
    "xul" -> "application/vnd.mozilla.xul+xml",
    "odc" -> "application/vnd.oasis.opendocument.chart",
    "odb" -> "application/vnd.oasis.opendocument.database",
    "odf" -> "application/vnd.oasis.opendocument.formula",
    "odg" -> "application/vnd.oasis.opendocument.graphics",
    "odi" -> "application/vnd.oasis.opendocument.image",
    "odp" -> "application/vnd.oasis.opendocument.presentation",
    "ods" -> "application/vnd.oasis.opendocument.spreadsheet",
    "odt" -> "application/vnd.oasis.opendocument.text",
    "abw" -> "application/x-abiword"
  )
  
  val gzipable = List(
    "text/html",
    "application/x-javascript",
    "text/css ",
    "text/plain",
    "application/xml",
    "application/xhtml+xml",
    "image/svg+xml",
    "application/rdf+xml",
    "application/rss+xml",
    "text/x-c++hdr",
    "text/x-c++src",
    "text/x-chdr",
    "text/x-haskell",
    "text/x-java",
    "text/x-python"
  )
}

