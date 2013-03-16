package info.kwarc.mmt.twelf

import java.util.regex.{Pattern, Matcher}


case class URI(var uri : java.net.URI) {
    /** Create an valid URI where the illegal or non-ascii character in path, query and fragment are quoted
      * @return a valid, %-encoded URI
      * @throws java.net.URISyntaxException if the URI violates RFC 2396 or RFC 3986
      */
    def this(s : String) = this(URI.encode(s))
    
    /** Create an valid URI where the illegal or non-ascii character in path, query and fragment are quoted
      * @return a valid, %-encoded URI
      * @throws java.net.URISyntaxException if both a scheme and a path are given but the path is relative, if the URI string constructed from the given components violates RFC 2396, or if the authority component of the string is present but cannot be parsed as a server-based authority 
      */
    def this(scheme : String, authority : String, path : String, query : String) = this(new java.net.URI(scheme, authority, path, query, null))
    
    /** Return the unencoded URI */
    override def toString = java.net.URLDecoder.decode(uri.toString.replace("+", "%2B"), "UTF-8")
    
    private def up(s : String) : String = {
      	 val p = s.lastIndexOf("/")
         if (p == -1) ""
         else if (p == 0) "/"
         else s.substring(0,p)
    }
    
    def /(n : String) : URI = new URI(uri.toString + "/" + n)
    def ?(n : String) : URI = new URI(uri.toString + "?" + n)
    def ^ : URI = new URI(uri.getScheme, uri.getAuthority, up(uri.getPath), null)
    def resolve(s : String) : URI = resolve(new java.net.URI(s))
    def resolve(u : java.net.URI) : URI = {
         //resolve implements old URI RFC, therefore special case for query-only URI needed
         if (u.getScheme == null && u.getAuthority == null && u.getPath == "")
            new URI(new java.net.URI(uri.getScheme, uri.getAuthority, uri.getPath, u.getQuery, u.getFragment))
         else
            new URI(uri.resolve(u))
    }
    def getBase : URI = new URI(uri.getScheme, uri.getAuthority, uri.getPath, null)
    def normalize : java.net.URI = uri.normalize
}



object URI {
    /** Create an valid URI where the illegal or non-ascii character in path, query and fragment are quoted
      * @return a valid, %-encoded URI
      * @throws java.net.URISyntaxException if the URI violates RFC 2396 or RFC 3986
      */
    private def encode(s : String) : java.net.URI = {
        val m : Matcher = Pattern.compile("^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?").matcher(s) // pattern taken from RFC 3986
        if (m.matches == false)
            throw new java.net.URISyntaxException(s, "malformed URI reference")
        return new java.net.URI(m.group(2), m.group(4), m.group(5), m.group(7), m.group(9))
    }
    implicit def toMyURI(u : URI) : java.net.URI = u.uri
    implicit def javaURItoMyURI(u : java.net.URI) : URI = new URI(u)
}


/*
object testme extends Application {
  //val u = new URI("file:/C:/courses/Twelf/logics/a?m")
  //println(URI(u) / "s")
  //println(URI(u) ? "s")
  //println(u.^ + "\n" + new URI("/a").^ + "\n" + new URI("/").^ + "\n" + new URI("/c:").^ + "\n" + new URI("/c:/a").^)
  val tst = new java.net.URL("http://a?q→")
  val s = "www.test.org/a^?q #f+a"
  val u = new info.kwarc.mmt.lf.URI(s)
  println(s + "     " + u.toString)

  println(isTaken(8080))
}*/