package info.kwarc.mmt.api.utils

 /** Custom implementation of the URI RFC that's better than java.net.URI
 * 
 * @param abs true if the path is absolute (ignored if scheme or authority are present) */
case class URI(scheme: Option[String], authority: Option[String], path: List[String] = Nil, private val abs: Boolean = false, query: Option[String] = None, fragment: Option[String] = None) {
   /** true if the path is absolute; automatically set to true if scheme or authority are present */ 
   val absolute = abs || scheme.isDefined || authority.isDefined
   /** drop path, query, fragment, append (absolute) path of length 1 */
   def !/(n : String) : URI = this !/ List(n)
   /** drop path, query, fragment, append (absolute) path */
   def !/(p : List[String]) : URI = {
      URI(scheme, authority, p, true, None, None)
   }
   /** drop query, fragment, append one segment to path */   
   def /(n : String) : URI = this / List(n)
   /** drop query, fragment, append to path
    *  path stays relative/absolute; but URI(_, Some(_), Nil, false, _, _) / _ turns path absolute
    *  trailing empty segment of this URI is dropped when appending
    */   
   def /(p : List[String]) : URI = URI(scheme, authority, pathNoTrailingSlash ::: p, absolute)
   /** drops query and fragment, drop last path segment (if any) */
   def ^ : URI = URI(scheme, authority, if (path.isEmpty) Nil else path.init, absolute)
   /** drops query and fragment and path */
   def ^! : URI = URI(scheme, authority)
   /** drop query, fragment, append query */
   def ?(q: String) = URI(scheme, authority, path, absolute, Some(q))
   /** drop fragment, append fragment */
   def ##(f: String) = URI(scheme, authority, path, absolute, query, Some(f))
   /** true iff this is a prefix of u */
   def <=(u: URI): Boolean = u.toString.startsWith(this.toString)
   /** makes a URI relative to this one */
   def relativize(u: URI): URI =
          if (scheme != u.scheme) u
     else if (authority != u.authority) URI(None, u.authority, u.path, u.absolute, u.query, u.fragment)
     else if (path != u.path) {
        if (absolute == u.absolute && path != Nil && u.path.startsWith(path.init))
           URI(None, None, u.path.drop(path.length-1), false, u.query, u.fragment)
        else
           URI(None, None, u.path, u.absolute, u.query, u.fragment)
     } else if (query != u.query) URI(None, None, Nil, false, u.query, u.fragment)
     else if (fragment != u.fragment) URI(None, None, Nil, false, None, u.fragment)
     else URI(None, None, Nil, false, None, None)
   /** parses a URI and resolves it against this */
   def resolve(s : String) : URI = resolve(URI(s))
   /** resolves a URI against this one (using the java.net.URI resolution algorithm except when u has no scheme, authority, path) */
   def resolve(u : URI) : URI = {
      //resolve implements old URI RFC, therefore special case for query-only URI needed
      if (u.scheme == None && u.authority == None && u.path == Nil)
         URI(scheme, authority, path, absolute, u.query, u.fragment)
      else
         URI(toJava.resolve(u.toJava))
   }
   /** removes an empty trailing segment, which results from a trailing / */
   val pathNoTrailingSlash = if (path.endsWith(List(""))) path.init else path
   /** returns the whole path as a string (/-separated, possibly with a leading /) */
   def pathAsString : String = {
     val tmp = path.mkString(if (absolute) "/" else "", "/", "")
     tmp.indexOf(";") match {
       case -1 => tmp
       case i => tmp.substring(0,i)
     }
   }
   
   /** convenience: the scheme or null */
   def schemeNull : String = scheme.getOrElse(null)
   /** convenience: the authority or null */
   def authorityNull : String = authority.getOrElse(null)

   def toJava = new java.net.URI(scheme.getOrElse(null), authority.getOrElse(null), pathAsString, query.getOrElse(null), fragment.getOrElse(null))
   override def toString = toJava.toString
}
   
object URI {
   private def nullToNone(s: String) = if (s == null) None else Some(s)
   /** transforms a Java URI into a URI */
   def apply(uri : java.net.URI) : URI = URI(uri.toString)
   /** parses a URI (using the regular expression from the RFC) */
   def apply(s : String) : URI = {
      val m : java.util.regex.Matcher = java.util.regex.Pattern.compile("^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?").matcher(s) // pattern taken from RFC 3986
      if (m.matches == false)
          throw new java.net.URISyntaxException(s, "malformed URI reference")
      val scheme = nullToNone(m.group(2))
      val authority = nullToNone(m.group(4))
      val jpath = m.group(5)
      val (pathString, absolute) = {
         if (jpath.startsWith("/")) (jpath.substring(1), true)
         else (jpath, false)
      }
      var path = pathString.split("/",-1).toList
      if (path == List(""))  //note: split returns at least List(""), never Nil
         path = Nil
      val query = nullToNone(m.group(7))
      val fragment = nullToNone(m.group(9))
      URI(scheme, authority, path, absolute, query, fragment)
   }
   /** returns a relative URI with scheme and authority only */
   def apply(s: String, a: String) : URI = URI(Some(s), Some(a))
   /** returns an absolute URI with scheme, authority, and path */
   def apply(s: String, a: String, p: List[String]) : URI = URI(Some(s), Some(a), p, true)
   /** returns a URI with scheme, authority, absolute path, and query */
   def apply(scheme : String, authority : String, path : List[String], query : String) : URI =
      (URI(scheme, authority) / path) ? query
   /** returns a URI with a scheme only */
   def scheme(s: String) = URI(Some(s), None)
   def empty = URI(None,None)
   /** the URI "file:" */
   val file = scheme("file")
   /** returns a URI with no scheme or authority and relative path */
   def relative(path: String*) = URI(None, None, path.toList, false)
   implicit def toJava(u : URI) : java.net.URI = u.toJava
   implicit def fromJava(u : java.net.URI) = apply(u)
}



/*
//split key=value into (key,value), missing value defaults to ""
def splitKeyVal(p : String) : (String, String) = {
   val l = p.split("=",2)
   if (l.length == 1)
      (l(0), "") else (l(0),l(1))
}
def Unsplit(q : Map[String,String]) = q.elements.map(x => x._1 + "=" + x._2).toList.myFold("")((x,y) => x + ";" + y)
*/