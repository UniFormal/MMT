package info.kwarc.mmt.api.utils
import scala.xml.{Node, PrettyPrinter}

object xml {
   /** reads an XML file and returns the first Node in it */
   def readFile(file : java.io.File) : scala.xml.Node = {
      val src = scala.io.Source.fromFile(file, "utf-8") // utf-8 forced due to error with default codec
      val cp = scala.xml.parsing.ConstructingParser.fromSource(src, false)
      val N = cp.document()(0)
      src.close
      N
   }
   
   /** writes an XML Node to a file */
   def writeFile(N : scala.xml.Node, file : java.io.File) {
      file.getParentFile.mkdirs
      val out = new java.io.BufferedWriter(new java.io.OutputStreamWriter(new java.io.FileOutputStream(file),"UTF-8"))
      out.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + "\n" + new PrettyPrinter(160,2).format(N))
      out.close
   }
  
   /** returns attribute value, "" if attribute not present */
   def attr(N : Node, att : String) : String = {
      val l = N.attribute(att).getOrElse(Nil).toList
      l.map(_.text).mkString("","","")
   }
   /** returns attribute value, default if attribute not present */
   def attr(N : Node, att : String, default : String) : String = {
      val l = N.attribute(att).map(_.toList)
      if (l.isEmpty)
         default
      else
         l.map(_.text).mkString("","","")
   }
   
   // decode all occurrences of %HH
   def decodeURI(s: String) : String = {
      var in = s
      var out = ""
      // invariant: s = out + in
      while (in != "") {
         val c = in(0)
         if (c == '%') {
            val hh = Integer.parseInt(in.substring(1,3), 16).toChar
            in = in.substring(3)
            out = out + hh
         }
         else {
            in = in.substring(1)
            out = out + c
         }
      }
      out
   }
   // encode all occurrences of ?/%# as %HH
   def encodeURI(s: String) : String = {
      var in = s
      var out = ""
      // invariant: s = out + in
      while (in != "") {
         val c = in(0)
         val enc = c match {
            case '?' => "%3F"
            case '/' => "%2F"
            case '%' => "%25"
            case '#' => "%23"
            case c => c
         }
         in = in.substring(1)
         out = out + enc
      }
      out
   }
   
   def post(url: java.net.URL, input: Node) : Node = {
      val conn = url.openConnection()// returns java.net.HttpURLConnection if url is http
      conn.setDoOutput(true);
      val wr = new java.io.OutputStreamWriter(conn.getOutputStream())
      wr.write(input.toString)   // this automatically sets the request method to POST
      wr.flush()
      val src = scala.io.Source.fromInputStream(conn.getInputStream(), "UTF-8")
      val output = scala.xml.parsing.ConstructingParser.fromSource(src, false).document()
      wr.close()
      src.asInstanceOf[scala.io.BufferedSource].close
      output(0)
   }
   
   /**
    * Checks whether an XML element has illegal attribute keys.
    * Prefixed attributes are ignored.
    * @param md the attributes to be checked
    * @param the allowed keys
    * @return the illegal keys
    */
   def checkKeys(md : scala.xml.MetaData, keys : List[String]) : List[String] = md match {
      case scala.xml.Null => Nil
      case md : scala.xml.PrefixedAttribute => checkKeys(md.next, keys)
      case md : scala.xml.UnprefixedAttribute =>
         List(md.key).filterNot(keys.contains) ::: checkKeys(md.next, keys)
   }
   
   /** common namespaces */
   def namespace (ns : String) : String = {
      ns match {
         case "xml" => "http://www.w3.org/XML/1998/namespace"
         case "omdoc" => "http://www.mathweb.org/omdoc"
         case "jobad" => "http://omdoc.org/presentation"
         case "visib" => namespace("omdoc")
         case "om" => "http://www.openmath.org/OpenMath"
         case "xhtml" => "http://www.w3.org/1999/xhtml"
         case "html" => "http://www.w3.org/1999/xhtml"
         case "mathml" => "http://www.w3.org/1998/Math/MathML"
      }
   }
}

/** Custom implementation of the URI RFC that's better than java.net.URI
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
/* potentially useful, but currently not needed   
   def relativize(u: URI): URI =
          if (scheme != u.scheme) u
     else if (authority != u.authority) URI(None, u.authority, u.path, u.absolute, u.query, u.fragment)
     else if (path != u.path) URI(None, None, u.path, u.absolute, u.query, u.fragment)
     else if (query != u.query) URI(None, None, Nil, false, u.query, u.fragment)
     else if (fragment != u.fragment) URI(None, None, Nil, false, None, u.fragment)
     else URI(None, None, Nil, false, None, None)
     */
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
   def apply(uri : java.net.URI) : URI = {
      val scheme = nullToNone(uri.getScheme)
      val authority = nullToNone(uri.getAuthority)
      val jpath = uri.getPath
      val (pathString, absolute) = {
         if (jpath.startsWith("/")) (jpath.substring(1), true)
         else (jpath, false)
      }
      var path = pathString.split("/",-1).toList
      if (path == List(""))  //note: split returns at least List(""), never Nil
         path = Nil
      val query = nullToNone(uri.getQuery)
      val fragment = nullToNone(uri.getFragment)
      URI(scheme, authority, path, absolute, query, fragment)
   }
   /** parses a URI (using the java.net.URI multiple-argument parser) */
   def apply(s : String) : URI = {
       val m : java.util.regex.Matcher = java.util.regex.Pattern.compile("^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?").matcher(s) // pattern taken from RFC 3986
       if (m.matches == false)
          throw new java.net.URISyntaxException(s, "malformed URI reference")
       return URI(new java.net.URI(m.group(2), m.group(4), m.group(5), m.group(7), m.group(9)))
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