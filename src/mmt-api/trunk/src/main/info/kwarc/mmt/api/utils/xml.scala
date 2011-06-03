package info.kwarc.mmt.api.utils
import scala.xml.Node

object xml {
   def readFile(file : java.io.File) : scala.xml.Node = {
      val src = scala.io.Source.fromFile(file, "utf-8") // utf-8 forced due to error with default codec
      val cp = scala.xml.parsing.ConstructingParser.fromSource(src, false)
      val N = cp.document()(0)
      src.close
      N
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
         case "html" => "http://www.w3.org/TR/REC-html40"
         case "mathml" => "http://www.w3.org/1998/Math/MathML"
      }
   }
}

case class URI(scheme: Option[String], authority: Option[String], path: List[String], absolute: Boolean, query: Option[String], fragment: Option[String]) {
   def /(n : String) : URI = this / List(n) // drops query and fragment
   def /(p : List[String]) : URI = {
      val abs = absolute || (authority != None && path == Nil) // if this has authority and empty path, make the path absolute
      URI(scheme, authority, path ::: p, abs, None, None) // drops query and fragment
   }
   def ?(q: String) = URI(scheme, authority, path, absolute, Some(q), None) // drops fragment
   def addFragment(f: String) = URI(scheme, authority, path, absolute, query, Some(f))
   def ^ : URI = URI(scheme, authority, if (path.isEmpty) Nil else path.init, absolute, None, None) // drops query and fragment
   def resolve(s : String) : URI = resolve(URI(s))
   def resolve(u : URI) : URI = {
      //resolve implements old URI RFC, therefore special case for query-only URI needed
      if (u.scheme == None && u.authority == None && u.path == Nil)
         URI(scheme, authority, path, absolute, u.query, u.fragment)
      else
         URI(toJava.resolve(u.toJava))
   }
   def pathAsString : String = path.mkString(if (absolute) "/" else "", "/", "")
   /*def resolve(u : java.net.URI) : URI = {
      //resolve implements old URI RFC, therefore special case for query-only URI needed
      if (u.getScheme == null && u.getAuthority == null && u.getPath == "")
         URI(scheme, authority, path, absolute, URI.nullToNone(u.getQuery), URI.nullToNone(u.getFragment))
      else
         URI(toJava.resolve(u))
   }*/
   def toJava = new java.net.URI(scheme.getOrElse(null), authority.getOrElse(null), pathAsString, query.getOrElse(null), fragment.getOrElse(null))
   override def toString = toJava.toString
}
   
object URI {
   def nullToNone(s: String) = if (s == null) None else Some(s)
   def apply(uri : java.net.URI) : URI = {
      val scheme = nullToNone(uri.getScheme)
      val authority = nullToNone(uri.getAuthority)
      var jpath = uri.getPath
      val absolute = jpath.startsWith("/")
      var path = jpath.split("/",-1).toList
      if (path == List(""))  //note: split returns at least List(""), never Nil
         path = Nil
      if (absolute)
         path = path.drop(1)
      val query = nullToNone(uri.getQuery)
      val fragment = nullToNone(uri.getFragment)
      URI(scheme, authority, path, absolute, query, fragment)
   }
   def apply(s : String) : URI = apply(new java.net.URI(s))
   def apply(s: String, a: String) : URI = URI(Some(s), Some(a), Nil, false, None, None)
   def apply(scheme : String, authority : String, path : List[String], query : String) : URI =
      (URI(scheme, authority) / path) ? query
   def file(absolute : Boolean, path: String*) = URI(Some("file"), None, path.toList, absolute, None, None)
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