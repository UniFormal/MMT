package jomdoc.utils
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
         case "visib" => namespace("omdoc")
         case "om" => "http://www.openmath.org/OpenMath"
         case "xhtml" => "http://www.w3.org/1999/xhtml"
         case "html" => "http://www.w3.org/TR/REC-html40"
         case "mathml" => "http://www.w3.org/1998/Math/MathML"
      }
   }
   
   case class URI(val uri : java.net.URI) {
      def this(s : String) = this(new java.net.URI(s))
      def this(scheme : String, authority : String, path : String, query : String) = this(new java.net.URI(scheme, authority, path, query, null))
      private def up(s : String) : String = {
      	val p = s.lastIndexOf("/")
         if (p == -1) ""
         else if (p == 0) "/"
         else s.substring(0,p)
      }
      def /(n : String) = new URI(this.getScheme, this.getAuthority, this.getPath + "/" + n, null)
      def path = uri.getPath.split("/").toList match {
    	  case List("") => Nil
    	  case l => l
      }
      def ^ : URI = new URI(this.getScheme, this.getAuthority, up(this.getPath), null)      
      def resolve(s : String) : URI = resolve(new java.net.URI(s))
      def resolve(u : java.net.URI) : URI = {
         //resolve implements old URI RFC, therefore special case for query-only URI needed
         if (u.getScheme == null && u.getAuthority == null && u.getPath == "")
            new URI(new java.net.URI(uri.getScheme, uri.getAuthority, uri.getPath, u.getQuery, u.getFragment))
         else
            new URI(uri.resolve(u))
      }
      override def toString = uri.toString
   }
   object URI {
      implicit def toURI(u : URI) : java.net.URI = u.uri
   }
   
   implicit def javaURItoMyURI(u : java.net.URI) = new URI(u)
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