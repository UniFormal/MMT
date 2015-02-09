package info.kwarc.mmt.api.utils
import scala.xml._
import scala.io.Source

object xml {
   def openTag(label: String, atts: List[(String,String)], close: Boolean = false) = {
      val attsS = atts.map(a => " " + a._1 + "=\"" + Utility.escape(a._2) + "\"").mkString("")
      "<" + label + attsS + (if (close) "/" else "") + ">"
   }
   def closeTag(label: String) = "</" + label + ">" 
   def element(label: String, atts: List[(String,String)], body: String) = {
      openTag(label, atts) + Utility.escape(body) + closeTag(label)
   }

   case class XMLError(s: String) extends java.lang.Throwable(s)
   
   /** reads an XML file and returns the first Node in it */
   def readFile(file : File) : scala.xml.Node = {
      val src = Source.fromFile(file.toJava, "utf-8") // utf-8 forced due to error with default codec
      val cp = parsing.ConstructingParser.fromSource(src, false)
      val N = try {
         cp.document()(0)
      } catch {case e: Exception =>
         throw XMLError("XML error while parsing " + file + ": " + e.getMessage)
      }
      src.close
      N
   }

   /** writes an XML Node to a file
    *  
    * overwrites existing files, creates directories if necessary
    */
   def writeFile(N : scala.xml.Node, file : File) {
      val out = File.Writer(file)
      val s = new PrettyPrinter(160,2).format(N)
      out.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + "\n" + s)
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
   /** like attr but forces integer value
    */
   def attrInt(N: Node, att: String, exc: String => Throwable) : Int = {
      val s = attr(N, att)
      try {s.toInt}
      catch {case _ : Throwable => throw exc("illegal attribute, expected integer, found: " + s)}
   }
   /** adds an attribute to a node */
   def addAttr(N: scala.xml.Elem, key: String, value: String): scala.xml.Elem = {
      N % (new scala.xml.UnprefixedAttribute(key, value, scala.xml.Null))
   }
   
   /** returns the list of namespaces of a node */
   def namespaces(nb: NamespaceBinding, seen: List[String] = Nil): List[(String,String)] = nb match {
      case TopScope => Nil
      case NamespaceBinding(p,u,parent) =>
         val pNonNull = if (p == null) "" else p
         val thisOne = if (seen contains pNonNull)
            // skip prefixes that are overridden by deeper ones
            Nil
         else
            List((pNonNull,u))
         thisOne ::: namespaces(parent, pNonNull :: seen)
   }

   // decode all occurrences of %HH
   def decodeURI(s: String) : String = {
      var in = s
      var out = ""
      // invariant: s = out + in
      while (in != "") {
         val c = in(0)
         if (c == '%') {
            val hh = try {Integer.parseInt(in.substring(1,3), 16).toChar}
                     catch {case _ : Exception => throw XMLError("error decoding URI " + s)}
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
   def get(url: java.net.URL): Node = {
      val conn = url.openConnection()
      val src = scala.io.Source.fromInputStream(conn.getInputStream(), "UTF-8")
      val output = scala.xml.parsing.ConstructingParser.fromSource(src, false).document()
      src.asInstanceOf[scala.io.BufferedSource].close
      output(0)
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

