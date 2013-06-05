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
   def get(url: java.net.URL): Node = {
      val conn = url.openConnection()
      val src = scala.io.Source.fromInputStream(conn.getInputStream(), "UTF-8")
      val output = scala.xml.parsing.ConstructingParser.fromSource(src, false).document()
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

