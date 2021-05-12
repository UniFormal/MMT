package info.kwarc.mmt.api.utils
import info.kwarc.mmt.api.LocalName

import scala.xml._
import scala.io.Source

object xml {
   /* xml header followed by a newline */
   val header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

   def openTag(label: String, atts: List[(String,String)], close: Boolean = false) = {
      val attsS = atts.map(a => " " + a._1 + "=\"" + Utility.escape(a._2) + "\"").mkString("")
      "<" + label + attsS + (if (close) "/" else "") + ">"
   }
   def closeTag(label: String) = "</" + label + ">"
   def element(label: String, atts: List[(String,String)], body: String) = {
      openTag(label, atts) + Utility.escape(body) + closeTag(label)
   }

   case class XMLError(s: String) extends info.kwarc.mmt.api.Error(s)

   /** reads an XML file and returns the first Node in it */
   def readFile(file : File) : scala.xml.Node = {
      val src = Source.fromFile(file.toJava, "utf-8")
      val cp = parsing.ConstructingParser.fromSource(src, false)
      val N = try {
         cp.document()(0)
      } catch {case e: Exception =>
         throw XMLError("XML error while parsing " + file + ": " + e.getMessage).setCausedBy(e)
      } finally {
         src.close
      }
      N
   }

   /** writes an XML Node to a file
    *
    * overwrites existing files, creates directories if necessary
    */
   def writeFile(N : scala.xml.Node, file : File) {
      val out = File.Writer(file)
      // do not use pretty printing - whitespace matters
      out.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + "\n" + N.toString)
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
   /** Turns an attribute into a LocalName **/
   def attrL(N : Node, att: String) : LocalName = {
      val l = N.attribute(att).getOrElse(Nil).toList
      LocalName.parse(l.map(_.text).mkString("","",""))
   }

   /** like attr but forces integer value
    */
   def attrInt(N: Node, att: String, exc: String => Throwable) : Int = {
      val s = attr(N, att)
      try {s.toInt}
      catch {case _ : Throwable => throw exc("illegal attribute, expected integer, found: " + s)}
   }
   /** adds an attribute to a node */
   def addAttr(N: Elem, key: String, value: String): Elem = {
      N % (new UnprefixedAttribute(key, value, scala.xml.Null))
   }

   /** dual of addAttrOrChild
    *  @return the node without the child (if any) and the attribute/child (without the key), empty string by default
    */
   def getAttrOrChild(n: Node, key: String): (Node, Union[String,Node]) = {
      if (n.attribute(key).isDefined) {
         (n, Left(attr(n, key)))
      } else {
          val (newnode, cOpt) = splitOffChild(n, key)
          val value: Union[String,Node] = cOpt match {
             case Some(c) => Right(c)
             case None => Left("")
          }
          (newnode, value)
      }
   }

   /** adds either key="value" or <key>value</key> to a node, depending on whether the value is a string or a node */
   def addAttrOrChild(n: Elem, key: String, value: Union[String,Node]) = value match {
      case Left(s) => addAttr(n, key, s)
      case Right(c) =>
        val wc = Elem(null, key, Null, n.scope, true, c)
        n.copy(child = wc ++ n.child)
   }

   /** removes the child with label "label" from "node" (if any), returns the remaining node and that child */
   def splitOffChild(node: Node, label : String) : (Node, Option[Node]) = node match {
       case scala.xml.Elem(p,l,a,s,cs @ _*) =>
           var n : Option[Node] = None
           val cs2 = cs flatMap {e =>
              if (e.label == label) {
                 n = Some(e)
                 Nil
              } else
                 e
           }
           (scala.xml.Elem(p,l,a,s,true,cs2 : _*), n)
       case n => (n, None)
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

   /** removes empty direct children of a node */
   def trimOneLevel(n : Node) : Node = n match {
     case e : Elem =>
       val nonWSchild = e.child.filter(c => !Utility.trimProper(c).isEmpty)
       e.copy(child = nonWSchild)
     case _ => n
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
      val conn = url.openConnection// returns java.net.HttpURLConnection if url is http
      conn.setDoOutput(true)
      conn.setRequestProperty("content-type", "text/xml")
      val wr = new java.io.OutputStreamWriter(conn.getOutputStream)
      wr.write(input.toString)   // this automatically sets the request method to POST
      wr.flush
      val src = scala.io.Source.fromInputStream(conn.getInputStream, "UTF-8") // always a BufferedSource
      val output = scala.xml.parsing.ConstructingParser.fromSource(src, false).document()
      wr.close
      src.asInstanceOf[scala.io.BufferedSource].close
      output(0)
   }
   /** dereference a URL and return XML */
   def get(url: URI): Node = {
      val input = URI.get(url)
      val src = scala.io.Source.fromInputStream(input, "UTF-8")
      try {
         val output = scala.xml.parsing.ConstructingParser.fromSource(src, false).document()
         output(0)
      } finally {
         input.close
         src.asInstanceOf[scala.io.BufferedSource].close
      }
   }

   /** common namespaces */
   def namespace(ns : String) : String = {
      ns match {
         case "xml" => "http://www.w3.org/XML/1998/namespace"
         case "omdoc" => "http://www.mathweb.org/omdoc"
         case "om" => "http://www.openmath.org/OpenMath"
         case "xhtml" => "http://www.w3.org/1999/xhtml"
         case "html" => "http://www.w3.org/1999/xhtml"
         case "mathml" => "http://www.w3.org/1998/Math/MathML"
         case "mws" => "http://search.mathweb.org/ns"
         case "jobad" => "http://www.mathhub.info/jobad" // TODO: Not sure which one this is!
         case _ => throw new Exception("Unknown XML Namespace " + ns)
      }
   }
}

