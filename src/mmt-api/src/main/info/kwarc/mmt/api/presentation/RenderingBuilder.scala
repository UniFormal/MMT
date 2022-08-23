package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._
import scala.xml._

/** A RenderingHandler collects output generated during presentation
 * @tparam A the return type, possibly Unit
 */
abstract class RenderingHandler {
   /** writes a string, to be implemented by subclasses */
   def write(s : String) : Unit
   /** shortcut for write */
   def <<[A <% String](a: A) = write(a)
   /** output a string */
   def apply(s : String): Unit = {
     write(s)
   }
   /** output an XML node */
   def apply(N : NodeSeq): Unit = {apply(N.toString)}

   // Convenience methods for rendering text
   /** output a string with line ending */
   def writeln(s: String): Unit = {apply(s + "\n")}

   /** wraps output in a pair of brackets */
   def wrap(before: String, after: String)(body: => Unit): Unit = {
      apply(before)
      body
      apply(after)
   }

   /*
   // Convenience methods for rendering XML
   /** begins an XML element, use empty prefix for unprefixed elements, content is provided by succeeding calls */
   def beginTag(prefix : String, label : String) {
     write("<" + getQualifiedName(prefix, label))
   }
   /** finishes an XML tag, assumes an open one exists */
   def finishTag() {
     write(">\n")
   }
   /** begins an XML attribute, assumes inside open tag, always starts with space separating from previous output */
   def beginAttribute(prefix : String, name : String) {
     write(s""" ${getQualifiedName(prefix, name)}="""")
   }
   /** finishes an XML attribute, assumes inside open tag and open attribute */
   def finishAttribute() {
     write("\"")
   }
   /** write an XML attribute at once, assumes inside open tag, always starts with space separating from previous output, can also be used for namespace bindings */
   def writeAttribute(prefix : String, name : String, value : String) {
     write(s""" ${getQualifiedName(prefix, name)}="$value"""")
   }*/
   @MMT_TODO("probably not needed anymore")
   /** write an XML start tag at once, including attributes and scope */
   def writeStartTag(prefix : String, label : String, attributes : MetaData, scope : NamespaceBinding): Unit = {
     write("<")
     write(getQualifiedName(prefix, label))
     write(attributes.toString) //starts with a space
     write(scope.toString) //starts with a space
     write(">")
   }
   @MMT_TODO("probably not needed anymore")
   /** write an XML end tag */
   def writeEndTag(prefix : String, label : String): Unit = {
     write(s"</${getQualifiedName(prefix, label)}>")
   }
   @MMT_TODO("probably not needed anymore")
   /** returns a qualified name from a prefix and a local part */
   private def getQualifiedName(prefix : String, name: String) =
     if (prefix == "" || prefix == null) name else (prefix + ":" + name)

   /** write an XML element in a way that the XML nesting is reflected in the code (attribute values will be escaped) */
   def elem(tag: String, attributes: (String,String)*)(body: => Unit): Unit = {
     val attS = attributes.map {case (k,v) => s"""$k="${XMLEscaping(v)}""""}.mkString(" ", " ", "")
     write(s"<$tag$attS>")
     nl
     indent(body)
     write(s"</$tag>")
     nl
   }

   /** write text inside an xml element (will be escaped) */
   def xmltext(s: String): Unit = {
     write(XMLEscaping(s))
   }

   /** releases all resources, empty by default */
   def done: Unit = {}

   // indentation management
   private var indentLevel = 0
   val indentString = "  "
   protected var afterIndentationString = ""
   /** renders a newline followed by indentation */
   def nl: Unit = {
    apply("\n")
    Range(0, indentLevel).foreach { _ =>
      apply(indentString)
    }
   }
   /** renders the body in such a way that all calls to nl create indentation one level deeper */
   def indent(body: => Unit): Unit = {
     indentLevel += 1
     nl
     try {
       body
     }
     finally {
       indentLevel -= 1
     }
   }
}

trait RenderingResult[A] extends RenderingHandler {
   /** releases all resources and returns the result of building */
   def get: A
   /** releases all resources without returning a result */
   override def done: Unit = {get}
}

/** writes text output to the console */
object ConsoleWriter extends RenderingHandler {
   def write(s : String): Unit = {print(s)}
}

/** writes text output to a file */
class FileWriter(val filename : File, compress: Boolean = false) extends RenderingHandler {
   private val file = utils.File.Writer(filename, compress)
   def write(s : String): Unit = {
     file.write(s)
   }
   override def done: Unit = {file.close}
}

/** writes text output to a StringBuilder */
class StringBuilder extends RenderingHandler with RenderingResult[String] {
  private val sb = new scala.collection.mutable.StringBuilder(5000)
   def write(s: String): Unit = {sb.append(s)}
   def get: String = sb.result
}

class HTMLRenderingHandler(hb: HTML) extends RenderingHandler {
   def write(s: String): Unit = {
      hb.out(s)
   }
}
