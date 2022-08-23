package info.kwarc.mmt.api.utils

import scala.collection.immutable.List
import scala.xml.Utility.escape

/**
 * a partial implementation of HTML designed for easily building and emitting HTML documents
 *
 * see [[archives.HTMLExporter]] for a usage example
 */
abstract class HTML {
   /** continuation function called on the text snippets making up the HTML document */
   def out(s: String): Unit

   private var nextid = 0
   /** @return a fresh id */
   def freshid: String = {
      val id = nextid
      nextid += 1
      "_" + id
   }

   /** @return the string key="value" if value is non-empty, empty string otherwise */
   private def optAttr(key: String, value: String) = if (value == "") "" else s""" $key="${escape(value)}""""
   /**
    * Most HTML tags inherit from this class
    *
    * @param tag the XML tag of the element, e.g., "html"
    */
   class Element(tag: String) {
      /**
       * @param cls the class attribute
       * @param id the id attribute
       * @param onclick the onclick attribute
       * @param the title attribute
       * @param attributes other attributes as (key,value) pairs
       * @param body the content of the element
       * empty attributes are dropped, all arguments are empty by default
       */
      def apply(cls: String = "", id: String = "", title: String = "", onclick: String = "", attributes: List[(String,String)] = Nil)(body: => Unit): Unit = {
         val mainatts = List("class" -> cls, "id" -> id, "title" -> title, "onclick" -> onclick)
         var attString = ""
         (mainatts ::: attributes) foreach {case (a,v) =>
            attString += optAttr(a,v)
         }
         out("<" + tag + attString + ">")
         body
         out("</" + tag + ">")
      }
      /** convenience method for the case of using an element without attributes */
      def apply(body: => Unit): Unit = {
         apply()(body)
      }
   }

   val html = new Element("html")
   val head = new Element("head")
   val body = new Element("body")

   val div  = new Element("div")
   val span = new Element("span")

   val pre  = new Element("pre")
   val code = new Element("code")

   val math       = new Element("math")
   val mo         = new Element("mo")
   val mi         = new Element("mi")
   val mn         = new Element("mn")
   val mrow       = new Element("mrow")
   val munder     = new Element("munder")
   val mover      = new Element("mover")
   val munderover = new Element("munderover")
   val msub       = new Element("msub")
   val msup       = new Element("msup")
   val msubsup    = new Element("msubsup")
   val mfrac      = new Element("mfrac")
   val msqrt      = new Element("msqrt")
   val merror     = new Element("merror")
   val mglyph     = new Element("mglyph")
   val mlabel     = new Element("mlabeledtr")
   val mphantom   = new Element("mphantom")
   val mroot      = new Element("mroot")
   val mtext      = new Element("mtext")

   val table = new Element("table")
   val thead = new Element("thead")
   val tbody = new Element("tbody")
   val tr = new Element("tr")
   val th = new Element("th")
   val td = new Element("td")

   val ul = new Element("ul")
   val ol = new Element("ol")
   val li = new Element("li")

   val h1 = new Element("h1")
   val h2 = new Element("h2")
   val h3 = new Element("h3")
   val h4 = new Element("h4")
   val h5 = new Element("h5")
   val h6 = new Element("h6")
   val h7 = new Element("h7")

   val p  = new Element("p")
   val button = new Element("button")

   /** br element */
   def br: Unit = {out("<br/>")}
   /** anchor element */
   def a(ref: String)(body: => Unit): Unit = {
      out(s"""<a href="$ref">""")
      body
      out("</a>")
   }
   def form(action : String = "", cls: String = "", id: String = "", title: String = "", onclick: String = "", attributes: List[(String,String)] = Nil)(body: => Unit): Unit = {
      new Element("form").apply(cls,id,title,onclick,("action",action) :: attributes) { body }
   }
   def select(name : String, cls: String = "", id: String = "", title: String = "", onclick: String = "", attributes: List[(String,String)] = Nil)(body: => Unit) = {
      new Element("select").apply(cls,id,title,onclick,("name",name) :: attributes) { body }
   }
   def input(itype : String = "", name : String = "", value : String = "", cls: String = "", id: String = "", title: String = "", onclick: String = "", attributes: List[(String,String)] = Nil)(body: => Unit): Unit = {
      new Element("input").apply(cls,id,title,onclick,("type",itype) :: List(("name",name),("value",value)).filterNot(_._2 == "") ::: attributes) { body }
   }
   def option(value : String = "", cls: String = "", id: String = "", title: String = "", onclick: String = "", attributes: List[(String,String)] = Nil)(body: => Unit): Unit = {
      new Element("option").apply(cls,id,title,onclick,("value",value) :: attributes) { body }
   }
   /** object element */
   def htmlobject(ref: String, tp: String): Unit = {
      out(s"""<object type="$tp" data="$ref"></object>""")
   }
   /** iframe element */
   def iframe(src: String): Unit ={
      out(s"""<iframe src="$src">""")
      out("</iframe>")
   }
   /** img element */
   def img(src: String): Unit = {
     out(s"""<img src="$src"/>""")
   }
   /** embed element */
   def embed(src: String): Unit = {
     out(s"""<embed src="$src"/>""")
   }
   /** text node */
   def text(s : String): Unit = {
     out(escape(s))
   }
   /** outputs as is */
   def literal(s: String): Unit = {
     out(s)
   }
   /** outputs as is */
   def literal(n: scala.xml.Node): Unit = {
     out(n.toString)
   }

   /**
    * produces a script element for a javascript file
    * @param src the src attribute (i.e., the javascript file)
    */
   def javascript(src: String): Unit = {
      out(s"""<script type="text/javascript" src="$src"></script>""")
   }
   /**
    * produces a link tag for a css file
    * @param src the href attribute (i.e., the css file)
    */
   def css(src: String): Unit = {
      out(s"""<link rel="stylesheet" type="text/css" href="$src"></link>""")
   }

   /** creates a javascript function application */
   object JS {
      def apply(fun: String)(args: String*) = fun + args.map(a => "'" + a + "'" ).mkString("(", ",", ")")
   }
}

/** collects HTML in a String */
class HTMLBuilder extends HTML {
   private var _result = new StringBuilder
   def out(s: String): Unit = {_result append s}
   def result = _result.toString
   def reset: Unit = {_result.clear}
}

/** collects HTML in a file */
class HTMLFileWriter(f: File) extends HTML {
   private val fw = File.Writer(f)
   def out(s: String): Unit = {fw.write(s)}
   def close: Unit = {
      fw.close
   }
}

object HTML {
   def apply(f: String => Unit) = new HTML {
      def out(s: String): Unit = {f(s)}
   }
   def build(f: HTML => Unit) = {
      val h = new HTMLBuilder
      f(h)
      h.result
   }
}
