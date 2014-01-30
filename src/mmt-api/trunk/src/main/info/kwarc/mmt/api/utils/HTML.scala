package info.kwarc.mmt.api.utils

import scala.xml.Utility

/**
 * a partial implementation of HTML designed for easily building and emitting HTML documents
 * @param out a continuation function called on the text snippets making up the HTML document
 * 
 * see [[archives.HTMLExporter]] for a usage example
 */
class HTML(out: String => Unit) {
   private var nextid = 0
   /** @return a fresh id */
   def freshid: String = {
      val id = nextid
      nextid += 1
      "_" + id
   }
   private def optAttr(key: String, value: String) = if (value == "") "" else s""" $key="$value""""
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
      def apply(cls: String = "", id: String = "", title: String = "", onclick: String = "", attributes: List[(String,String)] = Nil)(body: => Unit) {
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
      def apply(body: => Unit) {
         apply()(body)
      }
   }
   val html = new Element("html")
   val head = new Element("head")
   val body = new Element("body")
   
   val div  = new Element("div")
   val span = new Element("span")
   
   val pre  = new Element("pre")

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
   
   val table = new Element("table")
   val tr = new Element("tr")
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
   
   def br {out("<br/>")} 
   def a(ref: String)(body: => Unit) {
      out(s"""<a href="$ref">""")
      body
      out("</a>")
   }
   def text(s : String) {
     out(scala.xml.Utility.escape(s))
   }
   
   /**
    * produces a script tag pointing to a javascript file
    * @param src the src attribute (i.e., the javascript file)
    */
   def javascript(src: String) {
      out(s"""<script type="text/javascript" src="$src"></script>""")
   }
   /**
    * produces a link tag pointing to a css file
    * @param src the href attribute (i.e., the css file)
    */
   def css(src: String) {
      out(s"""<link rel="stylesheet" type="text/css" href="$src"></link>""")
   }
}