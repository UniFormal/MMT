package info.kwarc.mmt.doc
import info.kwarc.mmt.api.utils._

/** An auxiliary application for generating the documentation web site.
 *  It lists the folder passed as its first argument and writes it out as an html snippet.
 */
object MakeTOC {
   private def skip(le: List[String]) =
      (List("index.html", "toc.html", "img").map(List(_)) contains le) || le.last.startsWith("_") || le.last.startsWith(".")
   def main(args: Array[String]) {
      val folder = File("""C:\frabe\MMT\doc\html""")//shortcut for Florian's office PC
      //val folder = File(args(0))
      val out = new HTMLFileWriter(folder / "toc.html")
      import out._
      
      css("style.css")
      javascript("https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/script/jquery/jquery.js")
      def doFolder(l: List[String]) {
         ul("sidebargroup") {
             (folder / l).list foreach {e =>
               val le = l ::: List(e)
               val label = File(e).stripExtension.toString.replace("_"," ")
               if (e.endsWith(".html") && !skip(le)) {
                  li("sidebarelement",  onclick=s"parent.goto('${le.mkString("/")}')") {
                     text(label)
                  }
               } else if ((folder/le).isDirectory && ! skip(le)) {
                  li {
                     span("sidebargroupheader", onclick="$(this).next().toggle()") {
                        text(label)
                     }
                     doFolder(le)
                  }
               }
             }
         }
      }
      
      doFolder(Nil)
      out.close
   }
}