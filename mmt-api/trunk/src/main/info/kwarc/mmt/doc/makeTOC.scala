package info.kwarc.mmt.doc

import info.kwarc.mmt.api.utils._

/** An auxiliary application for generating the documentation web site.
  * It lists the folder passed as its first argument and writes it out as an html snippet.
  */
object MakeTOC {
  private val skip = List("index.html", "toc.html", ".svn", "img")

  def main(args: Array[String]) {
    val folder = File(args(0))
    val out = File.Writer(folder / "toc.html")
    out.write("<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"/>")
    def doFolder(l: List[String]) {
      out.write("<ul class=\"sidebargroup\" >\n")
      (folder / FPath(l)).list foreach { e =>
        val le = l ::: List(e)
        if (e.endsWith(".html") && !(l.isEmpty && skip.contains(e)))
          out.write("<li class=\"sidebarelement\" onclick=\"parent.goto('" + le.mkString("/") + "')\">" + e + "</li>\n")
        else if ((folder / FPath(le)).isDirectory && !skip.contains(e)) {
          out.write("<li>" + e)
          doFolder(le)
          out.write("</li>")
        }
      }
      out.write("</ul>\n")
    }
    doFolder(Nil)
    out.close
  }
}
