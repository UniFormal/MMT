/*******************************************************************************
 * This file is part of tiscaf.
 * 
 * tiscaf is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Foobar is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with tiscaf.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package tiscaf
package let

private object DirLet {

  def top(title: String, server: String) =
"""<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html><head><meta http-equiv="content-type" content="text/html; charset=utf-8">
<title>""" + title + """</title>

<style>
body {padding:.4em;font-family: sans-serif;}
a {text-decoration:none;}
#list {border-collapse:collapse; width:99%;}
#list th {border:1px #FFF solid; padding:.4em;}
#list td {border:1px #FFF solid; padding:.4em; white-space:nowrap;}
#list td.path {border:1px #DDD solid; margin-bottom:1.1em; white-space:normal; font-weight:bold; line-height:1.7em;}
#list td.title {border:0; text-align:right; padding:0;}
#list td.title span {border:1px #FFF solid; padding:.4em; position:relative; font-size:1.3em; color:#999999; font-weight:bold;}
#list td.clear {border:0; height:0.1em;}
.odd {background-color:#F0F0F0;}
.even {background-color:#FAFAFA;}
.td1 {width:1em;}
</style>

</head><body><center><table id="list"><tbody>

<tr><td colspan="4" class="title"><span>""" + server + """</span></td></tr>
"""

  def beforeList = """
<tr><td colspan="4" class="clear"></td></tr>
<tr><th class="td1"></th><th align="left">name</th><th align="right">bytes</th><th align="right">y-m-d h:m:s (local)</th></tr>"""

  def afterList = """</tbody></table></center></body></html>"""

  val numFormat = java.text.NumberFormat.getIntegerInstance
  numFormat.setGroupingUsed(true)
}

import DirLet._

import scala.concurrent._
import ExecutionContext.Implicits.global

protected class DirLet(dirRoot: String, uriRoot: String, pathRest: String) extends HSimpleLet {

  def act(tk: HTalk) {
    val uriExt = if (tk.req.uriExt.isDefined) { ";" + tk.req.uriExt.get } else ""
    val f = new java.io.File(dirRoot + pathRest)
    if (f.exists && f.isDirectory) {
      val out = tk.bytes(showDir(f, uriExt, tk.encoding))
      tk.setContentLength(out.length).setContentType("text/html").write(out)
    }
    else new ErrLet(HStatus.NotFound) act(tk)
  }

  private def showDir(f: java.io.File, uriExt: String, encoding: String): String = {
    val buf = new StringBuilder

    buf.append(top(uriRoot + pathRest, "tiscaf")).append("\n")

    val pathRefs = {
      val b = new StringBuilder
      b.append("""<tr><td colspan="4" class="clear"></td></tr><tr><td colspan="4" class="path">""")
      if (uriRoot == "") b.append(new PathItem("/" + uriExt, "root", encoding).toString )
      else b.append(new PathItem("/" + uriRoot + "/" + uriExt, uriRoot, encoding).toString )
      val pathItems = if (pathRest.startsWith("/")) pathRest.substring(1).split("/") else pathRest.split("/")
      for(i <- 0 until pathItems.length) {
        val itemName = pathItems(i)
        val itemRef =
          "/" +
          {if (uriRoot == "") "" else uriRoot + "/" } +
          { for(j <- 0 to i) yield pathItems(j) }.mkString("/") + "/"
        if (i == pathItems.length - 1) b.append(" / " + itemName)
        else b.append(" / " + new PathItem(itemRef, itemName, encoding).toString)
      }
      b.append("""</td></tr>""")
      b.toString
    }

    buf.append(pathRefs)
    buf.append(beforeList).append("\n")

    val fileItems = { for(file <- f.listFiles) yield {
      val item = new ListItem(
        file.isDirectory,
        file.getName,
        file.getName,
        new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss") format(new java.util.Date(file.lastModified)),
        DirLet.numFormat.format(file.length),
        uriExt,
        encoding
      )
      item
    }} .toList.sortWith((i1, i2) => i1 compare i2)

    for(idx <- 1 to fileItems.size) buf.append(fileItems(idx - 1).asString(idx)).append("\n")


    if (fileItems.size > 10) buf.append(pathRefs)
    buf.append(afterList)

    buf.toString
  }

}

private class PathItem(href: String, item: String, encoding: String) {
  override def toString = {
    def subdirs = href.split("/").map(java.net.URLEncoder.encode(_, encoding)).mkString("/")
    // split() doesn't keep trailing slash
    def encodedHref = if (href.endsWith("/")) subdirs + "/" else subdirs
    "<a href=\"" + encodedHref + "\">" + item + "</a>"
  }
}

private class ListItem(
  private val isDir: Boolean,
  href: String,
  private val name: String,
  date: String,
  size: String,
  uriExt: String,
  encoding: String ) {

  def asString(num: Int) =
    "<tr class=\"" + { if (num % 2 == 1) "odd" else "even" } + "\"><td class=\"td1\">" + { if (isDir) "dir" else "  " } +
    "</td><td><a href=\"" + java.net.URLEncoder.encode(href, encoding) +
    { if (isDir) "/" else "" } + uriExt + "\">" + name + "</a></td><td align=\"right\">" +
    { if (isDir) "   "  else size } + "</td><td align=\"right\">" + date + "</td></tr>"

  def compare(that: ListItem): Boolean = {
    if (isDir && !that.isDir) true
    else if (!isDir && that.isDir) false
    else name.toLowerCase.compareTo(that.name.toLowerCase) < 0
  }
}
