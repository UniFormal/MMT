package zgs.httpd.let


private object DirLet {
  
  def top(title : String, server : String) = 
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


import zgs.httpd. { HLet, HTalk, HStatus }
import DirLet._

protected class DirLet(dirRoot: String, uriRoot : String, pathRest : String) extends HLet {
  
  def act(tk : HTalk) {
    val uriExt = if (tk.req.uriExt.isDefined) { ";" + tk.req.uriExt.get } else ""
    val f = new java.io.File(dirRoot + pathRest)
    if (f.exists && f.isDirectory) {
      val out = showDir(f, uriExt).getBytes("UTF-8")
      tk.setContentLength(out.length).setContentType("text/html").write(out).close
    }
    else new ErrLet(HStatus.NotFound) act(tk)
  }
  
  private def showDir(f : java.io.File, uriExt : String) : String = {
    val buf = new StringBuilder
    
    buf.append(top(uriRoot + pathRest, "tiscaf")).append("\n")
    
    val pathRefs = {
      val b = new StringBuilder
      b.append("""<tr><td colspan="4" class="clear"></td></tr><tr><td colspan="4" class="path">""")
      if (uriRoot == "") b.append(new PathItem("/" + uriExt, "root") toString )
      else b.append(new PathItem("/" + uriRoot + "/" + uriExt, uriRoot) toString )
      val pathItems = if (pathRest.startsWith("/")) pathRest.substring(1).split("/") else pathRest.split("/")
      for(i <- 0 until pathItems.size) {
        val itemName = pathItems(i)
        val itemRef = 
          "/" + 
          {if (uriRoot == "") "" else uriRoot + "/" } +
          { for(j <- 0 to i) yield pathItems(j) }.mkString("/") + "/"
        if (i == pathItems.size - 1) b.append(" / " + itemName)
        else b.append(" / " + new PathItem(itemRef, itemName) toString)
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
        uriExt
      )
      item
    }} .toList.sortWith((i1, i2) => i1 compare i2)
    
    for(idx <- 1 to fileItems.size) buf.append(fileItems(idx - 1).asString(idx)).append("\n")
    
    
    if (fileItems.size > 10) buf.append(pathRefs)
    buf.append(afterList)
    
    buf.toString
  }

}

private class PathItem(href : String, item : String) {
  override def toString = "<a href=\"" + href + "\">" + item + "</a>"
}

private class ListItem(
  private val isDir : Boolean,
  href : String,
  private val name : String,
  date : String,
  size : String,
  uriExt : String ) {
  
  def asString(num : Int) = 
    "<tr class=\"" + { if (num % 2 == 1) "odd" else "even" } + "\"><td class=\"td1\">" + { if (isDir) "dir" else "  " } +
    "</td><td><a href=\"" + href + { if (isDir) "/" else "" } + uriExt + "\">" + name + "</a></td><td align=\"right\">" +
    { if (isDir) "   "  else size } +
    "</td><td align=\"right\">" + date + "</td></tr>"
  
  def compare(that : ListItem) : Boolean = {
    if (isDir && !that.isDir) true
    else if (!isDir && that.isDir) false
    else name.toLowerCase.compareTo(that.name.toLowerCase) < 0
  }
}
