package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.archives.{Archive, source}
import info.kwarc.mmt.api.utils.{File, MMTSystem}
import info.kwarc.mmt.api.web.WebQuery
import info.kwarc.mmt.stex.STeXServer

import scala.collection.mutable

trait ExportExtension { self : STeXServer =>

  private class State(val archive:Archive,val dir:File) {
    val todo_frag = mutable.Set.empty[GlobalName]
    val dones_frag = mutable.Set.empty[GlobalName]
  }

  def export(archive:Archive,document: File,to:File) = {
    if (to.exists()) to.descendants.foreach {
      case f if f.isDirectory => f.deleteDir
      case f if f.exists() => f.delete()
    }
    (to / "aux" / "fonts").mkdirs()
    (to / "docs").mkdirs()
    (to / "frags").mkdirs()

    to.descendants.foreach(_.delete())
    var index = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
    index = index.replace("CONTENT_URL_PLACEHOLDER", document.setExtension("doc.html").name)
    index = index.replace("BASE_URL_PLACEHOLDER", "")
    index = index.replace("SHOW_FILE_BROWSER_PLACEHOLDER", "false")
    index = index.replace("CONTENT_CSS_PLACEHOLDER", "aux/archive.css")

    File.write(to / "aux" / "archive.css",css(archive.id))

    val aux_files = MMTSystem.getResourceList("mmt-web/stex/mmt-viewer").filter(f => File(f).getExtension.isDefined && f != "index.html")
    aux_files.foreach{f =>
      File.write(to / "aux" / f,MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/" + f))
      index = index.replace("/stex/mmt-viewer/" + f,"aux/" + f)
    }
    File.write(to / "aux/fonts.css",
      MMTSystem.getResourceAsString("mmt-web/stex/fonts.css").replace("/stex/fonts/","fonts/")
    )
    index = index.replace("/stex/fonts.css","aux/fonts.css")

    val font_files = MMTSystem.getResourceList("mmt-web/stex/fonts")
    font_files.foreach { f =>
      val writer = File.Writer(to / "aux" / "fonts" / f)
      val in = MMTSystem.getResource("mmt-web/stex/fonts/" + f)
      in.readAllBytes().foreach(b => writer.write(b.toInt & 0xff))
      in.close()
      writer.close()
    }

    File.write(to / "index.html",index)
    implicit val state = new State(archive,to)
    doFile(document,to / document.setExtension("doc.html").name,true,None)
    while (state.todo_frag.nonEmpty) {
      val head = state.todo_frag.head
      doFrag(head)
    }
  }

  val footer = <div style="width:100%;display:inline-block;text-align:end;font-style:oblique;font-weight:bold;"><hr/>
    Created with <a href="https://github.com/slatex/sTeX" style="color:blue;"><div style="display:inline-flex;flex-direction:row;">
       <div style="bottom: -3.22919px;display:inline-block">S</div>
       <div style="margin-left: -3.22919px;display:inline-block">T</div>
       <div style="margin-left: -2.55606px;bottom: -3.22919px;display:inline-block">E</div>
       <div style="margin-left: -1.91666px;display:inline-block">X</div></div></a> by
      <a href="https://kwarc.info" style="color:blue;"><!--KWARC-->
        <img src="https://kwarc.info/public/kwarc_logo.svg" style="height:25px;vertical-align:middle;" title="KWARC"></img></a>
  </div>

  private def doFile(file:File,to:File,top : Boolean = false,withbindings:Option[LateBinding])(implicit state:State): Unit = {
    val params = new DocParams(new WebQuery(Nil)) {
      override lazy val path = None
      override lazy val language = None // TODO
      override lazy val filepath = Some(file.setExtension("xhtml").toString)
      override lazy val bindings = withbindings
      override lazy val archive = Some(state.archive)
    }
    val doc = doDocument(params)
    // TODO links
    val ret = if (top) {
      val bd = doc.get("div")()("body").head
      bd.add(footer)
      doc
    } else {
      val bd = doc.get("div")()("body").head
      bd.plain.attributes.remove((bd.namespace,"style"))
      bd.plain.attributes.remove((bd.namespace, "id"))
      bd
    }
    File.write(to,ret.toString.trim)
  }

  private def doFrag(gn : GlobalName)(implicit state:State): Unit = {
    state.todo_frag -= gn
    state.dones_frag += gn
  }


}
