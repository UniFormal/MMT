package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.archives.{Archive, ArchiveLike, RedirectableDimension}
import info.kwarc.mmt.api.utils.{FilePath, JSON, JSONArray, JSONObject, JSONString, MMTSystem}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.vollki.VirtualArchive
import info.kwarc.mmt.stex.{ErrorReturn, STeXServer}

trait SHTMLBrowser { this : STeXServer =>
  protected def browserRequest(request: ServerRequest): ServerResponse = {
    request.query match {
      case "menu" =>
        ServerResponse.JsonResponse(doMenu)
      case "" =>
        var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
        html = html.replace("BASE_URL_PLACEHOLDER", "")
        html = html.replace("SHOW_FILE_BROWSER_PLACEHOLDER", "true")
        html = html.replace("NO_FRILLS_PLACEHOLDER", "TRUE")
        html = html.replace("CONTENT_CSS_PLACEHOLDER", "/:" + this.pathPrefix + "/css?")
        ServerResponse(html, "html")
      case _ =>
        documentRequest(request.copy(path=request.path.init ::: List("fullhtml")))
      case _ =>
        throw ErrorReturn("Unknown query: " + request.query)
    }
  }

  def doMenu: JSON = {
    val archives = getArchives.sortBy(_.id).map { a => (a.id.split('/'), a) }

    def iterateGroup(in: List[String]): JSON = {
      val as = archives.filter(_._1.startsWith(in)).map(p => (p._1.drop(in.length), p._2))
      val archs = as.filter(_._1.length == 1).map(a => iterateArchiveLike(a._2))
      val groups = as.filter(_._1.length > 1).map(_._1.head).distinct.map(s => iterateGroup(in ::: List(s)))
      val children = JSONArray(groups ::: archs: _*)
      if (in.isEmpty) children else {
        JSONObject(
          ("label", JSONString(in.last)),
          ("children", children)
        )
      }
    }

    def iterateArchiveLike(a: ArchiveLike): JSON = a match {
      case a:Archive => iterateArchive(a)
      case va:VirtualArchive => iterateVirtualArchive(va)
    }

    def iterateVirtualArchive(a: VirtualArchive): JSON = {
      JSONObject(
        ("label", JSONString(a.id.split('/').last)),
        ("children", JSONArray(a.getIndex :_*))
      )
    }

    def iterateArchive(a: Archive): JSON = {
      val tophtml = a / RedirectableDimension("xhtml")
      val toptex = a / info.kwarc.mmt.api.archives.source

      def children(fp: FilePath) = {
        val html = {
          if ((tophtml / fp).exists) (tophtml / fp).children else Nil
        }.flatMap(f => if (f.isDirectory) Some(tophtml.relativize(f), true) else if (f.getExtension.contains("xhtml")) Some(tophtml.relativize(f), false) else None)
        val tex = {
          if ((toptex / fp).exists) (toptex / fp).children else Nil
        }.flatMap(f => if (f.isDirectory) Some(toptex.relativize(f), true) else if (f.getExtension.contains("tex")) Some(toptex.relativize(f).setExtension("xhtml"), false) else None)
        tex.filter(html.contains)
      }

      def iterate(fp: FilePath): JSON = JSONArray(
        children(fp).collect {
          case (f, true) =>
            JSONObject(
              ("label", JSONString(f.name)),
              ("children", iterate(fp / f.name))
            )
          case (f, _) =>
            JSONObject(
              ("label", JSONString(f.stripExtension.name)),
              ("archive",JSONString(a.id)),
              ("filepath",JSONString({
                if (fp.isEmpty) "" else fp.toString + "/"
              } + f.name))
            )
        }: _*)

      JSONObject(
        ("label", JSONString(a.id.split('/').last)),
        ("children", iterate(FilePath.apply(Nil)))
      )
    }

    val ret = iterateGroup(Nil)
    print("")
    ret
  }
}