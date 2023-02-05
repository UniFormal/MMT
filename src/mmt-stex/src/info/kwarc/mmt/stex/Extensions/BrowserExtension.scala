package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.archives.{Archive, RedirectableDimension}
import info.kwarc.mmt.api.utils.{FilePath, JSON, JSONArray, JSONObject, JSONString, MMTSystem}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
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
        html = html.replace("CONTENT_CSS_PLACEHOLDER", "/:" + this.pathPrefix + "/css?")
        ServerResponse(html, "html")
      /*case ps if ps.startsWith("archive=") || ps.startsWith("group=") =>
        request.query match {
          case "" =>
            ServerResponse("Empty Document path", "txt")
          case s =>
            var html = MMTSystem.getResourceAsString("mmt-web/stex/mmt-viewer/index.html")
            html = html.replace("CONTENT_URL_PLACEHOLDER", "/:" + pathPrefix + "/documentTop?" + s)
            html = html.replace("BASE_URL_PLACEHOLDER", "")
            ServerResponse(html, "text/html")
        } */
      case _ =>
        throw ErrorReturn("Unknown query: " + request.query)
    }
  }

  private object JavaScript {
    def doLink(uri: String) = "var iframe = document.getElementById(\"targetiframe\");iframe.src = \"" + uri + "\""

    def setEdit(uri: String, back: String) = "var editbutton = document.getElementById(\"editbutton\");" +
      "var backbutton = document.getElementById(\"backbutton\");" +
      "backbutton.style.display=\"none\";" +
      "editbutton.style.display=\"inline\";" +
      "editbutton.onclick = function () {" +
      "iframe.src=\"" + uri + "\";" +
      "backbutton.style.display=\"inline\";" +
      "editbutton.style.display=\"none\";" +
      "};" +
      "backbutton.onclick = function () {" +
      "iframe.src=\"" + back + "\";" +
      "editbutton.style.display=\"inline\";" +
      "backbutton.style.display=\"none\";" +
      "}"

    def newtabbutton(uri: String) = "var newtabbutton = document.getElementById(\"newtabbutton\");" +
      "newtabbutton.href=(\"" + uri + "\");" +
      "newtabbutton.style.display=\"inline\""

    def setCurrentPath(p: String) = "document.getElementById(\"contentpath\").innerHTML = \"<code>" + p + "</code>\""

    def setCurrentArchive(p: String) = "document.getElementById(\"archive\").innerHTML = \"<b>" + p + "</b>\""

    def doJs(s: String*) = JSONString(s.mkString(";"))

    val deactivateButtons = "document.getElementById(\"editbutton\").style.display=\"none\";" +
      "document.getElementById(\"newtabbutton\").style.display=\"none\";" +
      "document.getElementById(\"backbutton\").style.display=\"none\""
  }

  def doMenu: JSON = {
    import JavaScript._
    val archives = controller.backend.getArchives.filter(_.properties.get("format").contains("stex")).sortBy(_.id).map { a => (a.id.split('/'), a) }

    def iterateGroup(in: List[String]): JSON = {
      val as = archives.filter(_._1.startsWith(in)).map(p => (p._1.drop(in.length), p._2))
      val archs = as.filter(_._1.length == 1).map(a => iterateArchive(a._2))
      val groups = as.filter(_._1.length > 1).map(_._1.head).distinct.map(s => iterateGroup(in ::: List(s)))
      val children = JSONArray(groups ::: archs: _*)
      if (in.isEmpty) children else {
        JSONObject(
          ("label", JSONString(in.last)),
          ("children", children),
          ("link", doJs(
            doLink("/:" + this.pathPrefix + "/browser?group=" + in.mkString("/")),
            setCurrentArchive(in.mkString("/")),
            setCurrentPath(""),
            deactivateButtons
          ))
        )
      }
    }

    def iterateArchive(a: Archive): JSON = {
      val tophtml = a / RedirectableDimension("xhtml")
      val toptex = a / info.kwarc.mmt.api.archives.source

      def children(fp: FilePath) = {
        val html = {
          if ((tophtml / fp).exists) (tophtml / fp).children else Nil
        }.flatMap(f => if (f.isDirectory) Some(tophtml.relativize(f), true) else if (f.getExtension.contains("xhtml")) Some(tophtml.relativize(f).stripExtension, false) else None)
        val tex = {
          if ((toptex / fp).exists) (toptex / fp).children else Nil
        }.flatMap(f => if (f.isDirectory) Some(toptex.relativize(f), true) else if (f.getExtension.contains("tex")) Some(toptex.relativize(f).stripExtension, false) else None)
        tex.filter(html.contains)
      }

      def iterate(fp: FilePath): JSON = JSONArray(
        children(fp).collect {
          case (f, true) =>
            JSONObject(
              ("label", JSONString(f.name)),
              ("children", iterate(fp / f.name)),
              ("link", doJs(
                doLink("/:" + this.pathPrefix + "/documentTop?archive=" + a.id + "&filepath=" + fp.toString + {
                  if (fp.isEmpty) "" else "/"
                } + f.name),
                setCurrentArchive(a.id),
                setCurrentPath({
                  if (fp.isEmpty) "" else "/"
                } + fp.toString + "/" + f.name),
                deactivateButtons
              ))
            )
          case (f, _) =>
            def url(s: String) = "/:" + this.pathPrefix + "/" + s + "?archive=" + a.id + "&filepath=" + fp.toString + {
              if (fp.isEmpty) "" else "/"
            } + f.name + ".xhtml"

            JSONObject(
              ("label", JSONString(f.name)),
              ("children", JSONArray()),
              ("link", doJs(
                doLink(url("fulldocument")),
                setCurrentArchive(a.id),
                setCurrentPath({
                  if (fp.isEmpty) "" else "/"
                } + fp.toString + "/" + f.name),
                //setEdit(url("editor"),url("browser")),
                newtabbutton(url("fulldocument"))
              ))
            )
        }: _*)

      JSONObject(
        ("label", JSONString(a.id.split('/').last)),
        ("children", iterate(FilePath.apply(Nil))),
        ("link", doJs(
          doLink("/:" + this.pathPrefix + "/browser?archive=" + a.id),
          setCurrentArchive(a.id),
          setCurrentPath(""),
          deactivateButtons
        ))
      )
    }

    val ret = iterateGroup(Nil)
    print("")
    ret
  }
}