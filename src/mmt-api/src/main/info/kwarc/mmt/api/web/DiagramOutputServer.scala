package info.kwarc.mmt.api.web

import info.kwarc.mmt.api.modules.Diagram
import info.kwarc.mmt.api.{MPath, Path, presentation}
import info.kwarc.mmt.api.utils.{JSONString, URI}

object DiagramOutputServer {
  val pathPrefix: String = "diagramOutput"
  private val diagramPathKey: String = "diag"

  def getURIForDiagram(baseURI: URI, diag: MPath): URI = {
    (baseURI / (":" + DiagramOutputServer.pathPrefix)) ? (diagramPathKey + "=" + diag)
  }
}

/**
  * Provides a highlighted syntax presentation of all outputs of a diagram structural feature
  * at the URI [[http://<mmt server>/:diagramOutput?diag=<MPath to diagram structural feature use>]].
  *
  * (To be precise, it actually is
  *  [[http://<mmt server>/:<DiagramOutputServer.pathPrefix>?<diagramPathKey>=<MPath to diagram structural feature use>]])
  *
  * @example Suppose you have ''diagram myDiag : ?metaTheory = ...'' in one of your MMT files with namespace
  *          ''https://example.com''.
  *          Then, after elaboration of that diagram feature use (e.g. after building the containing MMT file
  *          to mmt-omdoc), you can access [[http://localhost:8080/:diag/?diag=https://example.com?myDiag]]
  *          to view all the outputs added by the diagram feature to the ambient theory graph.
  */
class DiagramOutputServer extends ServerExtension(DiagramOutputServer.pathPrefix) {
  final override val logPrefix = pathPrefix

  /**
    *
    *
    * Must be safe to interpolate into an HTML attribute enclosed by double quotes.
    */
  private val mmtWebEditorEndpoint: String = "http://192.168.0.10:8081/bare.html"

  final override def start(args: List[String]): Unit = {
    super.start(args)
  }

  final override def apply(request: ServerRequest): ServerResponse = {
    val diag = Path.parseM(request.parsedQuery(DiagramOutputServer.diagramPathKey).getOrElse(
      return ServerResponse.errorResponse("no ?diag=... parameter")
    ))

    val outputModules = Diagram.parseOutput(diag)(controller.globalLookup)

    val surfaceSyntax = {
      val sb = new presentation.StringBuilder
      outputModules.map(controller.get).foreach(controller.presenter(_)(sb))
      sb.get
    }

    val html = s"""
<!doctype html>
<html>
  <head></head>
  <style>
    *             { box-sizing: border-box; margin: 0; padding: 0; }
    html, body    { width: 100%; height: 100%; }
    iframe#editor { width: 100%; height: 100%; }
  </style>
  <body>
    <iframe id="editor" src="$mmtWebEditorEndpoint"></iframe>
    <script>
      const mmtSurfaceSyntax = ${JSONString(surfaceSyntax).toCompactString};

      const iframeEditor = document.getElementById("editor");
      iframeEditor.addEventListener("load", () => {
        iframeEditor.contentWindow.postMessage(mmtSurfaceSyntax, "http://192.168.0.10:8081/");
      });
    </script>
  </body>
</html>
"""
    ServerResponse.HTMLResponse(html)
  }
}