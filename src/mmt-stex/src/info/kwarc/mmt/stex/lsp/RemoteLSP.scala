package info.kwarc.mmt.stex.lsp

import info.kwarc.mmt.api.archives.{Dim, RedirectableDimension}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.utils.{JSONArray, JSONObject, JSONString}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.search.Searcher

class RemoteLSP extends Extension {

  /*private val testRemote : JSONArray = JSONArray(
    JSONObject(("id",JSONString("sTeX/Hurtz")),("deps",JSONArray())),
    JSONObject(("id",JSONString("sTeX/Algebra/Narf")),("deps",JSONArray())),
    JSONObject(("id",JSONString("Narf/meta-inf")),("deps",JSONArray())),
    JSONObject(("id",JSONString("Narf/Foo/Bar")),("deps",JSONArray(JSONString("Narf/meta-inf")))),
  )*/

  private val dimensions = List(
    RedirectableDimension("xhtml"),
    RedirectableDimension("content"),
    RedirectableDimension("errors"),
    RedirectableDimension("narration"),
    RedirectableDimension("relational"),
    RedirectableDimension("bin"),
    RedirectableDimension("buildresults"),
    RedirectableDimension("export"),
    RedirectableDimension (".img")
  )
  lazy val searcher : Searcher = {
    new Searcher(controller)
  }

  def serverReturn(request: ServerRequest): ServerResponse = request.path.lastOption match {
    case Some("getupdates") =>
      request.parsedQuery("archive").flatMap(controller.backend.getArchive(_)) match {
        case Some(a) =>
          val ts : Long = request.parsedQuery("timestamp").flatMap(_.toLongOption).getOrElse(0)
          val ret = dimensions.map(dim =>
            if ((a / dim).exists()) {
              val ls = (a/dim).descendants.filter(_.lastModified().toLong > ts)
              if (ls.isEmpty) None else Some((dim.key,JSONArray(ls.map(f => JSONString((a/dim).relativize(f).toString)) :_*)))
            } else None
          )
          ServerResponse.JsonResponse(JSONObject.apply(ret.collect{case Some((d,ls)) => (d,ls)} :_*))
        case _ => ServerResponse.JsonResponse(JSONObject())
      }
    case Some("allarchives") =>
      val archs = controller.backend.getArchive("MMT/urtheories").get :: controller.backend.getArchives.filter(_.properties.get("format").contains("stex"))
      ServerResponse.JsonResponse(JSONArray(archs.map(a => JSONObject(
        ("id",JSONString(a.id)),
        ("deps",JSONArray(a.dependencies.map(JSONString):_*)),
        ("git",JSONString("https://gl.mathhub.info/" + a.id + ".git"))
      )):_*))
    case Some("allarchfiles") if request.query.nonEmpty =>
      controller.backend.getArchive(request.query) match {
        case Some(a) =>
          ServerResponse.JsonResponse(JSONArray(dimensions.map(d => JSONObject(("dim",JSONString(d.toString)),("files",JSONArray({
            val top = a / d
            (if (top.exists() && top.isDirectory) {
              top.descendants.map(f => JSONString(top.relativize(f).toString))
            } else Nil)
          } :_*)))) :_*))
        case _ => ServerResponse.JsonResponse(JSONArray())
      }
    case Some("archfile") if request.parsedQuery.contains("arch") && request.parsedQuery.contains("dim") && request.parsedQuery.contains("file") =>
      val archmain = controller.backend.getArchive(request.parsedQuery("arch").get).getOrElse {
        return ServerResponse("Malformed archfile request: " + request.path.mkString("/") + "\n" +
          request.parsedQuery("arch"), "text/plain")
      }
      val file = (archmain / RedirectableDimension(request.parsedQuery("dim").get)) / request.parsedQuery("file").get
      if (archmain.root <= file) ServerResponse.FileResponse(file)
      else ServerResponse("Malformed archfile request: " + request.path.mkString("/") + "\n" +
          archmain.id + " at " + archmain.root + "\n" + request.parsedQuery("dim") + "\n" + file.toString, "text/plain")
    case Some("archfile") =>
      ServerResponse("Malformed archfile request: " + request.path.mkString("/") + "\n" +
        request.query + "\n" + request.parsedQuery.pairs.mkString("\n"), "text/plain")
    case Some("search") =>
      val archs = request.parsedQuery("skiparchs").getOrElse("").split(',').map(_.trim).filterNot(_ == "").toList
      val types = request.parsedQuery("types").getOrElse("").split(',').map(_.trim).filterNot(_ == "").toList
      val infors = request.parsedQuery("infors").contains("true")
      val query = request.parsedQuery("query").getOrElse {
        return ServerResponse.JsonResponse(JSONArray())
      }
      val results = searcher.search(query,10,types,archs,infors)
      ServerResponse.JsonResponse(JSONArray(
        results.map(sr => JSONObject(
          ("archive",JSONString(sr.archive)),
          ("sourcefile",JSONString(sr.sourcefile)),
          ("html", JSONString(sr.fragments.head._3))
        ))
        :_*))
    case _ =>
      ServerResponse("Unknown request: \"" + request.path.lastOption + "\"\n" + request.query + "\n" + request.parsedQuery.pairs, "text/plain")
  }
}
