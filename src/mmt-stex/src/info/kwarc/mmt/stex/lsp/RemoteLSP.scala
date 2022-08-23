package info.kwarc.mmt.stex.lsp

import info.kwarc.mmt.api.archives.{Dim, RedirectableDimension}
import info.kwarc.mmt.api.utils.{JSONArray, JSONObject, JSONString}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.Extensions.STeXExtension
import info.kwarc.mmt.stex.search.Searcher

class RemoteLSP extends STeXExtension {

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
    RedirectableDimension("export")
  )
  lazy val searcher : Searcher = {
    new Searcher(controller)
  }

  override def serverReturn(request: ServerRequest): Option[ServerResponse] = request.path.lastOption match {
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
          Some(ServerResponse.JsonResponse(JSONObject.apply(ret.collect{case Some((d,ls)) => (d,ls)} :_*)))
        case _ => Some(ServerResponse.JsonResponse(JSONObject()))
      }
    case Some("allarchives") =>
      val archs = controller.backend.getArchive("MMT/urtheories").get :: controller.backend.getArchives.filter(_.properties.get("format").contains("stex"))
      Some(ServerResponse.JsonResponse(JSONArray(archs.map(a => JSONObject(
        ("id",JSONString(a.id)),
        ("deps",JSONArray(a.dependencies.map(JSONString):_*)),
        ("git",JSONString("https://gl.mathhub.info/" + a.id + ".git"))
      )):_*)))
    case Some("allarchfiles") if request.query.nonEmpty =>
      controller.backend.getArchive(request.query) match {
        case Some(a) =>
          Some(ServerResponse.JsonResponse(JSONArray(dimensions.map(d => JSONObject(("dim",JSONString(d.toString)),("files",JSONArray({
            val top = a / d
            (if (top.exists() && top.isDirectory) {
              top.descendants.map(f => JSONString(top.relativize(f).toString))
            } else Nil)
          } :_*)))) :_*)))
        case _ => Some(ServerResponse.JsonResponse(JSONArray()))
      }
    case Some("archfile") if request.parsedQuery.contains("arch") && request.parsedQuery.contains("dim") && request.parsedQuery.contains("file") =>
      val archmain = controller.backend.getArchive(request.parsedQuery("arch").get).getOrElse { return None}
      val file = (archmain / RedirectableDimension(request.parsedQuery("dim").get)) / request.parsedQuery("file").get
      if (file <= archmain.root) Some(ServerResponse.FileResponse(file)) else None
    case Some("search") =>
      val archs = request.parsedQuery("skiparchs").getOrElse("").split(',').map(_.trim).filterNot(_ == "").toList
      val types = request.parsedQuery("types").getOrElse("").split(',').map(_.trim).filterNot(_ == "").toList
      val query = request.parsedQuery("query").getOrElse {
        return Some(ServerResponse.JsonResponse(JSONArray()))
      }
      val results = searcher.search(query,10,types,archs)
      Some(ServerResponse.JsonResponse(JSONArray(
        results.map(sr => JSONObject(
          ("archive",JSONString(sr.archive)),
          ("sourcefile",JSONString(sr.sourcefile)),
          ("html", JSONString(sr.fragments.head._3))
        ))
        :_*)))
    case _ => None
  }
}
