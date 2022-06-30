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
    case Some("allarchives") =>
      val archs = controller.backend.getArchive("MMT/urtheories").get :: controller.backend.getArchives.filter(_.properties.get("format").contains("stex"))
      Some(ServerResponse.JsonResponse(JSONArray(archs.map(a => JSONObject(
        ("id",JSONString(a.id)),
        ("deps",JSONArray(a.dependencies.map(JSONString):_*)),
        ("git",JSONString("https://gl.mathhub.info/" + a.id + ".git"))
      )):_*)))
    /*  Some(ServerResponse.JsonResponse(testRemote))
    case Some("allarchfiles") if request.query == "Narf/Foo/Bar" =>
      Some(ServerResponse.JsonResponse(JSONArray(dimensions.map(d => JSONObject(("dim",JSONString(d.toString)),("files",JSONArray(
        List("a.txt","a/b.txt","c.txt","d/e/f.txt").map(JSONString):_*
      )))):_*)))
    case Some("allarchfiles") if request.query == "Narf/meta-inf" =>
      Some(ServerResponse.JsonResponse(JSONArray(dimensions.map(d => JSONObject(("dim",JSONString(d.toString)),("files",JSONArray(
        List("meta.txt","lib/meta.txt","src/meta.txt").map(JSONString):_*
      )))):_*))) */
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
      Some(ServerResponse.FileResponse(
        (controller.backend.getArchive(request.parsedQuery("arch").get).get / RedirectableDimension(request.parsedQuery("dim").get)) / request.parsedQuery("file").get
      ))
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
          ("html",JSONString(sr.fragments.collectFirst{case p if p._1 != "title" => p._2}.getOrElse(sr.fragments.head._2)))
        ))
        :_*)))
    case _ => None
  }
}