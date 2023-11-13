package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.{GlobalName, Path}
import info.kwarc.mmt.api.ontology.SPARQL.T
import info.kwarc.mmt.api.ontology.ULO
import info.kwarc.mmt.api.utils.{JSONArray, JSONNull, JSONObject, JSONString}
import info.kwarc.mmt.api.web.ServerResponse.{JsonResponse, ResourceResponse, TextResponse}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.STeXServer

trait QueryExtension { this : STeXServer =>
  protected def queryRequest(request: ServerRequest): ServerResponse = {
    request.path.lastOption match {
      case Some("all") =>
        JsonResponse(JSONArray(getAll.map(p => JSONString(p.toString)) :_*))
      case Some("all_undefined") =>
        JsonResponse(JSONArray(undefineds.map(p => JSONString(p.toString)): _*))
      case Some("all_with_data") =>
        JsonResponse(JSONArray(symbols_with_stuff.map(p =>
          JSONObject(
            ("path", JSONString(p._1.toString)),
            ("macro", p._2.map(s => JSONString("\\" + s)).getOrElse(JSONNull)),
            //("notations", JSONArray(p._3.map(JSONString): _*))
          )):_*))
      case Some("test") =>
        JsonResponse(JSONArray(test.map(p => JSONString(p.toString)): _*))
      case Some("problems") =>
        import info.kwarc.mmt.api.ontology.SPARQL._
        val query = (request.parsedQuery("path").map(Path.parse),request.parsedQuery("dimension")) match {
          case (None,_) =>
            val query = SELECT("path","objective_symbol","cognitive_dimension") WHERE (
              HASTYPE(V("path"), ULO.problem) AND
                T(V("path"), ULO.objective, V("w")) AND
                T(V("w"), ULO.crossrefs, V("objective_symbol")) AND
                T(V("w"), ULO.cognitiveDimension, V("cognitive_dimension"))
              )
            return JsonResponse(controller.depstore.query(query).getJson)
          case (Some(path),None | Some("")) =>
            SELECT("x") WHERE (HASTYPE(V("x"), ULO.problem) AND T(V("x"), ULO.objective, V("y")) AND T(V("y"), ULO.crossrefs, path))
          case (Some(path),Some(dimen)) =>
            SELECT("x") WHERE (HASTYPE(V("x"), ULO.problem) AND T(V("x"), ULO.objective, V("y")) AND T(V("y"), ULO.crossrefs, path) AND T(V("y"), ULO.cognitiveDimension, dimen))
        }
        val res = controller.depstore.query(query).getPaths.map(_.toString)
        JsonResponse(JSONArray(res.map(JSONString): _*))
      case _ =>
        ResourceResponse("stex/queries.html")
    }
  }

  private lazy val archives = controller.backend.getArchives.filter { a =>
    a.properties.get("format").contains("stex")
  }

  private lazy val getAll: List[Path] = { // TODO hacky; should be more systematic
    archives.flatMap(a => {
      import info.kwarc.mmt.api.ontology.SPARQL._
      controller.depstore.query(
        SELECT("x") WHERE (
          (
            T(Subject(s"mmt://memory/archive#${a.id}"), ULO.contains, V("y")) UNION
              T(Subject(s"mmt://memory//archive#${a.id}"), ULO.contains, V("y"))
            ) AND
            T(V("y"),(ULO.contains | ULO.declares | ULO.specifies)+,V("x")) AND
            HASTYPE(V("x"),ULO.constant)
        )
      ).getPaths.filter(p => !p.asInstanceOf[GlobalName].module.parent.last.endsWith(".omdoc"))
    })
  }

  private def symbols_with_stuff: List[(Path,Option[String])] = {
    getAll.map{p =>
      (p,
        controller.getO(p).flatMap(SHTMLContentManagement.getMacroName),
        //SHTMLContentManagement.getNotations(p.asInstanceOf[GlobalName])(controller).map{n => n.notation.toString}
      )
    }
  }

  private def undefineds: List[Path] = {
    getAll.filter{p =>
      import info.kwarc.mmt.api.ontology.SPARQL._
      controller.depstore.query(
        SELECT("x") WHERE (
          T(p,ULO.docref,V("x")) UNION T(V("x"),ULO.defines,p)
        )
      ).getPaths.isEmpty
    }
  }

  private def test: List[Path] = {
    archives.flatMap(a => {
      import info.kwarc.mmt.api.ontology.SPARQL._
      controller.depstore.query(
        SELECT("x") WHERE (
          T(Subject(s"mmt://memory/archive#${a.id}"), ULO.contains, V("x")) UNION
            T(Subject(s"mmt://memory//archive#${a.id}"), ULO.contains, V("x"))
          )
      ).getPaths
    })
  }
}
