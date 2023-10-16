package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.utils.JSONArray.toList
import info.kwarc.mmt.api.utils.{File, JSON, JSONArray, JSONObject, JSONString}
import info.kwarc.mmt.stex.STeXServer

trait FrontendExtension { this : STeXServer =>
  def getFrontendElements = {
    getArchives.collect({
      case a : Archive =>
        val file = a.root / "META-INF" / "archive.json"
        if (file.exists()) {
          parseJson(File.read(file),a)
        } else Nil
    }).flatten
  }

  private def parseJson(s:String,a:Archive) : List[JSONObject] = try {
    JSON.parse(s) match {
      case arr : JSONArray => toList(arr).collect{case jo:JSONObject => doObj(jo,a)}
      case o : JSONObject => List(doObj(o,a))
      case _ => Nil
    }
  } catch {
    case e : Exception =>
      log(e.getMessage)
      Nil
  }
  private def doObj(j:JSONObject,a:Archive) : JSONObject = {
    JSONObject((JSONString("archive"),JSONString(a.id)) :: (j.map.map {
      case (k@JSONString("landing"|"notes"|"slides"|"file"),JSONString(v)) =>
        if (!v.endsWith(".tex")) (k,JSONString(v + ".tex")) else (k,JSONString(v))
      //case (JSONString("thumbnail"), v) => ???
      case o => o
    }))
  }
}
