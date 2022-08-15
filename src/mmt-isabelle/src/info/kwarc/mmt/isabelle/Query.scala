package info.kwarc.mmt.isabelle

import info.kwarc.mmt.api.utils.FileURI
import info.kwarc.mmt.api.{DPath, utils}

/**
  * Scala implementation of Isabelle/MMT query command
  */

object Query {
  def command(arg: String): String = {
    type Entry = (String, String)  // name, theory

    val (narration_base, (classes, types, consts), query) = {
      import isabelle.XML.Decode._
      import isabelle.Term_XML.Decode._
      val entries: T[List[Entry]] = list(pair(string, string))
      triple(string, triple(entries, entries, entries), list(term))(isabelle.YXML.parse_body(arg))
    }

    def add_content(
      kind: isabelle.Export_Theory.Kind.Value)(content: Importer.Content, entry: Entry
    ): Importer.Content = {
      val (name, theory) = entry
      val theory_path = DPath(utils.URI(narration_base)) ? theory
      content + Importer.Item.Name(theory_path, kind.toString, name)
    }

    val content1 =
      classes.foldLeft(Importer.Content.empty)(add_content(isabelle.Export_Theory.Kind.CLASS))
    val content2 =
      types.foldLeft(content1)(add_content(isabelle.Export_Theory.Kind.TYPE))
    val content3 =
      consts.foldLeft(content2)(add_content(isabelle.Export_Theory.Kind.CONST))

    val terms = query.map(content3.import_term(_))

    val result: List[String] = // FIXME proper query result
      content3.all_names.map(name => name.global.toString)

    {
      import isabelle.XML.Encode._
      isabelle.YXML.string_of_body(list(string)(result))
    }
  }
}
