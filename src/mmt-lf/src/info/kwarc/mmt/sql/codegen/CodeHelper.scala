package info.kwarc.mmt.sql.codegen

import info.kwarc.mmt.sql.Column

trait CodeHelper {

  val quotes = "\"\"\""

  def camelCase(s: String): String = "_([a-z\\d])".r.replaceAllIn(s, _.group(1).toUpperCase())
  def quoted(s: String): String = s""""$s""""
  def columnNameDB(c: Column): String = quoted(c.name.toUpperCase)

}
