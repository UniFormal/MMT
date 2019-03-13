package info.kwarc.mmt.sql.codegen

import info.kwarc.mmt.sql.Column

case class ColumnCode(column: Column) {

  def nameQuotes = s""""${column.name}""""
  def nameDb: String = column.name.toUpperCase
  def nameCamelCase: String = "_([a-z\\d])".r.replaceAllIn(column.name, _.group(1).toUpperCase())

  def typeString: String = if (column.isNullable) s"Option[${column.dbtype.toString}]" else column.dbtype.toString
  def caseClassField: String = s"$nameCamelCase: $typeString"

  def jsonWriterMapItem: String = s"""Some($nameQuotes -> o.$nameCamelCase.toJson)"""

  def accessorMethod: String = {
    val maybePrimary = if (column.isPrimaryKey) ", O.PrimaryKey" else ""
    s"""def $nameCamelCase: Rep[$typeString] = column[$typeString]("$nameDb"$maybePrimary)"""
  }

  def selectMapItem: String = s""""${column.name}" -> this.$nameCamelCase"""

  def jsonObjectProperties: String = { // TODO display name
    val colType = column.dbtype.toString match {
      case "Int" => "numeric"
      case "Boolean" => "bool"
      case _ => "opaque"
    }
    s"""$nameQuotes: {"isFilter": ${!column.opaque}, "display": $nameQuotes, "type": "$colType"}"""
  }

}
