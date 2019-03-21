package info.kwarc.mmt.sql.codegen

import info.kwarc.mmt.api.objects.{OMA, OMS}
import info.kwarc.mmt.sql.Column

case class ColumnCode(column: Column) {

  def nameQuotes = s""""${column.name}""""
  def nameDb: String = column.name.toUpperCase
  def nameCamelCase: String = "_([a-z\\d])".r.replaceAllIn(column.name, _.group(1).toUpperCase())

  def typeString: String = {
    if (column.dbtype.toString == "List[List[Int]]" || column.dbtype.toString == "List[Int]") s"List[Int]"
    else s"Option[${column.dbtype.toString}]"
  }
  def caseClassField: String = s"$nameCamelCase: $typeString"

  def jsonWriterMapItem: String = s"""Some($nameQuotes -> o.$nameCamelCase.toJson)"""

  def accessorMethod: String = {
    s"""def $nameCamelCase: Rep[$typeString] = column[$typeString]("$nameDb")"""
  }

  def selectMapItem: String = s""""${column.name}" -> this.$nameCamelCase"""

  def jsonObjectProperties: String = {
    val colType = column.dbtype.toString match {
      case "Int" => "numeric"
      case "Boolean" => "bool"
      case _ => "opaque"
    }
    s"""$nameQuotes: {"isFilter": ${!column.opaque}, "display": $nameQuotes, "type": "$codecName"}"""
  }

  private def codecName: String = column.codec match {
    case OMS(x) => x.name.toString
    case OMA(_, codecArgs) => codecArgs.head.toMPath.name.last.toString
  }

}
