package info.kwarc.mmt.sql.codegen

import info.kwarc.mmt.api.objects.{OMA, OMS}
import info.kwarc.mmt.sql.Column

case class ColumnCode(column: Column) {

  private def nameQuotes = s""""${column.name}""""
  private def nameDb: String = column.name.toUpperCase

  private def typeString: String = {
    if (column.dbtype.toString == "List[List[Int]]" || column.dbtype.toString == "List[Int]") s"List[Int]"
    else s"Option[${column.dbtype.toString}]"
  }

  private def codecName: String = column.codec match {
    case OMS(x) => x.name.toString
    case OMA(_, codecArgs) => codecArgs.head.toMPath.name.last.toString
  }

  // JsonSupport
  def jsonWriterMapItem: String = s"""Some($nameQuotes -> o.$nameCamelCase.toJson)"""

  // CaseClass
  def caseClassField: String = s"  $nameCamelCase: $typeString"
  def selectMapCaseClass: String = s"""    "${column.name}" -> $nameCamelCase"""

  // PlainQueryObject
  def getResultItem: String = typeString match {
    case "UUID" => "r.nextObject.asInstanceOf[UUID]"
    case "List[Int]" => "r.<<[Seq[Int]].toList"
    case _ => "r.<<"
  }

  // TableClass
  def nameCamelCase: String = "_([a-z\\d])".r.replaceAllIn(column.name, _.group(1).toUpperCase())
  def accessorMethod: String = s"""def $nameCamelCase: Rep[$typeString] = column[$typeString]("$nameDb")"""
  def selectMapTableClass: String = s"""    "${column.name}" -> this.$nameCamelCase"""

  // Frontend
  def jsonObjectProperties: String = {
    val colType = column.dbtype.toString match {
      case "Int" => "numeric"
      case "Boolean" => "bool"
      case _ => "opaque"
    }
    s"""$nameQuotes: {"isFilter": ${!column.opaque}, "display": $nameQuotes, "type": "$codecName"}"""
  }

}
