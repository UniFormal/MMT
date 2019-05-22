package info.kwarc.mmt.sql.codegen

import info.kwarc.mmt.api.objects.{OMA, OMS}
import info.kwarc.mmt.sql.Column

case class ColumnCode(column: Column, join: Option[JoinCode] = None) extends CodeHelper {

  private def nameQuotes: String = quoted(column.name)

  private def typeString: String = {
    if (column.dbtype.toString == "List[List[Int]]" || column.dbtype.toString == "List[Int]") s"List[Int]"
    else s"Option[${column.dbtype.toString}]"
  }

  private def codecName: String = column.codec match {
    case OMS(x) => x.name.toString
    case OMA(_, codecArgs) => codecArgs.head.toMPath.name.last.toString
  }

  def name: String = column.name
  def nameDbQuoted: String = columnNameDB(column)
  def isDisplayedByDefault: Boolean = column.isDisplayedByDefault

  // JsonSupport
  def jsonWriterMapItem: String = s"""Some($nameQuotes -> o.$nameCamelCase.toJson)"""

  // CaseClass
  def caseClassField: String = s"  $nameCamelCase: $typeString"
  def selectMapCaseClass: String = s"""    "$name" -> $nameCamelCase"""

  // PlainQueryObject
  def getResultItem: String = {
    if (join.nonEmpty) "None"
    else typeString match {
      case "UUID" => "r.nextObject.asInstanceOf[UUID]"
      case "List[Int]" => "r.<<[Seq[Int]].toList"
      case _ => "r.<<"
    }
  }

  // TableClass
  def nameCamelCase: String = camelCase(name)
  def accessorMethod: String = {
    val fk = join.map(_.fkMethod).map(m => s"\n$m").getOrElse("")
    s"""  def $nameCamelCase: Rep[$typeString] = column[$typeString]($nameDbQuoted)$fk"""
  }
  def selectMapTableClass: String = s"""    "$name" -> this.$nameCamelCase"""

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
