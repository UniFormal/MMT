package info.kwarc.mmt.sql.codegen

import info.kwarc.mmt.sql.{Column, Table}

case class TableCode(prefix: String, table: Table) {

  def tableName: String = table.name
  def tableObject: String = s"tb$tableName"
  def packageString = s"xyz.discretezoo.web.db.$prefix$tableName" // package for the table specific files
  // class names
  def plainQueryObject = s"${tableName}PlainQuery"
  def tableClass = s"${tableName}Table"
  def caseClass: String = table.name

  private def dbTableName = s"${prefix}_${tableName.toUpperCase}" // table name in the database

  private def columnCodeList: Seq[ColumnCode] = table.columns.map(ColumnCode)

//  TODO def inCollectionMap: String = collections.map(_.inCollectionItem).mkString(",\n")

  def codeTableClass: String = {
    val accessorMethods = columnCodeList.map(_.accessorMethod).mkString("\n")
    val caseClassMapParameters = columnCodeList.map(_.nameCamelCase).mkString(" ::\n")
    val selectMap = columnCodeList.map(_.selectMapItem).mkString(",\n")

    s"""package $packageString
       |import java.util.UUID
       |import slick.collection.heterogeneous.HNil
       |import slick.lifted.{ProvenShape, Rep}
       |import xyz.discretezoo.web.DynamicSupport.ColumnSelector
       |import xyz.discretezoo.web.ZooPostgresProfile.api._
       |
       |final class $tableClass(tag: Tag) extends Table[$caseClass](tag, "$dbTableName") with ColumnSelector {
       |
       |def ID: Rep[UUID] = column[UUID]("ID", O.PrimaryKey)
       |$accessorMethods
       |
       |def * : ProvenShape[$caseClass] = (
       |ID ::
       |$caseClassMapParameters ::
       |HNil
       |).mapTo[$caseClass]
       |
       |val select: Map[String, Rep[_]] = Map(
       |$selectMap
       |)
       |
       |val inCollection: Map[String, Rep[Boolean]] = Map(
       |"ID" -> true
       |)
       |
       |}""".stripMargin
  }

  def codeCaseClass: String = {
    val cols = columnCodeList.map(_.caseClassField).mkString(",\n")
    val selectMap = columnCodeList.map(_.selectMapItem).mkString(",\n")

    s"""package $packageString
       |import java.util.UUID
       |import xyz.discretezoo.web.ZooObject
       |
       |case class $caseClass(
       |ID: UUID,
       |$cols) extends ZooObject {
       |
       |def select: Map[String, _] = Map(
       |$selectMap
       |)
       |
       |}""".stripMargin
  }

  def codePlainQueryObject: String = {
    val getResultParameters = table.columns.map(c => {
      val special = Seq("UUID", "List[Int]")
      val column = ColumnCode(c)
      column.typeString match {
        case "UUID" => "r.nextObject.asInstanceOf[UUID]"
        case "List[Int]" => "r.<<[Seq[Int]].toList"
        case _ => "r.<<"
      }
    }).mkString(", ")

    s"""package $packageString
       |import java.util.UUID
       |import slick.jdbc.GetResult
       |import xyz.discretezoo.web.PlainSQLSupport
       |import xyz.discretezoo.web.ZooPostgresProfile.api._
       |
       |object $plainQueryObject extends PlainSQLSupport[$caseClass] {
       |
       |override val tableName: String = "$dbTableName"
       |override implicit val getResult: GetResult[$caseClass] = GetResult(r => $caseClass(r.nextObject.asInstanceOf[UUID], $getResultParameters))
       |
       |val inCollection: Map[String, String] = Map(
       |"$tableName" -> "ID"
       |)
       |
       |}""".stripMargin
  }

  def jsonSupportMap: String = {
    val columns = columnCodeList.map(c => s"        ${c.jsonWriterMapItem}").mkString(",\n")
    s"""case o: $caseClass => JsObject(
       |        List(
       |$columns
       |        ).flatten: _*
       |      )""".stripMargin
  }

  // from here on react stuff

  def jsonObjectProperties: String = {
    val columns = columnCodeList.map(_.jsonObjectProperties).mkString(",\n")
    s""""$tableObject": {
       |$columns
       |}
     """.stripMargin
  }

  def collectionsData: String = {
    val columns = columnCodeList.map(_.jsonObjectProperties).mkString(",\n")
    s""""$tableObject": {
       |  $tableName: {
       |    "id": "$tableName",
       |    "name": "Table description"
       |  }
       |}
     """.stripMargin
  }

}
