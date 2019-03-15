package info.kwarc.mmt.sql.codegen

import info.kwarc.mmt.sql.{Column, Table}

case class TableCode(table: Table) {

  def tableName: String = table.name
  def tableObject: String = s"tb$tableName"
  // class names
  def plainQueryObject = s"${tableName}PlainQuery"
  def tableClass = s"${tableName}Table"
  def caseClass: String = table.name
  // database
  private def dbTableName = s"ZOO_${tableName.toUpperCase}" // table name in the database

  def packageString = s"xyz.discretezoo.web.db.Zoo$tableName" // package for the table specific files
  private def columnCodeList: Seq[ColumnCode] = table.columns.map(ColumnCode)
  private def primaryKeyColumn: Column = table.columns.filter(_.isPrimaryKey).head

//  def inCollectionMap: String = collections.map(_.inCollectionItem).mkString(",\n")

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
       |$accessorMethods
       |
       |def * : ProvenShape[$caseClass] = (
       |$caseClassMapParameters ::
       |HNil
       |).mapTo[$caseClass]
       |
       |val select: Map[String, Rep[_]] = Map(
       |$selectMap
       |)
       |
       |val inCollection: Map[String, Rep[Boolean]] = Map(
       |"${primaryKeyColumn.name}" -> true
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
      val special = Seq("UUID", "List[Int]", "List[List[Int]]")
      if (special.contains(c.dbtype.toString)) s"r.nextObject.asInstanceOf[${c.dbtype.toString}]"
      else "r.<<"
    }).mkString(", ")

    s"""package $packageString
       |import java.util.UUID
       |import slick.jdbc.GetResult
       |import xyz.discretezoo.web.PlainSQLSupport
       |
       |object $plainQueryObject extends PlainSQLSupport[$caseClass] {
       |
       |override val tableName: String = "$dbTableName"
       |override implicit val getResult: GetResult[$caseClass] = GetResult(r => $caseClass($getResultParameters))
       |
       |val inCollection: Map[String, String] = Map(
       |"Ex" -> "ID"
       |)
       |
       |}""".stripMargin
  }

  def jsonSupportMap: String = {
    val columns = columnCodeList.map(_.jsonWriterMapItem).mkString(",\n")
    s"""case o: $caseClass => JsObject(
       |List(
       |$columns
       |).flatten: _*
       |)
     """.stripMargin
  }

  // from here on react stuff

  def jsonObjectProperties: String = {
    val columns = columnCodeList.map(_.jsonObjectProperties).mkString(",\n")
    s""""$tableObject": {
       |$columns
       |}
     """.stripMargin
  }

}
