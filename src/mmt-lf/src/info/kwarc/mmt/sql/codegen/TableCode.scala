package info.kwarc.mmt.sql.codegen

import info.kwarc.mmt.sql.{Column, Table}

case class TableCode(prefix: String, dbPackagePath: String, table: Table) {

  def tableName: String = table.name
  def tablePackageName : String = prefix + tableName

  private def packageString = s"xyz.discretezoo.web.db.$tablePackageName" // package for the table specific files
  private def columnCodeList: Seq[ColumnCode] = table.columns.map(ColumnCode)
  private def tableObject: String = s"tb$tableName"

  private val baseRepl: Map[String, String] = Map(
    "//package" -> s"package $packageString",
    "%caseClass%" -> caseClass,
    "%dbTableName%" -> s"${prefix}_${tableName.toUpperCase}" // table name in the database
  )

  // CaseClass
  private def caseClass: String = table.name
  def caseClassRepl: Map[String, String] = baseRepl ++ Map(
    "//cols" -> columnCodeList.map(_.caseClassField).mkString(",\n"),
    "//selectMap" -> columnCodeList.map(_.selectMapCaseClass).mkString(",\n")
  )

  // PlainQueryObject
  private def plainQueryObject: String = s"${tableName}PlainQuery"
  def plainQueryRepl: Map[String, String] = baseRepl ++ Map(
    "%plainQueryObject%" -> plainQueryObject,
    "%tableName%" -> tableName,
    "%getResultParameters%" -> table.columns.map(c => ColumnCode(c).getResultItem).mkString(", ")
  )

  // TableClass
  private def tableClass = s"${tableName}Table"
  def tableClassRepl: Map[String, String] = baseRepl ++ Map(
    "//accessorMethods" -> columnCodeList.map(_.accessorMethod).mkString("\n"),
    "//caseClassMapParameters" -> columnCodeList.map(_.nameCamelCase).mkString(" ::\n"),
    "//selectMap" -> columnCodeList.map(_.selectMapTableClass).mkString(",\n"),
    "%tableClass%" -> tableClass
  )

  // Create
  def zooCreateImport: String = s"import $packageString.$tableClass"
  def zooSchemaCreate: String = s"$tableObject.schema.create"

  // ZooDb
  def zooDbImport: String = s"import $packageString.{$plainQueryObject, $tableClass}"
  def zooDbObject: String = s"object $tableObject extends TableQuery(new $tableClass(_))"
  def dbCountQueryMatches: String =
    s"""      case ("$tableObject", true) => $plainQueryObject.count(qp)
       |      case ("$tableObject", false) => tb$tableName.dynamicQueryCount(qp).length.result""".stripMargin

  def dbGetQueryMatches: String =
    s"""      case ("$tableObject", true) => $plainQueryObject.get(rp)
       |      case ("$tableObject", false) => tb$tableName.dynamicQueryResults(rp).result""".stripMargin

//  TODO def inCollectionMap: String = collections.map(_.inCollectionItem).mkString(",\n")

  // JsonSupport
  def jsonSupportImport: String = s"import $packageString.$caseClass"
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
    s""""$tableObject": {
       |  "$tableName": {
       |    "id": "$tableName",
       |    "name": "${table.datasetName}"
       |  }
       |}
     """.stripMargin
  }

  def defaultColumns: String = {
    val columns = table.columns.filter(_.isDisplayedByDefault).map(c => s""""${c.name}"""").mkString(", ")
    s"""      "$tableObject": [$columns]""".stripMargin
  }

}
