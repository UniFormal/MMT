package info.kwarc.mmt.sql.codegen

case class TableCode( info: TableInfo,
                      dbPackagePath: String,
                      columns: Seq[ColumnCode],
                      datasetName: String,
                      joins: Seq[TableInfo]
) extends CodeHelper {

  private val baseRepl: Map[String, String] = Map(
    "//package" -> s"package ${info.packageString}",
    "%caseClass%" -> info.caseClass
  )

  // CaseClass
  def caseClassRepl: Map[String, String] = baseRepl ++ Map(
    "//cols" -> columns.map(_.caseClassField).mkString(",\n"),
    "//selectMap" -> columns.map(_.selectMapCaseClass).mkString(",\n")
  )

  // PlainQueryObject
  private def plainQueryObject: String = s"${info.name}PlainQuery"
  def plainQueryRepl: Map[String, String] = baseRepl ++ Map(
    "%plainQueryObject%" -> plainQueryObject,
    "%tableName%" -> info.name,
    "%getResultParameters%" -> columns.map(_.getResultItem).mkString(", "),
    "%sqlFrom%" -> info.dbTableName, // table name in the database
    "//columns" -> (quoted("ID") +: columns.filter(_.join.isEmpty).map(_.nameDbQuoted)).map(c => s"""      |${quoted(info.dbTableName)}.$c""").mkString(",\n")
  )

  // TableClass
  def tableClassRepl: Map[String, String] = baseRepl ++ Map(
    "//joinImports" -> joins.map(_.joinImport).mkString("\n"),
    "//joinQueries" -> joins.map(_.joinQuery).mkString("\n"),
    "//accessorMethods" -> columns.map(_.accessorMethod).mkString("\n"),
    "//caseClassMapParameters" -> columns.map(_.nameCamelCase).mkString(" ::\n"),
    "//selectMap" -> columns.map(_.selectMapTableClass).mkString(",\n"),
    "%dbTableName%" -> info.dbTableName, // table name in the database
    "%tableClass%" -> info.tableClass
  )

  // Create
  def tableClassImport: String = s"import ${info.packageString}.${info.tableClass}"
  def zooSchemaCreate: String = s"${info.tableObject}.schema.createIfNotExists"

  // ZooDb
  def zooDbImport: String = s"import ${info.packageString}.{$plainQueryObject, ${info.tableClass}}"
  def dbTableObject: String = s"  object ${info.tableObject} extends TableQuery(new ${info.tableClass}(_))"
  def dbCountQueryMatches: String =
    s"""      case ("${info.tableObject}", true) => $plainQueryObject.count(qp)
       |      case ("${info.tableObject}", false) => tb${info.name}.dynamicQueryCount(qp).length.result""".stripMargin

  def dbGetQueryMatches: String =
    s"""      case ("${info.tableObject}", true) => $plainQueryObject.get(rp)
       |      case ("${info.tableObject}", false) => tb${info.name}.dynamicQueryResults(rp).result""".stripMargin

  //  TODO def inCollectionMap: String = collections.map(_.inCollectionItem).mkString(",\n")

  // JsonSupport
  def jsonSupportImport: String = s"import ${info.packageString}.${info.caseClass}"
  def jsonSupportMap: String = {
    val cols = columns.map(c => s"        ${c.jsonWriterMapItem}").mkString(",\n")
    s"""case o: ${info.caseClass} => JsObject(
       |        List(
       |$cols
       |        ).flatten: _*
       |      )""".stripMargin
  }

  // from here on react stuff

  def jsonObjectProperties: String = {
    val cols = columns.map(_.jsonObjectProperties).mkString(",\n")
    s""""${info.tableObject}": {
       |$cols
       |}
     """.stripMargin
  }

  def collectionsData: String = {
    s""""${info.tableObject}": {
       |  "${info.name}": {
       |    "id": "${info.name}",
       |    "name": "$datasetName"
       |  }
       |}
     """.stripMargin
  }

  def defaultColumns: String = {
    val cols = columns.filter(_.isDisplayedByDefault).map(c => s""""${c.name}"""").mkString(", ")
    s"""      "${info.tableObject}": [$cols]""".stripMargin
  }

  def dbColumns(withID: Boolean): Seq[String] = {
    val maybeID = if (withID) Seq(quoted("ID")) else Seq()
    (maybeID ++ columns.filter(_.join.isEmpty).map(_.nameDbQuoted)).map(c => s"${quoted(info.dbTableName)}.$c")
  }

  def dbJoinSQL: String = {
    columns.collect({
      case ColumnCode(c, Some(j)) => (c, j)
    }).map(t => {
      val tbName = quoted(t._2.tbInfo.dbTableName)
      s"""JOIN $tbName ON $tbName."ID" = ${quoted(info.dbTableName)}.${columnNameDB(t._1)}"""
    }).mkString("\n")
  }

  override def toString: String =
    s"""TableCode: ${info.prefix} ${info.name} ($dbPackagePath) "$datasetName"
       |#columns:
       |${columns.map(_.toString).mkString("\n")}
       |#joins:
       |${joins.map(t => s"${t.prefix} ${t.name}").mkString("\n")}
     """.stripMargin

}
