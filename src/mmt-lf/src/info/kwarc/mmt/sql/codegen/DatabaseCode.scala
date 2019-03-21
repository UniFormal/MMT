package info.kwarc.mmt.sql.codegen

case class DatabaseCode(paths: ProjectPaths, name: String, tables: Seq[TableCode], dbInfo: JDBCInfo) {

  def tablePackagePrefix: String = name
  def tablePackagePath(name: String) = s"${paths.dbPackagePath}/$tablePackagePrefix$name"

  def createTablePackageDirectories(): Unit = {
    tables.foreach(t => {
      val dir = new java.io.File(tablePackagePath(t.tableName))
      if (!dir.exists()) dir.mkdir()
    })
  }

  private val tableObjects = tables
    .map(t => s"object ${t.tableObject} extends TableQuery(new ${t.tableClass}(_))").mkString(",\n")

  def mainCreateCode: String = {
    val importTablePackages = tables
      .map(t => s"import ${t.packageString}.${t.tableClass}").mkString(",\n")
    val schemaCreateList = tables.map(t => s"${t.tableObject}.schema.create").mkString(", ")
    scala.io.Source.fromFile(s"${paths.backendPackagePath}/Create.scala").mkString
      .replaceFirst("//importTablePackages", importTablePackages)
      .replaceFirst("//tableObjects", tableObjects)
      .replaceFirst("//schemaCreateList", schemaCreateList)
  }

  def jsonSupportCode: String = {
    val importTablePackages = tables
      .map(t => s"import ${t.packageString}.${t.caseClass}").mkString(",\n")
    val casesToJson = tables
      .map(_.jsonSupportMap).mkString("\n")
    scala.io.Source.fromFile(s"${paths.backendPackagePath}/JsonSupport.scala").mkString
      .replaceFirst("//importTablePackages", importTablePackages)
      .replaceFirst("//casesToJson", casesToJson)
  }

  def fileDbCode: String = {
    val importTablePackages = tables
      .map(t => s"import ${t.packageString}.{${t.plainQueryObject}, ${t.tableClass}}").mkString(",\n")
    val getQueryMatches = tables
      .map(t =>
        s"""      case ("${t.tableObject}", true) => ${t.plainQueryObject}.get(rp)
           |      case ("${t.tableObject}", false) => tb${t.tableName}.dynamicQueryResults(rp).result"""
          .stripMargin).mkString("\n")
    val countQueryMatches = tables
      .map(t =>
        s"""      case ("${t.tableObject}", true) => ${t.plainQueryObject}.count(qp)
           |      case ("${t.tableObject}", false) => tb${t.tableName}.dynamicQueryCount(qp).length.result"""
          .stripMargin).mkString("\n")
    scala.io.Source.fromFile(s"${paths.dbPackagePath}/ZooDb.scala").mkString
      .replaceFirst("//importTablePackages", importTablePackages)
      .replaceFirst("//tableObjects", tableObjects)
      .replaceFirst("//getQueryMatches", getQueryMatches)
      .replaceFirst("//countQueryMatches", countQueryMatches)
      .replaceFirst("%jdbc%", dbInfo.jdbc)
      .replaceFirst("%user%", dbInfo.user)
      .replaceFirst("%pass%", dbInfo.pass)
  }

  // react

  def objectPropertiesJSON: String = {
    val code = tables.map(_.jsonObjectProperties).mkString(",\n")
    s"""{
       |$code
       |}
     """.stripMargin
  }

  def collectionDataJSON: String = {
    val code = tables.map(_.collectionsData).mkString(",\n")
    s"""{
       |$code
       |}
     """.stripMargin
  }

  def settingsJSON: String = {
    val code = tables.map(_.defaultColumns).mkString(",\n")
    s"""{
       |    "title": "Search",
       |    "defaultColumns": {
       |$code
       |    }
       |}
     """.stripMargin
  }

}
