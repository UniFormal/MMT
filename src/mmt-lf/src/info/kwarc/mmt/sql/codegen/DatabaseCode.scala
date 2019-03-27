package info.kwarc.mmt.sql.codegen

import java.io.PrintWriter

case class DatabaseCode(paths: ProjectPaths, name: String, tables: Seq[TableCode], dbInfo: JDBCInfo) {

  private def writeToFile(f: String, s: String, write: Boolean): Unit = {
    if (write) {
      val pw = new PrintWriter(new java.io.File(f))
      try pw.write(s) finally pw.close()
    }
  }

  def writeAll(generate: Boolean = true): Unit = {
    // backend
    CodeFile(zooCreateRepl, s"${paths.backendPackagePath}/Create.scala").writeToFile(generate)
    CodeFile(jsonSupportRepl, s"${paths.backendPackagePath}/JsonSupport.scala").writeToFile(generate)
    CodeFile(zooDbRepl, s"${paths.dbPackagePath}/ZooDb.scala").writeToFile(generate)
    tables.foreach(t => {
      val tPath = tablePackagePath(t.tableName)
      val dir = new java.io.File(tPath)
      def tableTempPath(s: String) = s"${paths.dbPackagePath}/temp/$s.scala"
      def tableFilePath(s: String) = Some(s"$tPath/${t.tableName}$s.scala")
      if (generate) {
        if (!dir.exists()) dir.mkdir()
      }
      val name = t.table.name
      CodeFile(t.caseClassRepl, tableTempPath("CaseClass"), tableFilePath("")).writeToFile(generate)
      CodeFile(t.plainQueryRepl, tableTempPath("PlainQueryObject"), tableFilePath("PlainQuery")).writeToFile(generate)
      CodeFile(t.tableClassRepl, tableTempPath("TableClass"), tableFilePath("Table")).writeToFile(generate)
    })
    // frontend
    writeToFile(s"${paths.frontendPath}/objectProperties.json", objectPropertiesJSON, generate)
    writeToFile(s"${paths.frontendPath}/collectionsData.json", collectionDataJSON, generate)
    writeToFile(s"${paths.frontendPath}/settings.json", settingsJSON, generate)
  }

  // helpers

  private def tablePackagePrefix: String = name
  private def tablePackagePath(name: String) = s"${paths.dbPackagePath}/$tablePackagePrefix$name"

  private val tableObjects = tables.map(_.zooDbObject).mkString(",\n")

  // backend code

  private def zooCreateRepl: Map[String, String] = Map(
    "//tableObjects" -> tableObjects,
    "//importTablePackages" -> tables.map(_.zooCreateImport).mkString(",\n"),
    "//schemaCreateList" -> tables.map(_.zooSchemaCreate).mkString(", ")
  )

  private def jsonSupportRepl: Map[String, String] = Map(
    "//importTablePackages" -> tables.map(_.jsonSupportImport).mkString(",\n"),
    "//casesToJson" -> tables.map(_.jsonSupportMap).mkString("\n")
  )


  private def zooDbRepl: Map[String, String] = Map(
    "//tableObjects" -> tableObjects,
    "%jdbc%" -> dbInfo.jdbc,
    "%user%" -> dbInfo.user,
    "%pass%" -> dbInfo.pass,
    "//importTablePackages" -> tables.map(_.zooDbImport).mkString(",\n"),
    "//getQueryMatches" -> tables.map(_.dbGetQueryMatches).mkString("\n"),
    "//countQueryMatches" -> tables.map(_.dbCountQueryMatches).mkString("\n")
  )

  // frontend jsons

  private def objectPropertiesJSON: String = {
    val code = tables.map(_.jsonObjectProperties).mkString(",\n")
    s"""{
       |$code
       |}
     """.stripMargin
  }

  private def collectionDataJSON: String = {
    val code = tables.map(_.collectionsData).mkString(",\n")
    s"""{
       |$code
       |}
     """.stripMargin
  }

  private def settingsJSON: String = {
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
