package info.kwarc.mmt.sql.codegen

import java.io.PrintWriter

case class DatabaseCode(prefix: String, paths: ProjectPaths, tables: Map[TableInfo, TableCode], dbInfo: JDBCInfo, views: Seq[(String, String)]) {

  private def writeToFile(f: String, s: String, write: Boolean): Unit = {
    if (write) {
      val pw = new PrintWriter(new java.io.File(f))
      try pw.write(s) finally pw.close()
    }
    else println(s)
  }

  private def tableFiles(t: TableCode): Seq[(Map[String, String], String, String)] = {
    Seq(
      (t.caseClassRepl, "CaseClass", ""),
      (t.plainQueryRepl, "PlainQueryObject", "PlainQuery"),
      (t.tableClassRepl, "TableClass", "Table")
    )
  }

  def writeAll(generate: Boolean = true): Unit = {
    // backend
    Seq((zooCreateRepl, "Create"), (jsonSupportRepl, "JsonSupport"), (zooDbRepl, "db/ZooDb"))
      .foreach(t => CodeFile(t._1, s"${paths.backendPackagePath}/${t._2}.scala").writeToFile(generate))
    val tempDir = s"${paths.dbPackagePath}/temp"
    def tableTempPath(s: String) = s"$tempDir/$s.scala"
    tables.foreach(mapItem => {
      val t = mapItem._2
      val tPath = tablePackagePath(t.info.name)
      val dir = new java.io.File(tPath)
      if (generate && !dir.exists()) dir.mkdir()
//      val name = t.table.name
      tableFiles(t).foreach(f => {
        val out = Some(s"$tPath/${t.info.name}${f._3}.scala")
        CodeFile(f._1, tableTempPath(f._2), out).writeToFile(generate)
      })
    })

    // delete temp dir
    Seq(tableTempPath("CaseClass"), tableTempPath("PlainQueryObject"), tableTempPath("TableClass")).foreach(tp => {
      val temp = new java.io.File(tp)
      temp.delete()
    })
    val dir = new java.io.File(tempDir)
    println("delete:", dir.delete())

    // frontend
    writeToFile(s"${paths.frontendPath}/config/objectProperties.json", objectPropertiesJSON, generate)
    writeToFile(s"${paths.frontendPath}/config/collectionsData.json", collectionDataJSON, generate)
    writeToFile(s"${paths.frontendPath}/config/settings.json", settingsJSON, generate)
  }

  // helpers

  private def tablePackagePrefix: String = prefix
  private def tablePackagePath(name: String) = s"${paths.dbPackagePath}/$tablePackagePrefix$name"

  private val tableObjects = tables.map(_._2.dbTableObject).mkString("\n")

  // backend code

  def zooCreateRepl: Map[String, String] = Map(
    "//tableObjects" -> tableObjects,
    "//views" -> views.map(_._2).mkString("\n"),
    "//importTablePackages" -> tables.map(_._2.tableClassImport).mkString("\n"),
    "//schemaCreateList" -> tables.map(_._2.zooSchemaCreate).mkString(", "),
    "//viewCreateList" -> views.map(_._1).mkString(", ")
  )

  private def jsonSupportRepl: Map[String, String] = Map(
    "//importTablePackages" -> tables.map(_._2.jsonSupportImport).mkString("\n"),
    "//casesToJson" -> tables.map(_._2.jsonSupportMap).mkString("\n")
  )


  private def zooDbRepl: Map[String, String] = Map(
    "//tableObjects" -> tableObjects,
    "%jdbc%" -> dbInfo.jdbc,
    "%user%" -> dbInfo.user,
    "%pass%" -> dbInfo.pass,
    "//importTablePackages" -> tables.map(_._2.zooDbImport).mkString("\n"),
    "//getQueryMatches" -> tables.map(_._2.dbGetQueryMatches).mkString("\n"),
    "//countQueryMatches" -> tables.map(_._2.dbCountQueryMatches).mkString("\n")
  )

  // frontend jsons

  private def objectPropertiesJSON: String = {
    val code = tables.map(_._2.jsonObjectProperties).mkString(",\n")
    s"""{
       |$code
       |}
     """.stripMargin
  }

  private def collectionDataJSON: String = {
    val code = tables.map(_._2.collectionsData).mkString(",\n")
    s"""{
       |$code
       |}
     """.stripMargin
  }

  private def settingsJSON: String = {
    val code = tables.map(_._2.defaultColumns).mkString(",\n")
    s"""{
       |    "title": "Search",
       |    "defaultColumns": {
       |$code
       |    }
       |}
     """.stripMargin
  }

}
