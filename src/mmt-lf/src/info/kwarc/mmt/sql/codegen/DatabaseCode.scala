package info.kwarc.mmt.sql.codegen

import java.io.PrintWriter

case class DatabaseCode(prefix: String, paths: ProjectPaths, tables: Seq[Seq[TableCode]], dbInfo: JDBCInfo, viewCreation: Seq[(String, String)]) {

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
    tables.foreach(s => {
      val t = s.head
      val tPath = tablePackagePath(t.info.name)
      val dir = new java.io.File(tPath)
      if (generate && !dir.exists()) dir.mkdir()
//      val name = t.table.name
      s.foreach(t => tableFiles(t).foreach(f => {
        val out = Some(s"$tPath/${t.info.name}${f._3}.scala")
        CodeFile(f._1, tableTempPath(f._2), out).writeToFile(generate)
      }))
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
  private def allTableCodes: Seq[TableCode] = tables.flatten
  private def nonViewTableCodes: Seq[TableCode] = tables.map(_.head)

  private def tableObjects(seq: Seq[TableCode]) = seq.map(_.dbTableObject).mkString("\n")

  // backend code

  def zooCreateRepl: Map[String, String] = Map(
    "//tableObjects" -> tableObjects(nonViewTableCodes),
    "//views" -> viewCreation.map(_._2).mkString("\n"),
    "//importTablePackages" -> nonViewTableCodes.map(_.tableClassImport).mkString("\n"),
    "//schemaCreateList" -> nonViewTableCodes.map(_.zooSchemaCreate).mkString(", "),
    "//viewCreateList" -> viewCreation.map(_._1).mkString(", ")
  )

  private def jsonSupportRepl: Map[String, String] = Map(
    "//importTablePackages" -> allTableCodes.map(_.jsonSupportImport).mkString("\n"),
    "//casesToJson" -> allTableCodes.map(_.jsonSupportMap).mkString("\n")
  )


  private def zooDbRepl: Map[String, String] = Map(
    "//tableObjects" -> tableObjects(allTableCodes),
    "%jdbc%" -> dbInfo.jdbc,
    "%user%" -> dbInfo.user,
    "%pass%" -> dbInfo.pass,
    "//importTablePackages" -> allTableCodes.map(_.zooDbImport).mkString("\n"),
    "//getQueryMatches" -> allTableCodes.map(_.dbGetQueryMatches).mkString("\n"),
    "//countQueryMatches" -> allTableCodes.map(_.dbCountQueryMatches).mkString("\n")
  )

  // frontend jsons

  private def objectPropertiesJSON: String = {
    val code = allTableCodes.map(_.jsonObjectProperties).mkString(",\n")
    s"""{
       |$code
       |}
     """.stripMargin
  }

  private def collectionDataJSON: String = {
    val code = allTableCodes.map(_.collectionsData).mkString(",\n")
    s"""{
       |$code
       |}
     """.stripMargin
  }

  private def settingsJSON: String = {
    val code = allTableCodes.map(_.defaultColumns).mkString(",\n")
    s"""{
       |    "title": "Search",
       |    "defaultColumns": {
       |$code
       |    }
       |}
     """.stripMargin
  }

}
