package info.kwarc.mmt.sql.codegen

import java.io.PrintWriter

import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.{OMA, OMS}
import info.kwarc.mmt.sql.{Column, SQLBridge, SchemaLang, Table}

object CodeGenerator {

  def writeToFile(p: String, s: String): Unit = {
    val pw = new PrintWriter(new java.io.File(p))
    try pw.write(s) finally pw.close()
  }

  def main(args: Array[String]): Unit = {
    val outputDir = args(0)
    val archiveId = args(1)
    val schemaGroup = if (args.length > 2) Some(args(2)) else None

    val dirPaths = ProjectPaths(
      outputDir,
      "backend/src/main/scala/xyz/discretezoo/web",
      "frontend/src",
      "db"
    )
    val jdbcInfo = JDBCInfo("jdbc:postgresql://localhost:5432/discretezoo2", "discretezoo", "D!screteZ00")

    val controller = Controller.make(true, true, List())
//    val graphPath = SchemaLang._base ? "Graph"
//    val mxPath = SchemaLang._base ? "Maniplex"
//    val exJoe = SchemaLang._base ? "MatrixS"
//    val exJane = SchemaLang._base ? "MatrixWithCharacteristicS"
//    val mPath = SchemaLang._base ? "Matrices"

    // remove later
    controller.handleLine("build ODK/DiscreteZOO mmt-omdoc")

    def isInputTheory(t: Theory) = {
      val schemaLangIsMetaTheory = t.meta.contains(SchemaLang._path)
      val isInSchemaGroup = schemaGroup.forall(sg => {
        t.metadata.get(SchemaLang.schemaGroup).headOption.exists(_.value.toString == sg)
      })
      schemaLangIsMetaTheory && isInSchemaGroup
    }

    val theories = controller.backend.getArchive(archiveId).get.allContent.map(controller.getO).collect({
      case Some(t : Theory) if isInputTheory(t) => t
    })

    val prefix = "MBGEN"
    val generate = true

    val tableCodes = theories.map(theory => {
      SQLBridge.test(theory.path) match {
        case t: Table => {
          val description: String = theory.metadata.get(SchemaLang.datasetName).headOption.map(_.value.toString).getOrElse("")
          Some(TableCode(prefix, description, t))
        }
        case _ => None
      }
    }).collect({ case Some(t: TableCode) => t })

    val dbCode = DatabaseCode(dirPaths, prefix, tableCodes, jdbcInfo)

    if (generate) {
      // backend
      dbCode.createTablePackageDirectories()
      writeToFile(s"${dirPaths.backendPackagePath}/JsonSupport.scala", dbCode.jsonSupportCode)
      writeToFile(s"${dirPaths.backendPackagePath}/Create.scala", dbCode.mainCreateCode)
      writeToFile(s"${dirPaths.dbPackagePath}/ZooDb.scala", dbCode.fileDbCode)
      tableCodes.foreach(tableCode => {
        val name = tableCode.table.name
        writeToFile(s"${dbCode.tablePackagePath(name)}/$name.scala", tableCode.codeCaseClass)
        writeToFile(s"${dbCode.tablePackagePath(name)}/${name}PlainQuery.scala", tableCode.codePlainQueryObject)
        writeToFile(s"${dbCode.tablePackagePath(name)}/${name}Table.scala", tableCode.codeTableClass)
      })
      // frontend
      writeToFile(s"${dirPaths.frontendPath}/objectProperties.json", dbCode.objectPropertiesJSON)
      writeToFile(s"${dirPaths.frontendPath}/collectionsData.json", dbCode.collectionDataJSON)
      writeToFile(s"${dirPaths.frontendPath}/settings.json", dbCode.settingsJSON)
    }

  }

}
