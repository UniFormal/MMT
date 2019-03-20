package info.kwarc.mmt.sql.codegen

import java.io.PrintWriter

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.sql.{SQLBridge, SchemaLang, Table}

object CodeGenerator {

  def writeToFile(p: String, s: String): Unit = {
    val pw = new PrintWriter(new java.io.File(p))
    try pw.write(s) finally pw.close()
  }

  def main(args: Array[String]): Unit = {
    args.foreach(println)
    val dirPaths = ProjectPaths(
      args.headOption.getOrElse("~/DiscreteZooOutput"),
      "backend/src/main/scala/xyz/discretezoo/web",
      "frontend/src",
      "db"
    )
    val jdbcInfo = JDBCInfo("jdbc:postgresql://localhost:5432/discretezoo2", "discretezoo", "D!screteZ00")

    val controller = Controller.make(true, true, List())
    val graphPath = SchemaLang._base ? "Graph"
    val mxPath = SchemaLang._base ? "Maniplex"
    val exJoe = SchemaLang._base ? "MatrixS"
    val exJane = SchemaLang._base ? "MatrixWithCharacteristicS"
    val mPath = SchemaLang._base ? "Matrices"
    val maybeTable: Option[Table] = SQLBridge.test(exJane) match {
      case t: Table => Some(t)
      case _ => None
    }
    val sg = "Joe"

    // remove later
    controller.handleLine("build ODK/DiscreteZOO mmt-omdoc")

    def isInputTheory(t: Theory) = {
      val schemaLangIsMetaTheory = t.meta.contains(SchemaLang._path)
      val isInSchemaGroup = t.metadata.get(SchemaLang.schemaGroup).headOption.exists(_.value.toString == sg)
      schemaLangIsMetaTheory && isInSchemaGroup
    }

    val theories = controller.backend.getArchive("ODK/DiscreteZOO").get.allContent.map(controller.getO).collect({
      case Some(t : Theory) if isInputTheory(t) => t
    })
//    collect {
//      case Some(t : Theory) if t.meta.contains(SchemaLang._base.toMPath) => t.path
//    }
    theories.map(t => t).foreach(println)

    println(" - - - - - ")

    maybeTable.foreach(t => {

      val generate = false
      val prefix = "TEST"
      val tableCode = TableCode(prefix, t)
      val dbCode = DatabaseCode(dirPaths, prefix, Seq(tableCode), jdbcInfo) // TODO: one table only
      val name = {t.name}

//      t.columns.foreach(c => println(c))

//      t.columns.map(_.collection).collect({
//        case Some(info) => info
//      }).map(_.metadata).foreach(println)

//      println(t)

      if (generate) {
        // backend
        dbCode.createTablePackageDirectories()
        writeToFile(s"${dirPaths.backendPackagePath}/JsonSupport.scala", dbCode.jsonSupportCode)
        writeToFile(s"${dirPaths.backendPackagePath}/Create.scala", dbCode.mainCreateCode)
        writeToFile(s"${dirPaths.dbPackagePath}/ZooDb.scala", dbCode.fileDbCode)
        writeToFile(s"${dbCode.tablePackagePath(name)}/$name.scala", tableCode.codeCaseClass)
        writeToFile(s"${dbCode.tablePackagePath(name)}/${name}PlainQuery.scala", tableCode.codePlainQueryObject)
        writeToFile(s"${dbCode.tablePackagePath(name)}/${name}Table.scala", tableCode.codeTableClass)
        // frontend
        writeToFile(s"${dirPaths.frontendPath}/objectProperties.json", dbCode.objectPropertiesJSON)
      }

    })
  }

}
