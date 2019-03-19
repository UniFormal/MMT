package info.kwarc.mmt.sql.codegen

//import java.io

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.sql.{SQLBridge, SchemaLang, Table}
import java.io.{File, PrintWriter}

object CodeGenerator {

  def writeToFile(p: String, s: String): Unit = {
    val pw = new PrintWriter(new java.io.File(p))
    try pw.write(s) finally pw.close()
  }

  def main(args: Array[String]): Unit = {
    args.foreach(println)
    val outputDirPath = args.headOption.getOrElse("~/DiscreteZooOutput")
    val scalaPath = s"$outputDirPath/backend/src/main/scala/xyz/discretezoo/web"
    val reactPath = s"$outputDirPath/frontend/src"
    val relDbPackagePath = s"$scalaPath/db"

    val controller = Controller.make(true, true, List())
    val graphPath = SchemaLang._base ? "Graph"
    val mxPath = SchemaLang._base ? "Maniplex"
    val examplePath = SQLBridge.example
    val maybeTable: Option[Table] = SQLBridge.test(examplePath) match {
      case t: Table => Some(t)
      case _ => None
    }

    println(" - - - - - ")
    maybeTable.foreach(t => {
      val name = {t.name}
      val tablePackagePath = s"$relDbPackagePath/Zoo$name"
      val tablePackageDir = new java.io.File(tablePackagePath)
      if (!tablePackageDir.exists()) tablePackageDir.mkdir()

      val tableCode = TableCode(t)
      val dbCode = DatabaseCode("TEST", Seq(tableCode), "jdbc:postgresql://localhost:5432/discretezoo2", "discretezoo", "D!screteZ00")

      writeToFile(s"$relDbPackagePath/ZooDb.scala", dbCode.fileDbCode)
      writeToFile(s"$scalaPath/JsonSupport.scala", dbCode.jsonSupportCode)
      writeToFile(s"$tablePackagePath/$name.scala", tableCode.codeCaseClass)
      writeToFile(s"$tablePackagePath/${name}PlainQuery.scala", tableCode.codePlainQueryObject)
      writeToFile(s"$tablePackagePath/${name}Table.scala", tableCode.codeTableClass)

      writeToFile(s"$reactPath/objectProperties.json", dbCode.objectPropertiesJSON)

    })


//    maybeTable.foreach(t => println(TableCode(t).jsonSupportMap))
  }

}
