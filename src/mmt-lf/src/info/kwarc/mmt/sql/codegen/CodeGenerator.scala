package info.kwarc.mmt.sql.codegen

import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.api.frontend.{Controller, Extension}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.sql.{Column, SQLBridge, SchemaLang, Table}

import scala.collection.mutable

class CodeGenerator extends Extension {

  private def isInputTheory(t: Theory, schemaGroup: Option[String]): Boolean = {
    val schemaLangIsMetaTheory = t.meta.contains(SchemaLang._path)
    val isInSchemaGroup = schemaGroup.forall(sg => {
      t.metadata.get(SchemaLang.schemaGroup).headOption.exists(_.value.toString == sg)
    })
    schemaLangIsMetaTheory && isInSchemaGroup
  }

  override def start(args: List[String]): Unit = {
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
    val prefix = "MBGEN"

    // remove later
    controller.handleLine(s"build $archiveId mmt-omdoc")

    // add input theories to search
    val paths = controller.backend.getArchive(archiveId).get.allContent.map(controller.getO).collect({
      case Some(theory : Theory) if isInputTheory(theory, schemaGroup) => theory.path
    })

    val tables = new Tables(prefix, dirPaths, p => SQLBridge.test2(p, controller), jdbcInfo, paths)
    tables.databaseCode.writeAll(true)


  }

}