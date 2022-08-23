package info.kwarc.mmt.sql.codegen

import info.kwarc.mmt.api.frontend.actions.{Action, ActionCompanion, ActionState}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.sql.{SQLBridge, SchemaLang}

class CodeGenerator extends Extension {
  override def start(args: List[String]): Unit = {
    controller.extman.addExtension(MBGenActionCompanion)
  }

}

case class MBGenAction(args: List[String]) extends Action {
  override def toParseString: String = "mbgen "+args.mkString(" ")

  private def isInputTheory(t: Theory, schemaGroup: Option[String]): Boolean = {
    val schemaLangIsMetaTheory = t.meta.contains(SchemaLang._path)
    val isInSchemaGroup = schemaGroup.forall(sg => {
      t.metadata.get(SchemaLang.schemaGroup).headOption.exists(_.value.toString == sg)
    })
    if (schemaLangIsMetaTheory) println(t.metadata.get(SchemaLang.schemaGroup))
    schemaLangIsMetaTheory && isInSchemaGroup
  }

  def apply(): Unit = {
    val outputDir = args.head
    val handle = scala.io.Source.fromFile(outputDir + "/settings.txt")
    val settings = try {
      val lines = handle.getLines().toSeq
      val schemaGroup = if (lines(1).nonEmpty) Some(lines(1)) else None
      val jdbc = JDBCInfo(lines(2), lines(3), lines(4))
      val prefix = if (lines.length < 6 || lines(5).nonEmpty) lines(5) else "MBGEN"
      CodeGenSettings(lines.head, schemaGroup, jdbc, prefix)
    } finally handle.close()

    val dirPaths = ProjectPaths(
      outputDir,
      "backend/src/main/scala/xyz/discretezoo/web",
      "frontend/src",
      "db"
    )

    // remove later
    controller.handleLine(s"build ${settings.archiveId} mmt-omdoc")
    controller.handleLine("finish build")

    // add input theories to search
    val t = controller.backend.getArchive(settings.archiveId).get.allContent.map(controller.getO)
    t.collect({
      case Some(theory : Theory) => theory.path //  if isInputTheory(theory, schemaGroup)
    }).foreach(println)

    val paths = t.collect({
      case Some(theory : Theory) if isInputTheory(theory, settings.schemaGroup) => theory.path
    })

    val tables = new Tables(settings.prefix, dirPaths, p => SQLBridge.test2(p, controller), settings.jdbcInfo, paths)
    tables.databaseCode.writeAll(true)

  }
}

object MBGenActionCompanion extends ActionCompanion("run mbgen", "mbgen") {

  import Action._

  def parserActual(implicit state: ActionState) = str.* ^^ { l => MBGenAction(l) }
}

case class CodeGenSettings(archiveId: String, schemaGroup: Option[String], jdbcInfo: JDBCInfo, prefix: String)