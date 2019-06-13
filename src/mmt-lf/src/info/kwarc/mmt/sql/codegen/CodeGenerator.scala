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
    schemaLangIsMetaTheory && isInSchemaGroup
  }

  def apply() {
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
object MBGenActionCompanion extends ActionCompanion("run mbgen", "mbgen") {

  import Action._

  def parserActual(implicit state: ActionState) = str.* ^^ { l => MBGenAction(l) }
}