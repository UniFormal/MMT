package info.kwarc.mmt.sql.codegen

import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.sql.Table

import scala.collection.mutable

class Tables(prefix: String, dbPackagePath: String, readerCallback: MPath => Any) {

  // - None: haven't attempted retrieval yet
  // - Some(None): retrieved, not table
  private val pathsToTables: mutable.Map[MPath, Option[Option[TableCode]]] = mutable.Map[MPath, Option[Option[TableCode]]]()

  def processTables(paths: Seq[MPath] = Seq()): Map[MPath, TableCode] = {
    if (paths.nonEmpty) paths.foreach(addPath)
    while (unprocessed.nonEmpty) {
      unprocessed.foreach(path => {
        readerCallback(path) match {
          case table: Table =>
            table.includes.foreach(addPath)
            processPath(path, Some(getTableCode(table)))
          case _ => processPath(path, None)
        }
      })
    }
    pathsToTables.collect({ case (p: MPath, Some(Some(t: TableCode))) => (p, t) }).toMap
  }

  private def getTableCode(table: Table): TableCode = {
    val joins = table.columns.map(c => (c, c.foreignKey))
      .collect({ case (c, Some(p: MPath)) => (c, readerCallback(p))})
      .collect({ case (c, t: Table) => JoinCode(TableInfo(prefix, t.name), c.name) })
    val cols = table.columns.map(c => ColumnCode(c, joins.find(_.columnName == c.name)))
    TableCode(TableInfo(prefix, table.name), dbPackagePath, cols, table.datasetName.getOrElse(""), joins.map(_.tbInfo))
  }

  private def addPath(p: MPath): Unit = {
    if (!pathsToTables.contains(p)) pathsToTables += (p -> None)
  }

  private def processPath(p: MPath, t: Option[TableCode]): Unit = {
    if (pathsToTables.contains(p)) pathsToTables -= p
    pathsToTables += (p -> Some(t))
  }

  private def unprocessed: Seq[MPath] = pathsToTables.collect({ case (p, None) => p }).toSeq

  def print(): Unit = pathsToTables.foreach({
    case (p, None) => println(s"Unchecked: $p")
    case (p, Some(None)) => println(s"Not table: $p")
    case (p, Some(Some(t))) => println(s"Table:     ${t.info.name}")
  })

}
