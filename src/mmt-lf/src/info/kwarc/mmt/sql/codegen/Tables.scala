package info.kwarc.mmt.sql.codegen

import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.sql.Table

import scala.collection.mutable

class Tables(prefix: String, dirPaths: ProjectPaths, readerCallback: MPath => Any, jdbcInfo: JDBCInfo, inputTheories: Seq[MPath] = Seq()) extends CodeHelper {

  // - None: haven't attempted retrieval yet
  // - Some(None): retrieved, not table
  private val dbPackagePath: String = dirPaths.dbPackagePath
  private val builder: mutable.Map[MPath, Option[Option[TableCode]]] = mutable.Map[MPath, Option[Option[TableCode]]]()
  private def inputTableCodes: Seq[TableCode] = inputTheories.map(builder).collect({
    case Some(Some(t: TableCode)) => t
  })

  val tableMap: Map[TableInfo, TableCode] = processTables()
  val viewMap: Map[TableInfo, TableCode] = processViews()
  val views: Seq[(String, String)] = tableMap.values.filter(_.joins.nonEmpty).map(generateView).toSeq

  def databaseCode: DatabaseCode = {
    val tables = tableMap.map(t => Seq(t._2) ++ viewMap.get(t._2.info)).toSeq
    DatabaseCode(prefix, dirPaths, tables, jdbcInfo, views)
  }

  def printBuilder(): Unit = builder.foreach({
    case (p, None) => println(s"Unchecked: $p")
    case (p, Some(None)) => println(s"Not table: $p")
    case (p, Some(Some(t))) => println(s"Table:     ${t.info.name}")
  })

  private def generateView(t: TableCode): (String, String) = {
    val joins = t.joins
    val columnList =  t.dbColumns(true) ++ joins.flatMap(j => tableMap(j).dbColumns(false))
    val viewSQL = s"""  val ${t.info.viewObject}: DBIO[Int] =
       |    sqlu$quotes
       |        CREATE VIEW ${quoted(t.info.dbViewName)} AS
       |        SELECT ${columnList.mkString(", ")}
       |        FROM ${quoted(t.info.dbTableName)}
       |        ${t.dbJoinSQL};
       |    $quotes
     """.stripMargin
    (t.info.viewObject, viewSQL)
  }

  private def processTables(): Map[TableInfo, TableCode] = {
    if (inputTheories.nonEmpty) inputTheories.foreach(addPath)
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
    builder.collect({ case (_, Some(Some(t: TableCode))) => (t.info, t) }).toMap
  }

  private def processViews(): Map[TableInfo, TableCode] = {
    inputTableCodes.filter(_.joins.nonEmpty).map(t => {
      val view = getView(t.info)
      (t.info, view)
    }).toMap
  }

  // assume that the column names are unique
  private def getView(t: TableInfo): TableCode = {
    val tbCode = tableMap(t)
    def getColumns(t: TableInfo) = tableMap(t).columns.filter(_.join.isEmpty)
    val cols = getColumns(t) ++ tbCode.joins.flatMap(getColumns)
    TableCode(TableInfo(t.prefix, t.viewName), dbPackagePath, cols, tbCode.datasetName, Seq())
  }

  private def getTableCode(table: Table): TableCode = {
    val joins = table.columns.map(c => (c, c.foreignKey))
      .collect({ case (c, Some(p: MPath)) => (c, readerCallback(p))})
      .collect({ case (c, t: Table) => JoinCode(TableInfo(prefix, t.name), c.name) })
    val cols = table.columns.map(c => ColumnCode(c, joins.find(_.columnName == c.name)))
    TableCode(TableInfo(prefix, table.name), dbPackagePath, cols, table.datasetName.getOrElse(""), joins.map(_.tbInfo))
  }

  private def addPath(p: MPath): Unit = {
    if (!builder.contains(p)) builder += (p -> None)
  }

  private def processPath(p: MPath, t: Option[TableCode]): Unit = {
    if (builder.contains(p)) builder -= p
    builder += (p -> Some(t))
  }

  private def unprocessed: Seq[MPath] = builder.collect({ case (p, None) => p }).toSeq

}
