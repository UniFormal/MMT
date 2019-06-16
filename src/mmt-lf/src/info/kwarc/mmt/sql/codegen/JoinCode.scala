package info.kwarc.mmt.sql.codegen

case class JoinCode(tbInfo: TableInfo, columnName: String) extends CodeHelper {

  def fkMethod: String = {
    val camelName = camelCase(columnName)
    val fk = s"""  def fk${tbInfo.name} = foreignKey("${tbInfo.dbTableName}_FK", $camelName, ${tbInfo.tableObject})"""
    s"""$fk(_.ID, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)"""
  }

}
