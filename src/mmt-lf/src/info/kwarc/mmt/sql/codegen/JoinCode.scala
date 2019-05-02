package info.kwarc.mmt.sql.codegen

case class JoinCode(tbInfo: TableInfo, columnName: String) {

  def fkMethod: String = {
    val camelName = ColumnCode.camelCase(columnName)
    val fk = s"""  def fk${tbInfo.name} = foreignKey("${tbInfo.dbTableName}_FK", $camelName, ${tbInfo.tableObject})"""
    s"""$fk(_.ID, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)"""
  }

}
