package info.kwarc.mmt.sql.codegen

case class TableInfo(prefix: String, name: String) {

  def tablePackageName: String = prefix + name
  def dbTableName: String = s"${prefix}_${name.toUpperCase}"
  def viewName = s"${name}View"
  def dbViewName: String = s"${prefix}_${viewName.toUpperCase}"
  def packageString = s"xyz.discretezoo.web.db.$tablePackageName" // package for the table specific files

  def caseClass: String = name
  def tableObject: String = s"tb$name"
  def viewObject: String = s"view$name"
  def tableClass: String = s"${name}Table"

  def joinImport: String = s"import $packageString.$tableClass"
  def joinQuery: String = s"  val $tableObject = TableQuery[$tableClass]"

  def dbObject(name: String): String = {
    s"object $tableObject extends TableQuery(new $tableClass(_))"
  }

}
