package info.kwarc.mmt.sql.codegen

case class DatabaseCode(name: String, tables: Seq[TableCode], jdbc: String, user: String, pass: String) {

  // TODO Webserver

  def fileDbCode: String = {
    val importTablePackages = tables
      .map(t => s"import ${t.packageString}.{${t.plainQueryObject}, ${t.tableClass}").mkString(",\n")
    val tableObjects = tables
      .map(t => s"object tb${t.tableName} extends TableQuery(new ${t.tableClass}(_))").mkString(",\n")
    val getQueryMatches = tables
      .map(t =>
        s"""case ("tb${t.tableName}", true) => ${t.plainQueryObject}.get(rp)
           |case ("tb${t.tableName}", false) => tb${t.tableName}.dynamicQueryResults(rp).result
         """.stripMargin).mkString("\n")
    val countQueryMatches = tables
      .map(t =>
        s"""case ("tb${t.tableName}", true) => ${t.plainQueryObject}.count(rp)
           |case ("tb${t.tableName}", false) => tb${t.tableName}.dynamicQueryCount(qp).length.result
         """.stripMargin).mkString("\n")
    s"""package xyz.discretezoo.web.db
       |import akka.actor.ActorSystem
       |import akka.stream.ActorMaterializer
       |import scala.concurrent.{ExecutionContext, Future}
       |import slick.lifted.TableQuery
       |import xyz.discretezoo.web.ZooJsonAPI.{Count, ResultsParameters, SearchParameters}
       |import xyz.discretezoo.web.{ResultParam, SearchParam, ZooObject, ZooPostgresProfile}
       |import xyz.discretezoo.web.ZooPostgresProfile.api._
       |$importTablePackages
       |
       |object ZooDb {
       |
       |implicit val system: ActorSystem = ActorSystem("ZooActors")
       |implicit val materializer: ActorMaterializer = ActorMaterializer()
       |implicit val executionContext: ExecutionContext = system.dispatcher
       |
       |val db: ZooPostgresProfile.backend.DatabaseDef = Database.forURL(
       |scala.util.Properties.envOrElse("ZOODB_JDBC", "$jdbc"),
       |scala.util.Properties.envOrElse("ZOODB_USER", "$user"),
       |scala.util.Properties.envOrElse("ZOODB_PASS", "$pass"),
       |null,
       |"org.postgresql.Driver")
       |
       |$tableObjects
       |
       |def count(sp: SearchParameters, plain: Boolean = true): Future[Count] = {
       |  val qp = SearchParam.get(sp.collections, sp.filters)
       |  val q: DBIO[Int] = countDBIO(sp.objects, qp, plain)
       |  db.run(q).map(Count)
       |}
       |
       |def get(rp: ResultsParameters, plain: Boolean = true): Future[(Int, Seq[ZooObject])] = {
       |  val qp = SearchParam.get(rp.parameters.collections, rp.parameters.filters)
       |  val count: DBIO[Int] = countDBIO(rp.parameters.objects, qp, plain)
       |  val get: DBIO[(Int, Seq[ZooObject])] = count.flatMap(c => {
       |    val pages = (c / rp.pageSize).ceil.toInt
       |    val actualPage = if (rp.page >= 1 && rp.page <= pages) rp.page else 1
       |    val parameters = ResultParam.get(actualPage, rp.pageSize, qp, rp.orderBy)
       |    val query: DBIO[Seq[ZooObject]] = getDBIO(rp.parameters.objects, parameters, plain)
       |    query.map(seq => (pages, seq))
       |  })
       |  db.run(get)
       |}
       |
       |private def getDBIO(objects: String, rp: ResultParam, plain: Boolean = true): DBIO[Seq[ZooObject]] = {
       |(objects, plain) match {
       |$getQueryMatches
       |}
       |}
       |
       |private def countDBIO(objects: String, qp: SearchParam, plain: Boolean = true): DBIO[Int] = {
       |(objects, plain) match {
       |$countQueryMatches
       |}
       |}
       |
       |}
     """.stripMargin
  }

}
