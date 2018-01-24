package info.kwarc.mmt.guidedtours.utils

// Use H2Driver to connect to an H2 database
import slick.driver.H2Driver.api._
//import scala.slick.driver.SQLiteDriver.simple._
import scala.concurrent.ExecutionContext.Implicits.global

trait sqlite {
  val db = Database.forURL("jdbc:sqlite:ratings.sqlite", driver = "org.h2.Driver")

  class ExampleRatings(tag: Tag) extends Table[(Int, String, Int, Double, Double)] (tag, "ExampleRatings") {
    def id = column[Int]("ID", O.PrimaryKey)
    def topic = column[String]("TOPIC", O.NotNull)
    def time = column[Int]("TIME")
    def quality = column[Double]("QUALITY")
    def difficulty = column[Double]("DIFFICULTY")

    def * = (id, topic, time, quality, difficulty)
  }

  val exampleRatings = TableQuery[ExampleRatings]

  class ProblemRatings(tag: Tag) extends Table[(Int, String, Int, Double, Double)] (tag, "ProblemRatings") {
    def id = column[Int]("ID", O.PrimaryKey)
    def topic = column[String]("TOPIC", O.NotNull)
    def time = column[Int]("TIME")
    def quality = column[Double]("QUALITY")
    def difficulty = column[Double]("DIFFICULTY")

    def * = (id, topic, time, quality, difficulty)
  }

  val problemRatings = TableQuery[ProblemRatings]

  class UserKarma(tag: Tag) extends Table[(Int, Int, String, Double)] (tag, "UserKarma") {
    def id = column[Int]("ID", O.PrimaryKey)
    def userId = column[Int]("USER_ID")
    def topic = column[String]("TOPIC")
    def score = column[Double]("SCORE")

    def * = (id, userId, topic, score)
  }

  val userKarma = TableQuery[UserKarma]

  val setup = (
      (exampleRatings.schema ++ problemRatings.schema ++ userKarma.schema).create,
        userKarma += (1, 1, "http://mathhub.info/MiKoMH/GenCS/machines/en/VMP-call.omdoc?VMP-call", 70)
      )
}
