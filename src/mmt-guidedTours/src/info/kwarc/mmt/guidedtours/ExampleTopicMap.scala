package info.kwarc.mmt.guidedtours
import java.lang.String
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.parser._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.modules.DeclaredTheory
import objects._
import libraries._
import scala.concurrent._
import scala.collection.mutable.HashMap._
import info.kwarc.mmt.api.web._
import scala.util.parsing.json._
import scala.util._
import scala.annotation.tailrec
import scala.io._
import java.util.Calendar
import scala.collection.mutable._

class ExampleTopicMap(exampleName: Path, topicName: Path) {
  val example = exampleName
  val topic = topicName
  val difficulties = this.updateDifficulties()
  val ratings = this.updateRatings()
  val times = this.updateTimes()
  
  private def updateDifficulties() : HashMap[UserKarmaTemp, List[(Double, Double)]] = { // Read from DB
    new HashMap[UserKarmaTemp, List[(Double, Double)]]()
  }
  
  private def updateRatings() : HashMap[UserKarmaTemp,  List[(Double, Int)]] = { // Read from DB
    new HashMap[UserKarmaTemp,  List[(Double, Int)]]()
  }
  
  private def updateTimes() : HashMap[UserKarmaTemp,  List[(Double, Int)]] = { // Read from DB
    new HashMap[UserKarmaTemp,  List[(Double, Int)]]()
  }
  
  def newRating(fromUser : UserKarmaTemp, nRating : Int) {
    if(!ratings.contains(fromUser)) {
      val karma = fromUser.getKarma(this.topic)
      val ratingWeight = nRating * karma + 1/2 * (karma - 1)
      ratings(fromUser) = List((ratingWeight, nRating))
      return
    }
    val karma = fromUser.getKarma(this.topic)
    val ratingWeight = nRating * karma + 1/2 * (karma - 1)
    val current = ratings(fromUser)
    ratings(fromUser) = (ratingWeight, nRating) :: current
    this.save()
  }
  
  def newDifficulty(fromUser : UserKarmaTemp, newDifficulty : Double) {
    if(!difficulties.contains(fromUser)) {
      difficulties(fromUser) = List((fromUser.getKarma(this.topic), newDifficulty))
      return
    }
    
    val current = difficulties(fromUser)
    difficulties(fromUser) = (fromUser.getKarma(this.topic), newDifficulty) :: current
    this.save()
  }
  
  def newTime(fromUser : UserKarmaTemp, newTime :  Int) {
    if(!times.contains(fromUser)) {
      times(fromUser) = List((fromUser.getKarma(this.topic), newTime))
      return
    }
    
    val current = times(fromUser)
    times(fromUser) = (fromUser.getKarma(this.topic), newTime) :: current
    this.save()
  }
  
  def save() { // Write to DB
    
  }
}