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

class UserKarmaTemp(uniqueId: String) {
  val id = uniqueId
  val karma = new HashMap[Path, Double]() // The doubles are numbers between 0 and 100

  def getKarma(topicName : Path) : Double = {
    if(karma.contains(topicName)) {
      karma(topicName)
    }
    else {
      0.0
    }
  }
}
