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
import tiscaf._
import scala.collection.mutable.HashMap._
import info.kwarc.mmt.api.web._
import scala.util.parsing.json._
import scala.util._
import scala.annotation.tailrec
import scala.io._
import java.util.Calendar

class Example(controllerAttribute: Controller, theoryName: Path) {
  val name : Path = theoryName
  val controller = controllerAttribute
  val topics : List[Path] = this.updateTopics()
  val avgDifficulty : Double = this.updateDifficulty()
  val avgTime : Double = this.updateAvgTime()
  
  // The rating does not appear here because it is relevant only to a single topic. 
  // "The quick brown fox jumps over a lazy dog" is a good example of a sentence that covers all letters of the alphabet,
  // it's useless otherwise
  
  private def updateTopics() : List[Path] = {
    val examples = controller.depstore.getSubjects(ontology.HasCodomain, flexiformal.FragPath(name))
    examples.toList
  }
  
  private def updateDifficulty() : Double = {
    val allTopics = topics.map{x => {
      new ExampleTopicMap(name, x)
    }}
    
    //allTopics
    0.0
  }
  
  private def updateAvgTime() : Double = {
    0.0
  }
  
  def getRating(topic: Path) : Double = {
    0.0
  }
}