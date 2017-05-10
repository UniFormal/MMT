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
import scala.io.Source

class Tutorial(controllerArgument: Controller, endTopicArgument: Path, userIdArgument: Long) extends utils.sqlite  {
  val endTopic = endTopicArgument
  val userId = userIdArgument
  val controller = controllerArgument
  val threshold = 95.0
  val topics = getTutorialTopics()
  val examples = getExamples()
  val karma = getKarma()
  
  private def getTutorialTopics() : List[Path] = {
    val all = getAllChildren(endTopic)
    sort(all) :+ endTopic
  }
  
  def getContent(length: Int) : String = {
    println("Here")
    val start = topics.takeRight(length)
    val ex = examples.takeRight(length)
    
     
    //println(start)
    //val tmp = utils.Utilities.parseGraph("/home/filipbitola/Downloads/parsed_graph.txt")
    val all = start.map { x => {
      try{
        
        val sb = new presentation.StringBuilder()
        
        val presenter = controller.extman.getPresenter("planetary").getOrElse{
          println("defaulting to default presenter")
          controller.presenter
        }
        
        presenter.apply(controller.get(x), false)(sb)
        sb.get
      }
      catch
      {
        case e : Exception => ""
      }
    }}
    
    println("Before examples")
    
    val exa = examples.map { x => {
      try{
        val sb = new presentation.StringBuilder()
        val presenter = controller.extman.getPresenter("planetary").getOrElse{
          println("defaulting to default presenter")
          controller.presenter
        }
        x.map { y => presenter.apply(controller.get(y.name), false)(sb) }
        sb.get
      }
      catch
      {
        case e : Exception => ""
      }
    }}
    
    println("Here3")
    val head = "<html><head><meta charset=\"UTF-8\"><script src=\"http://ajax.aspnetcdn.com/ajax/jQuery/jquery-1.11.2.min.js\"></script></head><body>"
    val rest = intersperse(all, exa).mkString("\n")
    val end = "</body></html>"
    //all.mkString
    
    head + rest + end
  }
  
  private def intersperse[A](a : List[A], b : List[A]): List[A] = a match {
    case first :: rest => first :: intersperse(b, rest)
    case _             => b
  }
   
   private def getKarma() : Map[Path, Double] = {
    Map(Path.parseM("http://mathhub.info/MiKoMH/GenCS/machines/en/VMP-call.omdoc?VMP-call", NamespaceMap.empty) -> 70.0)
    
  }
  
  private def getExamples() : List[List[Example]] = {
    println("I was here")
    topics.map { x => {
        val examples = findExamples(x).map(y => new Example(controller, y))
        println("Then I was not")
        if(karma(x) < 45 || karma(x) >= 95) {
          println("Then I was not 1")
          val sortedEx = examples.sortBy( ex => (ex.avgDifficulty, ex.getRating(x)))
          sortedEx.drop((sortedEx.length.toDouble * karma(x) / 100).toInt).take(3)
        }
        else {
          println("Then I was not 2")
          val sortedEx = examples.sortBy(ex => (ex.getRating(x), ex.avgDifficulty))
          sortedEx.drop((sortedEx.length.toDouble * karma(x) / 100).toInt).take(3)
        }
      }    
    }
  }
  
  private def getImmediateChildren(path: Path) : List[Path] = {
    controller.depstore.queryList(path, ToObject(Includes))
  }
  
  private def getAllChildren(name: Path) : List[Path] = {
    //val path = Path.parseM(name, mmt.mmtbase)
    //controller.depstore.queryList(path, Transitive(ToObject(Includes)))
    val children = getImmediateChildren(name)
    children ++ children.flatMap(x => getAllChildren(x))
  }
  
  private def findExamples(path : Path) : List[Path] = {
    val examples = controller.depstore.getSubjects(ontology.HasDomain, flexiformal.FragPath(path))
    val ex2 = controller.depstore.getSubjects(ontology.isExemplifiedBy, flexiformal.FragPath(path))
    examples.toList ++ ex2.toList
  }
  
  private def sort(all : List[Path]) : List[Path] = {
    @tailrec
    def sortWithResult(list1: List[Path], acc:List[Path]) : List[Path] = {
      list1 match{
        case Nil => acc
        case head :: tail => 
          val children = getImmediateChildren(head)
          val res = check(children, tail)
          if(res == null)
          {
            sortWithResult(tail, acc :+ head)
          }
          else
          {
            val arr = res :: head :: tail
            sortWithResult(arr.distinct, acc)
          }
        case _ => acc
      }
    }
    sortWithResult(all, Nil)
  }
  
   private def check(list1: List[Path], list2: List[Path]) : Path = {
    list1 match{
      case Nil => null
      case head :: tail => 
        if(list2.contains(head)) 
        {
          head
        }
        else
        {
          check(tail, list2)
        }
    }
  }
}