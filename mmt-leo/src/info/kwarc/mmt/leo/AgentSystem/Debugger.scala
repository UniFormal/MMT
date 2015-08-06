package info.kwarc.mmt.leo.AgentSystem

import info.kwarc.mmt.api.frontend.{Controller, Report}
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.AndOrTree
import scala.collection.mutable

/** This trait encapsulates debugging ability and allows classes
  * to print to a progress log
  */
trait Debugger {
  def logPrefix: String

  /** Adds a message to the progress log*/
  def log(message: String, verbosity: Int = 1): Unit ={
    OutputLog.log = OutputLog.log:::List((verbosity,logPrefix,message))
  }

  /** displays a list with nice indentation*/
  protected def setDisplay[B](s:Set[B], title:String = "Set"):String ={
    var out = title+":\n"
    for (e <- s) {out = out+addIndent(e)+"\n"}
    out
  }

  /** Displays a queue of lists with nice indentation*/
  protected def QueueSetDisplay[B](q:mutable.Queue[Set[B]], title:String = "Queue",title2:String = "Set"):String ={
    var out = title+":\n"
    for (e <- q) {out = out+addIndent(setDisplay(e,title2))+"\n"}
    out
  }

  /** Special indentation function which handles the andOrTree case*/
  protected def addIndent(obj:Any, indent:Int=1):String ={
    obj match {
      case s:AndOrTree =>"\t"*indent+obj.toString.replaceFirst("\n","").replace("\n","\n"+"\t"*indent)
      case _ =>
        "\t"*indent+obj.toString.replace("\n","\n"+"\t"*indent)
    }
  }

}


/**
 * Created by Mark on 7/7/2015.
 *
 * This represents the logging and debugging objects that almost every class extends.
 * this ensures that all objects have logging capabilities
 *
 */

object Display {
  def report(implicit controller:Controller) = controller.report

  /** displays a list with nice indentation*/
  def setDisplay[B](s:Set[B], title:String = "Set"):String ={
    var out = title+":\n"
    for (e <- s) {out = out+addIndent(e)+"\n"}
    out
  }

  /** displays a list with nice indentation*/
  def listDisplay[B](s:List[B], title:String = "Set"):String ={
    var out = title+":\n"
    for (e <- s) {out = out+addIndent(e)+"\n"}
    out
  }

  /** Displays a queue of lists with nice indentation*/
  def QueueSetDisplay[B](q:mutable.Queue[Set[B]], title:String = "Queue",title2:String = "Set"):String ={
    var out = title+":\n"
    for (e <- q) {out = out+addIndent(setDisplay(e,title2))+"\n"}
    out
  }

  /** Special indentation function which handles the andOrTree case*/
  def addIndent(obj:Any, indent:Int=1):String ={
    obj match {
      case s:AndOrTree =>"\t"*indent+obj.toString.replaceFirst("\n","").replace("\n","\n"+"\t"*indent)
      case _ =>
        "\t"*indent+obj.toString.replace("\n","\n"+"\t"*indent)
    }
  }

}

object OutputLog {
  var log: List[(Int,String,String)] = Nil

  /** Adds indents to strings with new lines in them*/
  def addIndent(s:String, indent:Int=1):String ={
    "\t"*indent+s.replace("\n","\t"*indent+"\n")
  }

  /** prints a tuple of (verbosity,logPrefix,Message)*/
  def printTuple(tuple:(Int,String,String),verbosity:Int=10) = {
    if (tuple._1<=verbosity) {println("["+tuple._2+"]: \n" + addIndent(tuple._3))}
  }

  /** Display log at specified verbosity with specified prefix*/
  def display(prefix: String, verbosity: Int  ):Unit = {
    log.foreach(tuple =>
      if (tuple._2==prefix) {
        printTuple(tuple,verbosity)}
    )
  }

  /** Display log at specified verbosity with list of prefixes*/
  def display(pl: List[String], verbosity: Int ):Unit = {
    log.foreach(tuple => {
      if (pl.contains(tuple._2)) {
        printTuple(tuple, verbosity)
      }
    })
  }

  /** Display log at specified verbosity*/
  def display(verbosity: Int = 1):Unit = {
    log.foreach(tuple =>printTuple(tuple,verbosity) )
  }

}
