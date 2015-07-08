package info.kwarc.mmt.leo.datastructures

import scala.collection.mutable
import scala.reflect.ClassTag

/**
 * Created by Mark on 7/7/2015.
 */

object OutputLog {
  var log: List[(Int,String,String)] = Nil

  def addIndent(s:String, indent:Int=1):String ={
    "\t"*indent+s.replace("\n","\t"*indent+"\n")
  }

  /** Display log at specified verbosity with specified prefix*/
  def display(prefix: String, verbosity: Int  ):Unit = {
    log.foreach(tuple =>
      if (tuple._1<=verbosity && tuple._2==prefix) {
        println("["+tuple._2+"]: \n" + addIndent(tuple._3))
      }
    )
  }

  /** Display log at specified verbosity with list of prefixes*/
  def display(pl: List[String], verbosity: Int ):Unit = {
    log.foreach(tuple =>
      if (tuple._1<=verbosity && pl.contains(tuple._2)) {
        println("["+tuple._2+"]: \n" + addIndent(tuple._3))
      }
    )
  }

  /** Display log at specified verbosity*/
  def display(verbosity: Int = 1):Unit = {
    log.foreach(tuple =>
      if (tuple._1<=verbosity) {
        println("["+tuple._2+"]: \n" + addIndent(tuple._3))
      }
    )
  }

}

trait Debugger {
  def logPrefix: String

  def log(message: String, verbosity: Int = 1): Unit ={
    OutputLog.log = OutputLog.log:::List((verbosity,logPrefix,message))
  }

  protected def setDisplay[B](s:Set[B], title:String = "Set"):String ={
    var out = title+":\n"
    for (e <- s) {out = out+addIndent(e)+"\n"}
    out
  }

  protected def QueueSetDisplay[B](q:mutable.Queue[Set[B]], title:String = "Queue",title2:String = "Set"):String ={
    var out = title+":\n"
    for (e <- q) {out = out+addIndent(setDisplay(e,title2))+"\n"}
    out
  }

  protected def addIndent(obj:Any, indent:Int=1):String ={
    obj match {
      case ProofTree(x) =>
        "\t"*indent+obj.toString.replaceFirst("\n","").replace("\n","\n"+"\t"*indent)
      case _ =>
        "\t"*indent+obj.toString.replace("\n","\n"+"\t"*indent)
    }
  }

}
