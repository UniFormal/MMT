package info.kwarc.mmt.leo.datastructures

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

}
