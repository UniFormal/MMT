package main.scala.info.kwarc.mmt.api.webedit
import info.kwarc.mmt.api._
import frontend._

object Main {
   val controller = new Controller
   val editingService = new EditingServicePlugin(controller)
   def main(a : Array[String]) : Unit = {
      var args = a.toList
      val commands = args.toList.mkString(" ").split(" ; ")
      try {
         commands foreach controller.handleLine 
      } catch {
         case e: Error =>
           controller.report(e)
           controller.cleanup
           throw e
         case e : Throwable =>
           controller.cleanup
           throw e
      }
   }
}
