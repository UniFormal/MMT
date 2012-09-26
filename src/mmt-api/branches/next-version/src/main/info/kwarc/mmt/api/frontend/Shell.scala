package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.utils._

/** Creates a Controller and provides a shell interface to it.
 * The command syntax is given by the Action class and the parser in its companion object.

 */
class Shell() extends {
   val controller = new Controller

   def main(args : Array[String]) : Unit = {
      val command = args.mkString("", " ", "")
      try {
         controller.handleLine(command)
         val Input = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
         while (true) {
            val command = Input.readLine()
            controller.handleLine(command)
         }
      } catch {
         case e: Error =>
           controller.report(e)
           controller.cleanup
           throw e
         case e =>
           controller.cleanup
           throw e
      }
   }
}

/** A shell, the default way to run MMT as an application */
object Run extends Shell()

object RunWeb {
  val controller = new Controller

  def main(args : Array[String]) : Unit = {
    val command = args.mkString("", " ", "")
    try {
      controller.handleLine(command)
    } catch {
      case e => controller.cleanup; throw e
    }
  }
}