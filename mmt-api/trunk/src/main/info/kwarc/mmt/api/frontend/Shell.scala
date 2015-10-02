package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._

/** Creates a Controller and provides a shell interface to it.
  * The command syntax is given by the Action class and the parser in its companion object.

  */
class Shell {
  lazy val controller = new Controller

  private def getHelpText(cmd: String): Option[Iterator[String]] =
    (Option(getClass.getResourceAsStream("/help-text/" + cmd + ".txt"))).
      map(scala.io.Source.fromInputStream(_).getLines())

  private def printHelpText(cmd: String): Unit =
    getHelpText(cmd).foreach(_.foreach(println))

  def main(a: Array[String]): Unit = {

    // parse command line arguments
    val args = ShellArguments.parse(a.toList).getOrElse{
        printHelpText("usage")
        sys.exit(1)
      }

    // display some help text
    if(args.help){
      args.commands.foreach { s =>
        val optHelp = getHelpText(s)
        if (optHelp.isDefined)
           {
             optHelp.get.foreach(println)
             sys.exit(0)
           }
       }
      printHelpText("help")
      sys.exit(0)
    }

    // display some about text
    if(args.about){
      printHelpText("about")
      sys.exit(0)
    }

    // for the remaining cases we want to execute commands.
    // so we will join them with semicolons

    val mmtCommands = if(args.mmtFiles.nonEmpty){"file" :: args.mmtFiles} else {Nil:List[String]}
    val sbtCommands = if(args.scalaFiles.nonEmpty){"mbt" :: args.scalaFiles} else {Nil:List[String]}

    val commands = mmtCommands ++ sbtCommands ++ args.commands

    // maybe we want to send something to the remote
    if(args.send.isDefined){
      val uri = (URI("http", "localhost:" + args.send.get) / ":admin") ? commands.mkString(" ")
      try {
        println("sending: " + uri.toString)
        val ret = utils.xml.get(uri.toJava.toURL)
        println(ret.toString())
      } catch {
        case e: Exception =>
          println("error while connecting to remote MMT: " + e.getMessage)
      }
      sys.exit(0)
    }

    try {
      // execute startup arguments
      val startup = MMTSystem.rootFolder / "startup.msl"

      //println("trying to run " + startup)
      if (startup.exists) {
        controller.handle(ExecFile(startup, None))
      }

      //run the commands for each line.
      commands.mkString(" ").split(" ; ") foreach controller.handleLine

      // if we want a prompt, use a prompt
      if (args.prompt){
        printHelpText("shelltitle")
      }

      // create a new shell.
      val Input = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))

      // wait for commands as long as we have a prompt.
      while (args.prompt) {
        val command = Input.readLine()
        if (command != null)
           controller.handleLine(command)
      }

      // cleanup if we want to exit.
      if (args.runCleanup){
        controller.cleanup()
      }
    } catch {
      case e: Error =>
        controller.report(e)
        controller.cleanup()
        throw e
      case e: Exception =>
        controller.cleanup()
        throw e
    }
  }
}

/** A shell, the default way to run MMT as an application */
object Run extends Shell()
