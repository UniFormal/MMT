package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import utils._

/** Creates a Controller and provides a shell interface to it.
  *
  * The command syntax is given by the Action class and the parser in its companion object.
  */
class Shell {
  lazy val controller = new Controller

  private def getHelpText(cmd: String): Option[Iterator[String]] =
    Option(getClass.getResourceAsStream("/help-text/" + cmd + ".txt")).
      map(scala.io.Source.fromInputStream(_).getLines())

  private def printHelpText(cmd: String) {
    getHelpText(cmd).foreach(_.foreach(println))
  }

  def main(a: Array[String]) {

    // parse command line arguments
    val args = ShellArguments.parse(a.toList).getOrElse {
      printHelpText("usage")
      sys.exit(1)
    }

    // display some help text
    if (args.help) {
      args.commands.foreach { s =>
        val optHelp = getHelpText(s)
        if (optHelp.isDefined) {
          optHelp.get.foreach(println)
          sys.exit(0)
        }
      }
      printHelpText("help")
      sys.exit(0)
    }

    // display some about text
    if (args.about) {
      printHelpText("about")
      sys.exit(0)
    }

    // for the remaining cases we want to execute commands.
    // so we will join them with semicolons

    val mmtCommands = if (args.mmtFiles.nonEmpty) {
      "file" :: args.mmtFiles
    } else {
      Nil: List[String]
    }
    val mbtCommands = if (args.scalaFiles.nonEmpty) {
      "mbt" :: args.scalaFiles
    } else {
      Nil: List[String]
    }

    val commands = mmtCommands ++ mbtCommands ++ args.commands

    // maybe we want to send something to the remote
    if (args.send.isDefined) {
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
      if (startup.exists) {
        controller.execFileAction(startup, None)
      }
      controller.getConfig.add(MMTConfig.parse(MMTSystem.getResourceAsString("/mmtrc")))
      // TODO val clCfg = FILE path from --cfg switch, add at the end of startuipLocations
      val cfgLocations: List[File] = List(MMTSystem.rootFolder / "mmtrc", MMTSystem.userConfigFile)
      //println("trying to run " + startup)
      cfgLocations.foreach {l =>
        if (l.exists) {
          controller.getConfig.add(MMTConfig.parse(l))
        }
      }

      //run the commands for each line.
      commands.mkString(" ").split(" ; ") foreach {l => controller.handleLine(l, showLog = false)}

      // if we want a shell, prompt and handle input
      if (args.prompt) {
        printHelpText("shelltitle")
        // create a new shell.
        val Input = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
        // switch on console reports for wrong user inputs
        controller.report.addHandler(ConsoleHandler)
        // handle commands as long as we get input.
        var command = Option(Input.readLine())
        while (command.isDefined) {
          controller.handleLine(command.get, showLog = true)
          command = Option(Input.readLine())
        }
      }

      // cleanup if we want to exit.
      if (args.runCleanup) {
        controller.cleanup
      }
    } catch {
      case e: Error =>
        controller.report(e)
        controller.cleanup
        throw e
      case e: Exception =>
        controller.cleanup
        throw e
    }
  }
}

/** A shell, the default way to run MMT as an application */
object Run extends Shell()
