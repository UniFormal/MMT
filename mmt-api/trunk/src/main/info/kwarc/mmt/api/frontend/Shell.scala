package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.{BuildQueue, BuildManager}
import utils._

/** used to add a new command line application to MMT
  *
  * ShellExtensions are looked for after all configurations are loaded.
  * These must contain the corresponding [[ExtensionConf]] entry so that MMT can find a ShellExtension.
  */
abstract class ShellExtension(command: String) extends FormatBasedExtension {
   def isApplicable(s: String) = s == command

  /** executed via the shell command "mmt :command ARGS"
    *
    * @return true iff MMT should clean up and exit
    */
   def run(args: List[String]): Boolean
   /** help text for this command */
   def helpText: String
}

/** pass on arguments to an MMT server instance and terminate */
class ShellSendCommand extends ShellExtension("send") {
   def helpText = "mmt :send URL ARGS"
   def run(args: List[String]): Boolean = {
      if (args.isEmpty) {
         println(helpText)
         return true
      }
      val target = args.head
      val server = if (target.forall(_.isDigit)) {
         // port number
         URI("http", "localhost:" + args.head)
      } else {
         URI(target)
      }
      val url = (server / ":action") ? args.tail.mkString(" ")
      try {
        println("sending: " + url.toString)
        val ret = utils.xml.get(url.toURL)
        println(ret.nonEmptyChildren.flatMap(d => d.nonEmptyChildren.map(_.toString)).mkString("\n"))
      } catch {
        case e: Exception =>
          println("error while connecting to remote MMT: " + e.getMessage)
      }
      true
   }
}


/** Creates a Controller and provides a shell interface to it.
  *
  * The command syntax is given by the Action class and the parser in its companion object.
  */
class Shell {
  lazy val controller = new Controller

  private def getHelpText(cmd: String): Option[String] = {
     val s = MMTSystem.getResourceAsString("/help-text/" + cmd + ".txt")
     Option(s)
  }

  private def printHelpText(cmd: String) {
    getHelpText(cmd) foreach println
  }

  /** creates controller, loads configurations/startup files, processes arguments, possibly drops into shell or terminates */
  def main(a: Array[String]) {
    try {
       mainRaw(a)
    } catch {
      case e: Error =>
        controller.report(e)
        controller.cleanup
        throw(e)
    }
  }

  private def loadConfig(cfg: File) {
     if (cfg.exists) {
        controller.loadConfig(MMTConfig.parse(cfg), false)
     }
  }
  private def loadMsl(msl: File) {
     if (msl.exists) {
        controller.execFileAction(msl, None)
     }
  }

  /** main method without exception handling */
  private def mainRaw(a: Array[String]) {
     // load additional configurations (default config is loaded by Controller.init)
     val mainFolder = MMTSystem.runStyle.parentFolder.toList
     val cfgLocations: List[File] = mainFolder.map(_ / "mmtrc") ::: List(MMTSystem.userConfigFile)
     cfgLocations foreach loadConfig
     // execute startup arguments
     mainFolder foreach {f =>
        loadMsl(f / "startup.msl")
     }

     // check for "mmt :command ARGS" and delegate to ShellExtensions
     a.toList match {
       case ccom::args if ccom.startsWith(":") =>
          val com = ccom.substring(1)
          controller.extman.getOrAddExtension(classOf[ShellExtension], com) match {
             case Some(se) =>
                val cleanup = se.run(args)
                if (cleanup) {
                   controller.cleanup
                }
             case None =>
                println("no shell extension found for " + com)
          }
          return
       case _ =>
    }

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
          println(optHelp.get)
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

    // load additional config files as given by arguments
    args.cfgFiles.map(File(_)) foreach loadConfig


    if (args.useQueue) {
       controller.extman.addExtension(new BuildQueue)
    }

    // run -file and -mbt commands
    val mmtCommands = args.mmtFiles map {s => ExecFile(File(s), None)}
    val mbtCommands = args.scalaFiles map {s => MBT(File(s))}
    (mmtCommands ::: mbtCommands) foreach {a => controller.handle(a)}
    // run the remaining commands
    args.commands.mkString(" ").split(" ; ") foreach {l => controller.handleLine(l, showLog = false)}

    // if we want a shell, prompt and handle input
    if (args.prompt) {
        printHelpText("shelltitle")
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

    // cleanup if we want to exit
    if (args.runCleanup) {
       controller.extman.get(classOf[BuildManager]).foreach(_.waitToEnd)
       controller.cleanup
    }
  }
}

/** A shell, the default way to run MMT as an application */
object Run extends Shell()
