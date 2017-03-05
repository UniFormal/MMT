package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.{BuildQueue, BuildManager}
import utils._

/** mixes in helper functions for interactive shells */
trait StandardIOHelper {
  lazy val input = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))

    /** read either 'yes' or 'no' from standard input */
  def getYesNo(default: Boolean) = {
     val answer = input.readLine().toLowerCase
     if (answer.isEmpty) default
     else if (answer.startsWith("y")) true
     else if (answer.startsWith("n")) false
     else {
       println("I'll take that as a " + (if (default) "'no'" else "'yes'"))
       !default
     }
  }

  def getFile(msg: String, default: Option[File]) = {
     val defMsg = default.map(d => " (" + d + "): ").getOrElse("")
     println(msg + defMsg)
     val answer = input.readLine
     default match {
       case Some(d) => d / answer
       case None => File(answer)
     }
  }
}

/** Creates a Controller and provides a shell interface to it.
  *
  * The command syntax is given by the Action class and the parser in its companion object.
  */
class Shell extends StandardIOHelper {
  lazy val runStyle = MMTSystem.runStyle
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
       sys.exit(Shell.EXIT_CODE_OK)
    } catch {
      case e: Error =>
        controller.report(e)
        controller.cleanup
        // We do not re-throw the exception here
        // but instead simply exit with a non-zero code
        sys.exit(Shell.EXIT_CODE_FAIL_EXCEPTION)
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

  /** run a ShellExtension */
  private def deferToExtension(key: String, args: List[String]) {
     controller.extman.getOrAddExtension(classOf[ShellExtension], key) match {
       case Some(se) =>
          val doCleanup = se.run(this, args)
          if (doCleanup) {
             controller.cleanup
          }
       case None =>
          println("no shell extension found for " + key)
    }
  }

  lazy val repl : REPLExtension = {
    // TODO: throw a warning if there is more than one REPL
    // or have a mechanism to get a specific REPL
    controller.extman.get(classOf[REPLExtension]).headOption.getOrElse {
      controller.extman.addExtension(StandardREPL)
      StandardREPL
    }
  }

  /** main method without exception handling */
  private def mainRaw(a: Array[String]) {
     val deployFolder = runStyle match {
       case rs: MMTSystem.DeployRunStyle => Some(rs.deploy)
       case _ => None
     }
     val defRc = deployFolder.map(_/"mmtrc")
     // load additional configurations (default config is loaded by Controller.init)
     val cfgLocations: List[File] = defRc.toList ::: List(MMTSystem.userConfigFile)
     cfgLocations foreach loadConfig
     // execute startup arguments
     deployFolder foreach {f =>
        loadMsl(f / "startup.msl")
     }

     // guess if MMT is run for the first time and offer changing into setup
     if (a.isEmpty && defRc.map(!_.exists).getOrElse(true)) {
       println("\n\n\n\n\nIt looks like you might be running MMT for the first time.\n" +
               "Do you want me to run setup (y/n)?"
       )
       if (getYesNo(true)) {
         deferToExtension("setup", Nil)
         return
       } else {
         println("\n\n")
         defRc.foreach {rc =>
           println("If you want to avoid this prompt in the future, create an empty configuration file at " + rc + "\n" +
                   "Do you want me to create one now (y/n)?"
           )
           if (getYesNo(false)) {
             File.write(rc, "// add configuration options here\n")
           }
         }
       }
     }

     // check for "mmt :command ARGS" and delegate to ShellExtensions
     a.toList match {
       case ccom::args if ccom.startsWith(":") =>
          val com = ccom.substring(1)
          deferToExtension(com, args)
          return
       case _ =>
    }

    // parse command line arguments
    val args = ShellArguments.parse(a.toList).getOrElse {
      printHelpText("usage")
      sys.exit(Shell.EXIT_CODE_FAIL_ARGUMENT)
    }

    // display some help text
    if (args.help) {
      args.commands.foreach { s =>
        val optHelp = getHelpText(s)
        if (optHelp.isDefined) {
          println(optHelp.get)
          sys.exit(Shell.EXIT_CODE_OK)
        }
      }
      printHelpText("help")
      sys.exit(Shell.EXIT_CODE_OK)
    }

    // display some about text
    if (args.about) {
      printHelpText("about")
      sys.exit(Shell.EXIT_CODE_OK)
    }

    // configure logging
    if (args.consoleLog) {
      controller.report.addHandler(ConsoleHandler)
    }
    if (args.debugOutput) {
      controller.report.groups += "debug"
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
        // try to load the extended repl by default
        controller.extman.addExtensionO("info.kwarc.mmt.repl.ExtendedREPL", List())
        
        repl.enter(args)
        // run the repl and cleanup
        try {
          repl.run()
        } finally {
          repl.exit()
        }
    }
    input.close

    // cleanup if we want to exit
    if (args.runCleanup) {
       controller.extman.get(classOf[BuildManager]).foreach(_.waitToEnd)
       controller.cleanup
    }
  }
}

/**
  * An extension that provides REPL functionality to MMT.
  */
trait REPLExtension extends Extension {

  /** Banner of the REPL to be printed when (before even entering it) */
  protected val banner : String = MMTSystem.getResourceAsString("/help-text/shelltitle.txt")

  /* A report handler to be added to the console automatically when needed */
  protected val handler : ReportHandler = ConsoleHandler

  /** Called when entering (i.e. starting up) the REPL */
  def enter(args : ShellArguments) : Unit = {
    //print the banner
    println(banner)

    // switch on console reports for wrong user inputs
    controller.report.addHandler(ConsoleHandler)
  }

  /** Called when running the REPL */
  def run() : Unit

  /** Called up leaving the REPL to clean up */
  def exit() : Unit
}

/** The standard, bare-bones implementation of the REPL */
object StandardREPL extends REPLExtension {
  private lazy val input = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))

  def run() : Unit =  {
    var command = Option(input.readLine)
    while (command.isDefined) {
      controller.handleLine(command.get, showLog = true)
      command = Option(input.readLine)
    }
  }

  def exit() : Unit = {input.close()}
}

object Shell {
  /** exit code for when everything is OK */
  final val EXIT_CODE_OK : Int = 0
  /** exit code for when parsing arguments fails */
  final val EXIT_CODE_FAIL_ARGUMENT : Int = 1
  /** exit code for when an unexpected exception occurs */
  final val EXIT_CODE_FAIL_EXCEPTION : Int = 2
}

/** A shell, the default way to run MMT as an application */
object Run extends Shell()
