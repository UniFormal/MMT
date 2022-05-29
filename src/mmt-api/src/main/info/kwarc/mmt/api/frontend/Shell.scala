package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import actions._
import info.kwarc.mmt.api.archives.{BuildManager, BuildQueue}
import utils._

import scala.util.Try

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
       case Some(d) => d.resolve(answer)//d / answer
       case None => File(answer)
     }
  }

  def getString(msg: String, default: Option[String]) = {
    val defMsg = default.map(d => s" ($d)").getOrElse("")
    println(msg + defMsg)
    val answer = Option(input.readLine).getOrElse("")
    if (answer.isEmpty) default.getOrElse("") else answer
  }
}

/** Creates a Controller and provides a shell interface to it.
  *
  * The command syntax is given by the Action class and the parser in its companion object.
  */
class Shell extends StandardIOHelper {
  lazy val runStyle = MMTSystem.runStyle
  lazy val controller = new Controller

  /** if set to true, disables the first run / setup routine during test cases */
  var disableFirstRun = false

  /** creates controller, loads configurations/startup files, processes arguments, possibly drops into shell or terminates */
  def main(a: Array[String]) {
    try {
       mainRaw(a)
    } catch {
      case e: Error =>
        controller.report(e)
        controller.cleanup
        // We do not re-throw the exception here but instead simply exit with a non-zero code
        // Make sure ShellArguments is configured in such a way that we log to the console by default; otherwise, this would suppress errors
        exitFail
    }
  }

  protected def exitFail = sys.exit(Level.Fatal.toInt)

  private def loadConfig(cfg: File) {
     if (cfg.exists) {
        controller.loadConfig(MMTConfig.parse(cfg), false)
     }
  }
  private def loadMsl(msl: File) {
     if (msl.exists) {
        controller.runMSLFile(msl, None, true, None)
     }
  }

  /** run a ShellExtension */
  private def deferToExtension(key: String, args: List[String]) {
     controller.extman.getOrAddExtension(classOf[ShellExtension], key) match {
       case Some(se) =>
          controller.report.addHandler(ConsoleHandler)
          controller.report.groups += se.logPrefix
          val result = se.run(this, args)
          result.foreach {l =>
            controller.cleanup
            if (l > Level.Warning)
              sys.exit(l.toInt)
          }
       case None =>
          println("no shell extension found for " + key)
          exitFail
    }
  }

  lazy val repl: REPLExtension = {
    // try to load the extended repl by default
    controller.extman.addExtensionO("info.kwarc.mmt.repl.ExtendedREPL", Nil)
    controller.extman.get(classOf[REPLExtension]).headOption.getOrElse {
      val re = new StandardREPL
      controller.extman.addExtension(re)
      re
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
     if (a.isEmpty && defRc.map(!_.exists).getOrElse(true) && !disableFirstRun) {
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
      controller.handle(HelpAction("usage_short"))
      exitFail
    }

    // configure logging
    if (args.consoleLog) {
      controller.report.addHandler(ConsoleHandler)
    }
    if (args.debugOutput) {
      controller.report.groups += "debug"
    }

    // display some help text
    if (args.help) {
      val commands = args.commands
      if(commands.isEmpty)
        controller.handle(HelpAction("usage"))
      else
        commands.foreach(t => controller.handle(HelpAction(t)))
      controller.report.flush
      return
    }

    // display some about text
    if (args.about) {
      println(MMTSystem.version)
      return
    }

    // load additional config files as given by arguments
    args.cfgFiles.map(File(_)) foreach loadConfig

    if (args.useQueue) {
       controller.extman.addExtension(new BuildQueue)
    }

    // run -file commands
    val mmtCommands = args.mmtFiles map {s => ExecFile(File(s), None)}
    mmtCommands foreach {a => controller.handle(a)}
    // run the remaining commands
    args.commands.mkString(" ").split(" ; ") foreach {l => controller.handleLine(l, showLog = true)}

    // if we want a shell, prompt and handle input
    if (args.prompt) {
        repl.enter(args)
        // run the repl and cleanup
        try {
          repl.run
        } finally {
          repl.exit
        }
    }
    input.close

    // cleanup if we want to exit
    if (args.runCleanup) {
       controller.extman.extensions.foreach(_.waitUntilRemainingTasksFinished)
       controller.cleanup
    }
  }
}

/**
  * An extension that provides REPL functionality to MMT.
  */
trait REPLExtension extends Extension {
  /** Banner of the REPL to be printed when (before even entering it) */
  protected val banner : String = MMTSystem.getResourceAsString("/help-text/help.txt")
  /* A report handler to be added to the console automatically when needed */
  protected val handler : ReportHandler = ConsoleHandler
  /** Called when entering (i.e. starting up) the REPL */
  def enter(args : ShellArguments) {
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
class StandardREPL extends REPLExtension {
  private lazy val input = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
  def run {
    var command = Option(input.readLine)
    while (command.isDefined) {
      controller.tryHandleLine(command.get) match {
        case a: ActionResultError => log(a.error)
        case _ =>
      }
      command = Option(input.readLine)
    }
  }
  def exit {input.close()}
}

/** A shell, the default way to run MMT as an application */
object Run extends Shell()
