package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import utils._
import java.nio.file.{Files, Paths}

/**
  * Represents arguments passed to MMT on the command line.
  *
  * @param help       Should we print a help text
  * @param about      Should we print an about text
  * @param mmtFiles   List of MMT files to load
  * @param scalaFiles List of Scala files to load
  * @param cfgFiles   List of config files to load
  * @param commands   List of commands to run
  * @param shell      interactive mode requested (show shell, terminate manually)
  * @param noshell    batch mode unrequested (no shell, terminate immediately)
  * @param keepalive  server mode requested (after shell (if at all) terminate only when processes finished)
  */
case class ShellArguments(
   help: Boolean,
   about: Boolean,
   mmtFiles: List[String],
   scalaFiles: List[String],
   cfgFiles: List[String],
   commands: List[String],
   shell: Boolean,
   noshell: Boolean,
   keepalive: Boolean,
   useQueue: Boolean
 ) {
   /** decides whether to run in interactive/server or batch/server mode
    *  default behavior: interactive if nothing else is happening
    */
   def prompt = if (shell) true else if (noshell) false else {
      scalaFiles.isEmpty && mmtFiles.isEmpty && commands.isEmpty
   }
   /** decides whether to run in non-server or server mode
     * default behavior: batch
     */
   def runCleanup = ! keepalive && ! prompt
}

import AnaArgs._

object ShellArguments {
  val toplevelArgs: OptionDescrs = List(
    OptionDescr("help", "h", NoArg, "command line help"),
    OptionDescr("about", "a", NoArg, "about the program"),
    OptionDescr("shell", "i", NoArg, "force interactive mode (start shell)"),
    OptionDescr("noshell", "", NoArg, "force batch mode (execute command line arguments and terminate)"),
    OptionDescr("keepalive", "w", NoArg, "force server mode (wait for secondary processes to finish)"),
    OptionDescr("mbt", "", StringListArg, "mbt input file "),
    OptionDescr("file", "", StringListArg, "msl input file"),
    OptionDescr("cfg", "", StringListArg, "config input file"),
    OptionDescr("noqueue", "", NoArg, "do not start a build queue")
  )

  def parse(arguments: List[String]): Option[ShellArguments] = {
    val (m, cs) = AnaArgs(toplevelArgs, arguments)
    val sa = ShellArguments(
      help = m.get("help").isDefined,
      about = m.get("about").isDefined,
      mmtFiles = getStringList(m, "file"),
      scalaFiles = getStringList(m, "mbt"),
      cfgFiles = getStringList(m, "cfg"),
      commands = cs,
      shell = m.get("shell").isDefined,
      noshell = m.get("noshell").isDefined,
      keepalive = m.get("keepalive").isDefined,
      useQueue = m.get("noqueue").isEmpty
    )
    val fs = sa.mmtFiles ++ sa.scalaFiles ++ sa.cfgFiles
    if (sa.help && sa.about) {
      println("atmost one of --help and --about arguments can be used.")
      None
    } else if (sa.shell && sa.noshell) {
      println("At most one of --shell and --noshell arguments can be used.")
      None
    } else {
      var fail = false
      fs.foreach { f =>
        val path = Paths.get(f)
        if (!Files.isRegularFile(path)) {
          println("Argument " + f + " cannot be used: " + path.toString + " is not a file.")
          fail = true
        }
      }
      if (fail || sa.help) {
        usageMessage(toplevelArgs).foreach(println)
      }
      if (fail) None else Some(sa)
    }
  }
}
