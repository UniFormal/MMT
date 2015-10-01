package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._

/** Creates a Controller and provides a shell interface to it.
  * The command syntax is given by the Action class and the parser in its companion object.

  */
class Shell {
  lazy val controller = new Controller
  private val usagetext = """usage:
    mmt [--help|--about] [--shell|--keepalive|--noshell] [--file FILENAME] [--mbt FILENAME] [--send PORT] [COMMANDS]
"""
  private val helptext = usagetext+"""
the MMT shell script

general arguments:
  -h, --help                show this help message and exit.
  -a, --about               print some information about MMT.

commands and files to process:
  COMMANDS                  Semicolon (;) separated commands to be interpreted by MMT. Type "help" inside the shell to
                            a list of commands or look at the documentation for a list of available commands.
  -r, --send PORT           instead of executing COMMANDS in a new MMT instance, send them to another MMT Instance
                            listening at PORT and exit immediately (even if a different termination behaviour is
                            specified. )
  -f, --file FILENAME       In addition to running COMMANDS, load mmt-style commands from FILE. May be used multiple
                            times.
  -m, --mbt FILENAME        In addition to running COMMANDS, load scala-style commands from FILE. May be used multiple
                            times.

termination behaviour:

The default exit behaviour is to terminate immediately after COMMANDS have been executed. If no arguments are given to
MMT, it will start an interactive shell for the user to enter commands. The default behaviour of MMT can be overwritten
with the following arguments.

  -i, --shell               execute COMMANDS and take further commands on the MMT shell. Default if no arguments are
                            provided.
  -w, --keepalive           execute COMMANDS and terminate only after all threads have finished.
  -e, --noshell             same as --keepalive (for backwards compatibility)

note: any arguments listed here can be given in the form -argument, --argument or /argument syntax.
"""

  private val shelltitle = """
This is the MMT shell.
See https://svn.kwarc.info/repos/MMT/doc/api/index.html#info.kwarc.mmt.api.frontend.Action for the available commands.

                           """

  def main(a: Array[String]): Unit = {

    // parse command line arguments
    val args = ShellArguments.parse(a.toList).getOrElse{
        println(usagetext)
        sys.exit(1)
      }

    // FOR DEBUG PURPOSES
    /*
      println("help", args.help)
      println("about", args.about)
      println("interactive", args.prompt)
      println("cleanup", args.runCleanup)
      println("send", args.send)
      println("mmtfiles", args.mmtFiles)
      println("scalafiles", args.scalaFiles)
      println("commands", args.commands)
      sys.exit
    */

    // display some help text
    if(args.help){
      args.commands.foreach { s =>
        val stream = Option(getClass.getResourceAsStream("/help-text/" + s + ".txt"))
        if (stream.isDefined)
           {
             scala.io.Source.fromInputStream(stream.get).getLines.foreach(println)
             sys.exit(0)
           }
       }
      println(helptext)
      sys.exit(0)
    }

    // display some about text
    if(args.about){
      println("See documentation in https://svn.kwarc.info/repos/MMT/doc/html/index.html")
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
        println(shelltitle)
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
