package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._

/** Creates a Controller and provides a shell interface to it.
  * The command syntax is given by the Action class and the parser in its companion object.

  */
class Shell extends {
  lazy val controller = new Controller
  private val usagetext = """usage:
    mmt [--help|--about] [--send PORT | --shell | --noshell] [--file FILENAME] [--mbt FILENAME] [COMMANDS]
"""
  private val helptext = usagetext+"""
the MMT shell script

general arguments:
  -h, --help                show this help message and exit.
  -a, --about               print some information about MMT.

what to do:
  -i, --shell               execute COMMANDS and take further commands on the MMT shell. Default if no
                            arguments are provided.
  -ni, --noshell            execute COMMANDS and exit.
  -r, --send PORT COMMANDS  send COMMANDS to mmt instance listening at PORT

commands and files to process:
  -f, --file FILENAME       In addition to using COMMANDS, load mmt-style commands from FILE. May be used multiple times.
  -m, --mbt FILENAME        In addition to using COMMANDS, load scala-style commands from FILE. May be used multiple times.

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

    /*
      println("help", args.help)
      println("about", args.about)
      println("interactive", args.interactive)
      println("send", args.send)
      println("mmtfiles", args.mmtfiles)
      println("scalafiles", args.scalafiles)
      println("commands", args.commands)
      println("interactive", args.interactive)
    */

    // display some help text
    if(args.help){
      println(helptext)
      sys.exit
    }

    // display some about text
    if(args.about){
      println("See documentation in https://svn.kwarc.info/repos/MMT/doc/html/index.html")
      sys.exit
    }


    // for the remaining cases we want to execute commands.
    // so we will join them with semicolons

    val mmtCommands = (if(args.mmtfiles.length>0){"file" :: args.mmtfiles} else {Nil:List[String]})
    val sbtCommands = (if(args.scalafiles.length>0){"mbt" :: args.scalafiles} else {Nil:List[String]})

    val commands = mmtCommands ++ sbtCommands ++ args.commands

    // maybe we want to send something to the remote
    if(args.send != None){
      val uri = (URI("http", "localhost:" + args.send.get) / ":admin") ? commands.mkString(" ")
      try {
        println("sending: " + uri.toString)
        val ret = utils.xml.get(uri.toJava.toURL)
        println(ret.toString)
      } catch {
        case e: Exception =>
          println("error while connecting to remote MMT: " + e.getMessage)
      }
      sys.exit
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

      // wait for interactive commands
      if (args.interactive){
        println(shelltitle)
      }

      // create a new shell.
      val Input = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))

      // wait for commands as long as we are interactive.
      while (args.interactive) {
        val command = Input.readLine()
        if (command != null)
           controller.handleLine(command)
      }

      // cleanup when we are interactive
      if (args.interactive){
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
