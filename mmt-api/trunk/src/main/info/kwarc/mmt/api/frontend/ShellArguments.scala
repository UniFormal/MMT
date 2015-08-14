package info.kwarc.mmt.api.frontend

import java.nio.file.{Files, Paths}

/**
 * Represents arguments parsed to MMT on the command line.
 * @param help Should we print an help text
 * @param about Should we print an about text
 * @param send Should we send commands to a remote port instead of running them locally?
 * @param mmtFiles List of MMT files to load
 * @param scalaFiles List of SCALA files to load
 * @param commands List of commands to run
 * @param prompt Should we start a prompt?
 * @param runCleanup Should we run some cleanup
 */
class ShellArguments(
    val help: Boolean,
    val about: Boolean,
    
    val send:Option[Int],

    val mmtFiles: List[String],
    val scalaFiles: List[String],
    val commands: List[String],

    val prompt: Boolean,
    val runCleanup: Boolean
)

object ShellArguments{

  // a mapping with long_name -> short_name
  private val LongToShortArguments = Map[String, String](
    "help"      ->  "h", // h for help
    "about"     ->  "a", // a for about

    "shell"     ->  "i", // i as in interactive
    "keepalive" ->  "w", // w as in wait
    "noshell"   ->  "e", // e as in exit


    "send"      ->  "r", // r as in remote or relay

    "mbt"       ->  "m", // m as in mbt
    "file"      ->  "f"  // f as in file
  )

  // and one which goes the other way.
  private lazy val ShortToLongArguments = LongToShortArguments.map(_.swap)

  /**
   * Tries to understand an argument from the command line.
   * @param arg
   * @return None (in case of a string argument), Some("") in case of an unknown argument or "" in case of a name
   */
  private def getCanonicalArgumentName(arg: String): Option[String] = {
    val shortName =
    // strip the prefixes
    if(arg.startsWith("--")){
      arg.substring(2)
    } else if(arg.startsWith("-") || arg.startsWith("/")){
      arg.substring(1)
    } else {
      // or if there is no prefix, we have nothing to return.
      return None
    }

    // if it is a long name, we can return it.
    if(LongToShortArguments.contains(shortName)){
      return Some(shortName)
    }

    // otherwise try to lookup a short name.
    Some(ShortToLongArguments.getOrElse(shortName, ""))
  }

  /**
   * Parses arguments from the commands line
   * @param arguments arguments to parse
   * @return
   */
  def parse(arguments: List[String]): Option[ShellArguments] = {

    //help && about
    var help = false
    var about = false
    var setHelpAbout = false

    // termination behaviour
    var prompt = false
    var runCleanup = false
    var setTerminationBehaviour = false

    // files and commands to process
    // we do not care if we set these or not.
    var mmtFiles: List[String] = Nil
    var scalaFiles: List[String] = Nil
    var commands: List[String] = Nil

    // a port for senmd
    var send:Option[Int] = None

    // iterate through the arguments
    // the old fashioned way
    var i = 0
    var cArg : String = ""
    while ( i < arguments.length){
      // get the current argument
      cArg = arguments(i)

      // check what this argument is
      getCanonicalArgumentName(cArg) match {

        // it is not a flag of any kind
        // so just add it as a comma nd
        case None => {
          commands = arguments(i) :: commands
        }

        // <editor-fold desc="Help && About">
        // the help and about flags
        // are mutually exclusive

        // the help flag
        case Some("help") => {
          if (setHelpAbout && about) {
            println("Argument " + cArg + " cannot be used here: Atmost one of --help and --about arguments can be used. ")
            return None
          }
          setHelpAbout = true
          help = true
        }

        // the about flag
        case Some("about") => {
          if (setHelpAbout && help) {
            println("Argument " + cArg + " cannot be used here: Atmost one of --help and --about arguments can be used. ")
            return None
          }

          setHelpAbout = true
          about = true
        }

        // </editor-fold>

        // <editor-fold desc="Termination behaviour">
        // the shell, noshell and keepalive args

        // the shell flag
        case Some("shell") => {
          if (setTerminationBehaviour) {
            println("Argument " + cArg + " cannot be used here: Atmost one of --shell, --noshell and --keepalive arguments can be used. ")
            return None
          }

          setTerminationBehaviour = true
          prompt = true
          runCleanup = false
        }

        // the noshell flag
        case Some("noshell") => {
          if (setTerminationBehaviour) {
            println("Argument " + cArg + " cannot be used here: Atmost one of --shell, --noshell and --keepalive arguments can be used. ")
            return None
          }

          setTerminationBehaviour = true
          prompt = false
          runCleanup = true
        }

        case Some("keepalive") => {
          if (setTerminationBehaviour) {
            println("Argument " + cArg + " cannot be used here: Atmost one of --shell, --noshell and --keepalive arguments can be used. ")
            return None
          }

          setTerminationBehaviour = true
          prompt = false
          runCleanup = false
        }

        // </editor-fold>

        // <editor-fold desc="Send Command">
        // the send flag
        case Some("send") => {
          if (send.isDefined) {
            println("Argument " + cArg + " cannot be used here: --send can only be used once. ")
            return None
          }

          i = i + 1

          // must be followed
          if (i >= arguments.length) {
            println("Argument " + cArg + " cannot be used here: Missing PORT argument. ")
            return None
          }

          // by something that can be turned into an integer.
          send = {
            try {
              Some(arguments(i).toInt)
            } catch {
              case e: Exception => {
                println("Argument " + cArg + " cannot be used here: Expected an integer, but found " + arguments(i) + " instead. ")
                return None
              }
            }
          }
        }
        // </editor-fold>

        // <editor-fold desc="Load files">
        // the file flag
        case Some("file") => {
          i = i + 1

          // must be followed
          if (i >= arguments.length) {
            println("Argument " + cArg + " cannot be used here: Missing FILENAME argument. ")
            return None
          }

          // resolve the possible path
          val path = Paths.get(arguments(i))

          // check that it is indeed a file.
          if (!Files.isRegularFile(path)) {
            println("Argument " + cArg + " cannot be used here: "+ path.toString() + " is not a file. ")
            return None
          }

          // add it as a string.
          mmtFiles = path.toAbsolutePath().toString() :: mmtFiles
        }

        // the mbt flag
        case Some("mbt") => {
          i = i + 1

          // must be followed
          if (i >= arguments.length) {
            println("Argument " + cArg + " cannot be used here: Missing FILENAME argument. ")
            return None
          }

          // resolve the possible path
          val path = Paths.get(arguments(i))

          // check that it is indeed a file.
          if (!Files.isRegularFile(path)) {
            println("Argument " + cArg + " cannot be used here: "+ path.toString() + " is not a file. ")
            return None
          }

          // add it as a string.
          scalaFiles = path.toAbsolutePath().toString() :: scalaFiles
        }

        // </editor-fold>

        case Some(_) => {
          println("Unknown Argument " + cArg)
          return None
        }

      }

      // and the next argument
      i = i+1
    }

    // if we did not yet set the termination bahaviour we need to do it here.
    if(!setTerminationBehaviour){

      if(commands.isEmpty){
        if(mmtFiles.isEmpty && scalaFiles.isEmpty) {
          // no commands && no files => --shell
          prompt = true
          runCleanup = false
        } else {
          // no commands && some files => --keepalive
          prompt = false
          runCleanup = false
        }
      } else {
        if(mmtFiles.isEmpty && scalaFiles.isEmpty){
          // some commands && no files => --noshell
          prompt = false
          runCleanup = true
        } else {
          // some commands && files => --keepalive
          prompt = false
          runCleanup = false
        }
      }


    }


    // build the shell arguments object and return it
    Some(new ShellArguments(help, about, send, mmtFiles.reverse, scalaFiles.reverse, commands.reverse, prompt, runCleanup))
  }

}