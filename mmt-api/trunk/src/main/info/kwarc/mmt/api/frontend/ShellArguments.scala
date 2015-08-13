package info.kwarc.mmt.api.frontend

import java.nio.file.{Paths, Files}

/**
 * Represents arguments parsed to MMT on the command line.
 * @param help
 */
class ShellArguments(
    val help: Boolean = false,
    val about: Boolean = false,
    
    val send:Option[Int] = None,

    val mmtfiles: List[String] = Nil,
    val scalafiles: List[String] = Nil,
    val commands: List[String] = Nil,

    val interactive: Boolean = true
){}

object ShellArguments{


  // a mapping with long_name -> short_name
  private val LongToShortArguments = Map[String, String](
    "help"    ->  "h" , // h for help
    "about"   ->  "a" , // a for about
    "shell"   ->  "i" , // i as in interactive
    "noshell" ->  "ni", // ni as in non-interactive
    "send"    ->  "r" , // r as in remote or relay
    "mbt"     ->  "m" , // m as in mbt
    "file"    ->  "f"   // f as in file
  )

  // and one which goes the other way.
  private lazy val ShortToLongArguments = LongToShortArguments.map(_.swap)

  /**
   * Tries to understand an argument from the command line.
   * @param arg
   * @return None (in case of a string argument), Some("") in case of an unknown argument or "" in case of a name
   */
  private def getCanonicalArgumentName(arg: String): Option[String] = {
    var shortName = ""

    // strip the prefixes
    if(arg.startsWith("--")){
      shortName = arg.substring(2)
    } else if(arg.startsWith("-") || arg.startsWith("/")){
      shortName = arg.substring(1)

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

  def parse(arguments: List[String]): Option[ShellArguments] = {

    // setup all the defaults
    var help = false
    var about = false
    var setHelpAbout = false

    var interactive = true
    var send:Option[Int] = None
    var setInteractive = false

    var mmtFiles: List[String] = Nil
    var scalaFiles: List[String] = Nil
    var commands: List[String] = Nil

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

          if(!setInteractive){
            interactive = false
          }
        }

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

        // the shell, noshell and send flags
        // are mutually exclusive

        // the mmt flag
        case Some("shell") => {
          if (setInteractive && (!interactive || send != None)) {
            println("Argument " + cArg + " cannot be used here: Atmost one of --shell, --noshell and --send arguments can be used. ")
            return None
          }

          setInteractive = true
          interactive = true
        }

        // the scala flag
        case Some("noshell") => {
          if (setInteractive && (interactive || send != None)) {
            println("Argument " + cArg + " cannot be used here: Atmost one of --shell, --noshell and --send arguments can be used. ")
            return None
          }

          setInteractive = true
          interactive = false
        }

        // the send flag
        case Some("send") => {
          if (setInteractive) {
            println("Argument " + cArg + " cannot be used here: Atmost one of --shell, --noshell and --send arguments can be used. ")
            return None
          }

          i = i + 1

          // must be followed
          if (i >= arguments.length) {
            println("Argument " + cArg + " cannot be used here: Missing PORT argument. ")
            return None
          }

          setInteractive = true

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

        case Some(_) => {
          println("Unknown Argument " + cArg)
          return None
        }

      }

      // and the next argument
      i = i+1
    }

    // build the shell arguments object and return it
    Some(new ShellArguments(help, about, send, mmtFiles, scalaFiles, commands, interactive))
  }

}
