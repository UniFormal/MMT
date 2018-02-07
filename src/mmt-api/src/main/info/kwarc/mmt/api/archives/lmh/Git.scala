package info.kwarc.mmt.api.archives.lmh

import info.kwarc.mmt.api.utils.{File, ShellCommand}

/** builds git commands */
abstract class Git {
  /** the path to git, defaults to "git" */
  val gitPath = "git"
  /**
    * @param args the git commands (excluding "git")
    */
  def toArgs(args: String*): List[String]

  /**
    * runs git and returns a pair of (stdout, success)
    * @param wd Working directory to run command in
    * @param args Arguments to pass to git
    * @return
    */
  def apply(wd: File, args: String*) : ShellCommand = {
    val command = toArgs(args:_*)
    // running shell commands is always brittle; better always print what we're trying to do 
    println("running git command: " + command.mkString(" "))
    println("in directory: " + wd.toString)
    new ShellCommand(Some(wd), command, Map("GIT_TERMINAL_PROMPT" -> "0", "GIT_ASKPASS" -> "/bin/false"))({_ =>})({_ => })
  }
}

/** prepends the path to git to the git commands */
object UnixGit extends Git {
  def toArgs(args: String*) = gitPath :: args.toList
}

/**
  * git behaves rather weirdly under windows, especially if authentication is needed
  *
  * this runs the [[UnixGit]] commands in the bash shell that comes with windows git
  *
  * @param sh the path to git's bash (may contain spaces), defaults to "sh"
  */
class WindowsGit(sh: String = "sh") extends Git {
  def toArgs(args: String*) = List(sh, "--login", "-c", UnixGit.toArgs(args:_*).mkString("\"", " ", "\""))
}
